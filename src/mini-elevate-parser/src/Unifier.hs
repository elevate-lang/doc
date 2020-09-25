{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

module Unifier where

import AST
import Data.IORef
import Control.Monad
import Control.Exception
import qualified UnionFind as UF
import Data.Comp
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Comp.Derive
import Data.Void
import qualified Control.Monad.Fail as Fail
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Foldable
import Data.List

data KindRep = Type | RowLack (Set.Set Label) | RowPres (Set.Set Label) deriving (Eq, Ord)

instance Show KindRep where
  show Type = "Type"
  show (RowLack ls) = "~{" ++ intercalate ", " (Set.toList ls) ++ "}"
  show (RowPres ls) = "{" ++ intercalate ", " (Set.toList ls) ++ "}"

data TIdRep = TIdRep {name :: Id, kind :: KindRep} deriving (Show, Eq, Ord)

newTId :: Id -> KindRep -> TIdRep
newTId name kind = TIdRep name kind

data TypeRepF t = 
  IdTypeRep TIdRep |
  FunTypeRep t t |
  RowRep (Map.Map Label t) t |
  VariantRep t |
  RecordRep t
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- $(derive [smartConstructors, smartAConstructors][''TypeRepF])

kindInstCond :: KindRep -> KindRep -> Bool
kindInstCond Type Type = True
kindInstCond (RowLack rla) (RowLack rlb) = Set.isSubsetOf rla rlb
kindInstCond (RowLack rl) (RowPres rp) = Set.disjoint rl rp
kindInstCond (RowPres rp) (RowLack rl) = False
kindInstCond (RowPres rpa) (RowPres rpb) = Set.isSubsetOf rpb rpa
kindInstCond _ _ = False

kindExtend :: (Fail.MonadFail m) => Set.Set Label -> KindRep -> m KindRep
kindExtend ls (RowLack rl) = 
  if Set.isSubsetOf ls rl 
  then return (RowLack (Set.difference rl ls)) 
  else Fail.fail "cannot extend"
kindExtend ls (RowPres rp) = 
  if Set.disjoint ls rp
  then return (RowPres (Set.union ls rp))
  else Fail.fail "cannot extend"
kindExtend _ _ = Fail.fail "cannot extend"

kindMerge :: (Fail.MonadFail m) => KindRep -> KindRep -> m KindRep
kindMerge (RowLack rla) (RowLack rlb) = return (RowLack (Set.union rla rlb))
kindMerge (RowLack rl) (RowPres rp) = return (RowPres (Set.difference rp rl))
kindMerge (RowPres rp) (RowLack rl) = return (RowPres (Set.difference rp rl))
kindMerge (RowPres rpa) (RowPres rpb) = return (RowPres (Set.intersection rpa rpb))
kindMerge _ _ = Fail.fail "kind check fails"

{-
kindCheck :: (Fail.MonadFail m) => Term TypeRepF -> m KindRep
kindCheck = cataM kindCheckAlg
  where
    kindCheckAlg (IdTypeRep (TIdRep _ k)) = return k
    kindCheckAlg (FunTypeRep arg ret) = case (arg, ret) of
      (Type, Type) -> return Type
      _ -> Fail.fail "kind check fails"
    kindCheckAlg (RowRep ls r) = case r of
      Type -> Fail.fail "kind check fails"
      _ -> kindExtend (Map.keysSet ls) r
    kindCheckAlg (VariantRep r) = case r of
      Type -> Fail.fail "kind check fails"
      _ -> return Type
    kindCheck (RecordRep r) = case r of
      Type -> Fail.fail "kind check fails"
      _ -> return Type
-}

type Node = UF.Point Desc

data Desc = Desc {
  structure :: TypeRepF Node,
  isType :: Bool,
  visited :: Int
}

newDesc :: TypeRepF Node -> Desc
newDesc t = Desc t False 0

freshNode :: (MonadIO m) => TypeRepF Node -> m Node
freshNode t = liftIO $ UF.fresh (newDesc t)

pushLabels :: (Fail.MonadFail m) => Map.Map Label t -> TypeRepF t -> m (TypeRepF t)
pushLabels rls (RowRep ls r) = do
  ls' <- sequence $ Map.unionWith (\_ _ -> Fail.fail "kind check fails") (Map.map return rls) (Map.map return ls)
  return $ RowRep ls' r
pushLabels _ _ = Fail.fail "kind check fails"

unify :: (MonadIO m, Fail.MonadFail m, MonadReader (IORef Int) m) => Node -> Node -> m KindRep
unify m n = do
  a <- liftIO $ UF.repr m
  b <- liftIO $ UF.repr n
  if (a == b) then do
    da <- liftIO $ UF.find a
    dav <- liftIO $ readIORef da
    case structure dav of
      FunTypeRep arga reta -> if isType dav then return Type else do
        liftIO $ writeIORef da (dav {isType = True})
        argk <- unify arga arga
        retk <- unify reta reta
        case (argk, retk) of
          (Type, Type) -> return Type
          _ -> Fail.fail "kind check fails"
      VariantRep ra -> if isType dav then return Type else do
        liftIO $ writeIORef da (dav {isType = True})
        rk <- unify ra ra
        case rk of
          Type -> Fail.fail "kind check fails"
          _ -> return Type
      RecordRep ra -> if isType dav then return Type else do
        liftIO $ writeIORef da (dav {isType = True})
        rk <- unify ra ra
        case rk of
          Type -> Fail.fail "kind check fails"
          _ -> return Type
      RowRep lsa ra -> do
        liftIO $ writeIORef da (dav {isType = False})
        lsa' <- mapM (\x -> unify x x) lsa
        rk <- unify ra ra
        if not (null (Map.filter (/= Type) lsa'))
        then Fail.fail "kind check fails"
        else case rk of
          Type -> Fail.fail "kind check fails"
          _ -> kindExtend (Map.keysSet lsa') rk
      IdTypeRep (TIdRep {kind = k}) -> do
        case k of
          Type -> liftIO $ writeIORef da (dav {isType = True})
          _ -> liftIO $ writeIORef da (dav {isType = False})
        return k
      _ -> Fail.fail "kind check fails"
  else do
    da <- liftIO $ UF.find a
    db <- liftIO $ UF.find b
    dav <- liftIO $ readIORef da
    dbv <- liftIO $ readIORef db
    case (structure dav, structure dbv) of
      (IdTypeRep (TIdRep {kind = ak}), IdTypeRep (TIdRep {kind = bk})) -> do
          if kindInstCond ak bk
          then liftIO $ UF.union a b >> return bk
          else 
            if kindInstCond bk ak 
            then liftIO $ UF.union b a >> return ak
            else do
              rvk <- kindMerge ak bk
              n <- ask
              freshName <- liftIO $ (("r" ++) . show) <$> readIORef n
              liftIO $ modifyIORef n (+ 1)
              rv <- liftIO $ UF.fresh (newDesc (IdTypeRep (newTId freshName rvk)))
              liftIO $ UF.union a rv
              liftIO $ UF.union b rv
              return rvk
      (IdTypeRep (TIdRep {kind = k}), _) -> do
        bk <- unify b b
        if kindInstCond k bk
        then liftIO $ UF.union a b >> return bk
        else do
          row <- liftIO $ UF.fresh (newDesc (RowRep Map.empty a))
          unify row b
      (_, IdTypeRep (TIdRep {kind = k})) -> do
        ak <- unify a a
        if kindInstCond k ak
        then liftIO $ UF.union b a >> return ak
        else do
          row <- liftIO $ UF.fresh (newDesc (RowRep Map.empty b))
          unify row a
      (FunTypeRep arga reta, FunTypeRep argb retb) -> do
        liftIO $ UF.union a b
        argk <- unify arga argb
        retk <- unify reta retb
        case (argk, retk) of
          (Type, Type) -> return Type
          _ -> Fail.fail "kind check fails"
      (VariantRep ra, VariantRep rb) -> do
        liftIO $ UF.union a b
        rk <- unify ra rb
        case rk of
          Type -> Fail.fail "kind check fails"
          _ -> return Type
      (RecordRep ra, RecordRep rb) -> do
        liftIO $ UF.union a b
        rk <- unify ra rb
        case rk of
          Type -> Fail.fail "kind check fails"
          _ -> return Type
      (RowRep lsa ra, RowRep lsb rb) -> do
        liftIO $ UF.union a b
        commonLabels <- sequence (Map.intersectionWith unify lsa lsb)
        if not (Map.null (Map.filter (/= Type) commonLabels))
        then Fail.fail "kind check fails"
        else do
          let resa = Map.difference lsa commonLabels
          let resb = Map.difference lsb commonLabels
          ra' <- liftIO $ UF.repr ra
          rb' <- liftIO $ UF.repr rb
          if (ra' == rb') then
            if not (Map.null resa && Map.null resb)
            then Fail.fail "cannot unify"
            else do
              rk <- unify ra' ra'
              case rk of
                Type -> Fail.fail "kind check fails"
                _ -> kindExtend (Map.keysSet commonLabels) rk
          else do
            (ra'', rb'') <- if Map.null resa && Map.null resb then return (ra', rb') else do
              dra <- liftIO $ UF.find ra'
              drb <- liftIO $ UF.find rb'
              drav <- liftIO $ readIORef dra
              drbv <- liftIO $ readIORef drb
              case (structure drav, structure drbv) of
                (RowRep _ _, RowRep _ _) -> do
                  newra <- pushLabels resa (structure drav)
                  newrb <- pushLabels resb (structure drbv)
                  ra' <- liftIO (UF.fresh (drav {structure = newra}))
                  rb' <- liftIO (UF.fresh (drbv {structure = newrb}))
                  return (ra', rb')
                (RowRep _ _, IdTypeRep _) -> do
                  newra <- pushLabels resa (structure drav)
                  ra' <- liftIO (UF.fresh (drav {structure = newra}))
                  rb' <- if Map.null resb then return rb' else liftIO $ UF.fresh (newDesc (RowRep resb rb'))
                  return (ra', rb')
                (IdTypeRep _, RowRep _ _) -> do
                  newrb <- pushLabels resb (structure drbv)
                  rb' <- liftIO (UF.fresh (drbv {structure = newrb}))
                  ra' <- if Map.null resa then return ra' else liftIO $ UF.fresh (newDesc (RowRep resa ra'))
                  return (ra', rb')
                (IdTypeRep (TIdRep {kind = ak}), IdTypeRep (TIdRep {kind = bk})) -> do
                    n <- ask
                    freshName <- liftIO $ (("r" ++) . show) <$> readIORef n
                    liftIO $ modifyIORef n (+ 1)
                    kindExtend (Map.keysSet resa) ak -- kind check
                    kindExtend (Map.keysSet resb) bk -- kind check
                    rvk <- kindMerge ak bk
                    rv <- liftIO $ UF.fresh (newDesc (IdTypeRep (newTId freshName rvk)))
                    ub <- if Map.null resa then return rv else liftIO $ UF.fresh (newDesc (RowRep resa rv))
                    ua <- if Map.null resb then return rv else liftIO $ UF.fresh (newDesc (RowRep resb rv))
                    ubk <- kindExtend (Map.keysSet resa) rvk
                    uak <- kindExtend (Map.keysSet resb) rvk
                    if kindInstCond ak uak then liftIO $ UF.union ra' ua else Fail.fail "kind check fails"
                    if kindInstCond bk ubk then liftIO $ UF.union rb' ub else Fail.fail "kind check fails"
                    -- unify ra' ua
                    -- unify rb' ub
                    ra' <- if Map.null resa then return ra' else liftIO $ UF.fresh (newDesc (RowRep resa ra'))
                    rb' <- if Map.null resb then return rb' else liftIO $ UF.fresh (newDesc (RowRep resb rb'))
                    return (ra', rb')   
            rk <- unify ra'' rb''
            if Map.null commonLabels
            then
              liftIO $ UF.change a =<< UF.find' ra''
            else
              liftIO $ UF.change a (newDesc (RowRep (Map.intersection lsa commonLabels) ra''))
            case rk of
              Type -> Fail.fail "kind check fails"
              _ -> kindExtend (Map.keysSet commonLabels) rk
      _ -> Fail.fail "cannot unify"

testUnify1 :: (MonadIO m, Fail.MonadFail m, MonadReader (IORef Int) m) => m KindRep
testUnify1 = do
  e <- liftIO $ UF.fresh (newDesc (IdTypeRep (newTId "*" (RowPres Set.empty))))
  r <- liftIO $ UF.fresh (newDesc (IdTypeRep (newTId "r" (RowPres (Set.fromList ["C", "D"])))))
  s <- liftIO $ UF.fresh (newDesc (IdTypeRep (newTId "s" (RowLack (Set.fromList ["A", "C"])))))
  a <- liftIO $ UF.fresh (newDesc (RecordRep e))
  b <- liftIO $ UF.fresh (newDesc (RecordRep e))
  c <- liftIO $ UF.fresh (newDesc (RecordRep e))
  r1 <- liftIO $ UF.fresh (newDesc (RowRep (Map.fromList [("A", a), ("B", b)]) r))
  r2 <- liftIO $ UF.fresh (newDesc (RowRep (Map.fromList [("A", a), ("C", c)]) s))
  -- r1 = (A: {*} | B: {*} | (r: {D}))
  -- r2 = (A: {*} | C: {*} | (s: ~ {A, C}))
  -- r1[r |-> (C: {*} | (r0: {D}))] = r2[s |-> (B: {*} | (r0: {D}))]
  -- (A: {*} | B: {*} | C: {*} | (r0: {D}))
  k <- unify r1 r2
  showType r1 >>= (liftIO . putStrLn)
  showType r >>= (liftIO . putStrLn)
  showType s >>= (liftIO . putStrLn)
  return k

testUnify2 :: (MonadIO m, Fail.MonadFail m, MonadReader (IORef Int) m) => m KindRep
testUnify2 = do
  e <- liftIO $ UF.fresh (newDesc (IdTypeRep (newTId "*" (RowPres Set.empty))))
  u <- liftIO $ UF.fresh (newDesc (RecordRep e))
  v <- liftIO $ UF.fresh (newDesc (VariantRep e))
  self <- liftIO $ UF.fresh (newDesc (IdTypeRep (newTId "self" Type)))
  f1 <- liftIO $ UF.fresh (newDesc (FunTypeRep u self))
  unify self f1
  self <- liftIO $ UF.fresh (newDesc (IdTypeRep (newTId "self" Type)))
  f2' <- liftIO $ UF.fresh (newDesc (FunTypeRep u self))
  f2 <- liftIO $ UF.fresh (newDesc (FunTypeRep u f2'))
  unify self f2
  -- f1 = a as {*} -> a
  -- f2 = a as {*} -> {*} -> a
  unify f1 f2
  self <- liftIO $ UF.fresh (newDesc (IdTypeRep (newTId "self" Type)))
  f3'' <- liftIO $ UF.fresh (newDesc (FunTypeRep u self))
  f3' <- liftIO $ UF.fresh (newDesc (FunTypeRep u f3''))
  f3 <- liftIO $ UF.fresh (newDesc (FunTypeRep u f3'))
  unify self f3
  k <- unify f3 f2
  -- f3 = a as {*} -> {*} -> {*} -> a
  -- f2 = a as {*} -> {*} -> a
  showType f3 >>= (liftIO . putStrLn)
  return k

testUnify3 :: (MonadIO m, Fail.MonadFail m, MonadReader (IORef Int) m) => m ()
testUnify3 = do
  e <- freshNode (IdTypeRep (newTId "*" (RowPres Set.empty)))
  u <- freshNode (RecordRep e)
  self <- freshNode (IdTypeRep (newTId "self" Type))
  natRow <- freshNode (RowRep (Map.fromList [("Z", u), ("S", self)]) e)
  nat <- freshNode (VariantRep natRow)
  unify self nat
  showType nat >>= (liftIO . putStrLn)
  self <- freshNode (IdTypeRep (newTId "self" Type))
  lamRow <- freshNode (RowRep (Map.fromList [("Param", nat), ("Body", self)]) e)
  lam <- freshNode (RecordRep lamRow)
  appRow <- freshNode (RowRep (Map.fromList [("Fun", self), ("Arg", self)]) e)
  app <- freshNode (RecordRep appRow)
  exprRow <- freshNode (RowRep (Map.fromList [("Var", nat), ("Lam", lam), ("App", app)]) e)
  expr <- freshNode (VariantRep exprRow)
  unify self expr
  showType expr >>= (liftIO . putStrLn)
  self0 <- freshNode (IdTypeRep (newTId "self" Type))
  self1 <- freshNode (IdTypeRep (newTId "self" Type))
  extRow <- freshNode (RowRep (Map.fromList [("Type", self0), ("Row", self1)]) e)
  ext <- freshNode (RecordRep extRow)
  rowRow <- freshNode (RowRep (Map.fromList [("EmptyRow", u), ("IdRow", nat), ("ExtRow", ext)]) e)
  row <- freshNode (VariantRep rowRow)
  funRow <- freshNode (RowRep (Map.fromList [("Arg", self0), ("Ret", self0)]) e)
  fun <- freshNode (RecordRep funRow)
  typeRow <- freshNode (RowRep (Map.fromList [("IdType", nat), ("FunType", fun), 
    ("Variant", row), ("Record", row)]) e)
  typeType <- freshNode (VariantRep typeRow)
  unify self0 typeType
  unify self1 row
  showType typeType >>= (liftIO . putStrLn)
  showType row >>= (liftIO . putStrLn)

runTest t = do
  c <- newIORef 0
  runExceptT (runReaderT t c)

data VisitTypeRep a = NonRec (TypeRepF a) | RecHead Int (TypeRepF a) | RecBody Int

traverseNode :: (MonadIO m, Fail.MonadFail m) => (Node -> VisitTypeRep a -> a) -> Node -> m a
traverseNode f t = do
  counter <- liftIO $ newIORef 1
  flip runReaderT counter $ traverseNodeRun f t
  where
    traverseNodeRun :: (MonadIO m, Fail.MonadFail m, MonadReader (IORef Int) m) => 
      (Node -> VisitTypeRep a -> a) -> Node -> m a
    traverseNodeRun f t = do
      rt <- liftIO $ UF.repr t
      dt <- liftIO $ UF.find rt
      dtv <- liftIO $ readIORef dt
      let dts = structure dtv
          isRec = visited dtv
      case compare isRec 0 of
        GT -> return (f rt (RecBody isRec))
        LT -> do
          liftIO $ writeIORef dt (dtv {visited = negate isRec})
          return (f rt (RecBody (negate isRec)))
        EQ -> do
          counter <- ask
          n <- liftIO $ readIORef counter
          liftIO $ writeIORef dt (dtv {visited = negate n})
          liftIO $ modifyIORef counter (+ 1)
          result <- case dts of
            IdTypeRep i -> return (f rt (NonRec (IdTypeRep i)))
            FunTypeRep arg ret -> do
              arg' <- traverseNodeRun f arg
              ret' <- traverseNodeRun f ret
              isRec <- liftIO $ visited <$> readIORef dt
              case compare isRec 0 of
                GT -> return (f rt (RecHead n (FunTypeRep arg' ret')))
                LT -> return (f rt (NonRec (FunTypeRep arg' ret')))
                EQ -> Fail.fail "impossible"
            RowRep ls r -> do
              ls' <- sequence $ Map.map (traverseNodeRun f) ls
              r' <- traverseNodeRun f r
              isRec <- liftIO $ visited <$> readIORef dt
              case compare isRec 0 of
                GT -> return (f rt (RecHead n (RowRep ls' r')))
                LT -> return (f rt (NonRec (RowRep ls' r')))
                EQ -> Fail.fail "impossible"
            VariantRep r -> do
              r' <- traverseNodeRun f r
              isRec <- liftIO $ visited <$> readIORef dt
              case compare isRec 0 of
                GT -> return (f rt (RecHead n (VariantRep r')))
                LT -> return (f rt (NonRec (VariantRep r')))
                EQ -> Fail.fail "impossible"
            RecordRep r -> do
              r' <- traverseNodeRun f r
              isRec <- liftIO $ visited <$> readIORef dt
              case compare isRec 0 of
                GT -> return (f rt (RecHead n (RecordRep r')))
                LT -> return (f rt (NonRec (RecordRep r')))
                EQ -> Fail.fail "impossible"
          liftIO $ writeIORef dt (dtv {visited = 0})
          return result

restrictRec :: (Fail.MonadFail m) => Node -> VisitTypeRep (m Node) -> m Node
restrictRec t (RecBody _) = return t
restrictRec t (NonRec _) = return t
restrictRec t (RecHead _ (VariantRep _)) = return t
restrictRec t (RecHead _ (RowRep _ _)) = return t
restrictRec t _ = Fail.fail "non-variant recursive type"

showType :: (Fail.MonadFail m, MonadIO m) => Node -> m String
showType t = do
  c <- liftIO $ newIORef 0
  join $ (flip evalStateT False . flip runReaderT (c, Map.empty)) <$> traverseNode (const showType') t
  where
    showType' :: (Fail.MonadFail m, MonadState Bool m, 
      MonadReader (IORef Int, Map.Map Int String) m, MonadIO m) => 
      VisitTypeRep (m String) -> m String
    showType' (RecBody n) = do
      name <- (Map.lookup n . snd) <$> ask
      case name of
        Just name -> return name
        Nothing -> Fail.fail "impossible"
    showType' (NonRec (IdTypeRep (TIdRep {name = name, kind = kind}))) = {-return name-}
      case kind of
        RowPres p | Set.null p -> return "*"
        Type -> return name
        _ -> return (name ++ ": " ++ show kind)
    showType' (NonRec (FunTypeRep arg ret)) = do
      pos <- get
      put True
      arg <- arg
      put False
      ret <- ret
      if pos then return ("(" ++ arg ++ " -> " ++ ret ++ ")")
      else return (arg ++ " -> " ++ ret)
    showType' (NonRec (RowRep ls r)) = do
      ls <- sequence $ Map.mapWithKey (\l t -> (\t' -> l ++ ": " ++ t' ++ " | ") <$> t) ls
      r <- r
      return (concat ls ++ r)
    showType' (NonRec (VariantRep r)) = do
      r <- r
      return ("<" ++ r ++ ">")
    showType' (NonRec (RecordRep r)) = do
      r <- r
      return ("{" ++ r ++ "}")
    showType' (RecHead n t) = do
      (freshCounter, names) <- ask
      freshName <- liftIO $ (("t" ++) . show) <$> readIORef freshCounter
      liftIO $ modifyIORef freshCounter (+ 1)
      s <- local (const (freshCounter, Map.insert n freshName names)) (showType' (NonRec t))
      return (freshName ++ " as (" ++ s ++ ")")
