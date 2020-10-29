{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeApplications #-}

module TypeRep where

import Id
import qualified Label as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Control.Monad.Fail as Fail
import Control.Monad
import qualified UnionFind as UF
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Except
import HList as HList
import Data.IORef

data KindRep = Type | RowLack L.Labels | RowPres L.Labels deriving (Eq, Ord)

instance Show KindRep where
  show Type = "Type"
  show (RowLack ls) = "~{" ++ show ls ++ "}"
  show (RowPres ls) = "{" ++ show ls ++ "}"

data TIdRep = TIdRep {name :: Id, kind :: KindRep} deriving (Eq, Ord)

tIdRep :: Id -> KindRep -> TIdRep
tIdRep name kind = TIdRep name kind

instance Show TIdRep where
  show (TIdRep {name = name, kind = kind}) = case kind of
    RowPres p | L.null p -> "*"
    Type -> show name
    _ -> show name ++ ": " ++ show kind

data TypeRepF t = 
  IdTypeRep TIdRep |
  FunTypeRep t t |
  RowRep (Map.Map L.Label t) t |
  VariantRep t |
  RecordRep t
  deriving (Eq, Ord, Functor, Foldable, Traversable)

kindInstCond :: KindRep -> KindRep -> Bool
kindInstCond Type Type = True
kindInstCond (RowLack rla) (RowLack rlb) = L.isSubsetOf rla rlb
kindInstCond (RowLack rl) (RowPres rp) = L.disjoint rl rp
kindInstCond (RowPres rp) (RowLack rl) = False
kindInstCond (RowPres rpa) (RowPres rpb) = L.isSubsetOf rpb rpa
kindInstCond _ _ = False

extendKind :: (Fail.MonadFail m) => Set.Set L.Label -> KindRep -> m KindRep
extendKind ls (RowLack rl) = let ls' = L.labels ls in
  if L.isSubsetOf ls' rl 
  then return (RowLack (L.difference rl ls')) 
  else Fail.fail "cannot extend"
extendKind ls (RowPres rp) = let ls' = L.labels ls in
  if L.disjoint ls' rp
  then return (RowPres (L.union ls' rp))
  else Fail.fail "cannot extend"
extendKind _ _ = Fail.fail "cannot extend"

mergeKind :: (Fail.MonadFail m) => KindRep -> KindRep -> m KindRep
mergeKind (RowLack rla) (RowLack rlb) = return (RowLack (L.union rla rlb))
mergeKind (RowLack rl) (RowPres rp) = return (RowPres (L.difference rp rl))
mergeKind (RowPres rp) (RowLack rl) = return (RowPres (L.difference rp rl))
mergeKind (RowPres rpa) (RowPres rpb) = return (RowPres (L.intersection rpa rpb))
mergeKind _ _ = Fail.fail "kind check fails"

extendRow :: (Fail.MonadFail m) => Map.Map L.Label t -> TypeRepF t -> m (TypeRepF t)
extendRow rls (RowRep ls r) = do
  ls' <- sequence $ 
    Map.unionWith (\_ _ -> Fail.fail "kind check fails") (Map.map return rls) (Map.map return ls)
  return $ RowRep ls' r
extendRow _ _ = Fail.fail "kind check fails"

type TypeRep = UF.Point TypeDesc

data TypeDesc = TypeDesc {
  structure :: TypeRepF TypeRep,
  isType :: Bool,
  visited :: Int
}

typeDesc :: TypeRepF TypeRep -> TypeDesc
typeDesc t = TypeDesc t False 0

typeRep :: (MonadIO m) => TypeRepF TypeRep -> m TypeRep
typeRep t = liftIO $ UF.fresh (typeDesc t)

typeRepFromDesc :: (MonadIO m) => TypeDesc -> m TypeRep
typeRepFromDesc d = liftIO $ UF.fresh d

genFreshIdType :: (MonadIO m, Occurs ("NameCounter" :- IORef Int) ts HList, 
  MonadReader (HList ts) m) => KindRep -> m TypeRep
genFreshIdType kind = do
  let pre = case kind of {Type -> "t"; _ -> "r"}
  freshId <- genFreshId pre
  typeRep (IdTypeRep (tIdRep freshId kind))

data VisitTypeRep a = NonRec (TypeRepF a) | RecHead Int (TypeRepF a) | RecBody Int

traverseTypeRep :: (MonadIO m, Fail.MonadFail m) => (TypeRep -> VisitTypeRep a -> a) -> TypeRep -> m a
traverseTypeRep f t = do
  counter <- liftIO $ newIORef 1
  flip runReaderT counter $ traverseTypeRepRun f t
  where
    traverseTypeRepRun :: (MonadIO m, Fail.MonadFail m, MonadReader (IORef Int) m) => 
      (TypeRep -> VisitTypeRep a -> a) -> TypeRep -> m a
    traverseTypeRepRun f t = do
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
              arg' <- traverseTypeRepRun f arg
              ret' <- traverseTypeRepRun f ret
              isRec <- liftIO $ visited <$> readIORef dt
              case compare isRec 0 of
                GT -> return (f rt (RecHead n (FunTypeRep arg' ret')))
                LT -> return (f rt (NonRec (FunTypeRep arg' ret')))
                EQ -> Fail.fail "impossible"
            RowRep ls r -> do
              ls' <- sequence $ Map.map (traverseTypeRepRun f) ls
              r' <- traverseTypeRepRun f r
              isRec <- liftIO $ visited <$> readIORef dt
              case compare isRec 0 of
                GT -> return (f rt (RecHead n (RowRep ls' r')))
                LT -> return (f rt (NonRec (RowRep ls' r')))
                EQ -> Fail.fail "impossible"
            VariantRep r -> do
              r' <- traverseTypeRepRun f r
              isRec <- liftIO $ visited <$> readIORef dt
              case compare isRec 0 of
                GT -> return (f rt (RecHead n (VariantRep r')))
                LT -> return (f rt (NonRec (VariantRep r')))
                EQ -> Fail.fail "impossible"
            RecordRep r -> do
              r' <- traverseTypeRepRun f r
              isRec <- liftIO $ visited <$> readIORef dt
              case compare isRec 0 of
                GT -> return (f rt (RecHead n (RecordRep r')))
                LT -> return (f rt (NonRec (RecordRep r')))
                EQ -> Fail.fail "impossible"
          liftIO $ writeIORef dt (dtv {visited = 0})
          return result

showTypeRep :: (Fail.MonadFail m, MonadIO m) => Bool -> TypeRep -> m String
showTypeRep isConcise t = do
  c <- liftIO $ newIORef 0
  toHide <- if not isConcise then return Set.empty else (Map.keysSet . Map.filter (== 1)) <$> idTypeOccur t
  let cxt :: HList '["NameCounter" :- IORef Int, "RecIds" :- Map.Map Int Id, "ToHide" :- Set.Set Id]
      cxt = Field c :| Field Map.empty :| Field toHide :| HNil
  join $ (flip evalStateT False . flip runReaderT cxt) <$> traverseTypeRep (const showTypeRep') t
  where
    showTypeRep' :: (Fail.MonadFail m, MonadState Bool m,
      Occurs ("NameCounter" :- IORef Int) ts HList,
      Update ("RecIds" :- Map.Map Int Id) ts HList,
      Occurs ("ToHide" :- Set.Set Id) ts HList,
      MonadReader (HList ts) m, MonadIO m) => VisitTypeRep (m String) -> m String
    showTypeRep' (RecBody n) = do
      recId <- (Map.lookup n . select @"RecIds" @(Map.Map Int Id)) <$> ask
      case recId of
        Just recId -> return (show recId)
        Nothing -> Fail.fail "impossible"
    showTypeRep' (NonRec (IdTypeRep tid@(TIdRep {name = name, kind = kind}))) = do
      isHidden <- (Set.member name . select @"ToHide" @(Set.Set Id)) <$> ask
      case kind of
        RowPres p | L.null p -> return (show tid)
        Type -> return (show tid)
        _ | isHidden -> return ""
        _ -> return (show tid)
    showTypeRep' (NonRec (FunTypeRep arg ret)) = do
      pos <- get
      put True
      arg <- arg
      put False
      ret <- ret
      if pos then return ("(" ++ arg ++ " -> " ++ ret ++ ")")
      else return (arg ++ " -> " ++ ret)
    showTypeRep' (NonRec (RowRep ls r)) = do
      ls <- sequence $ Map.mapWithKey (\l t -> (\t' -> show l ++ ": " ++ t' ++ " | ") <$> t) ls
      r <- r
      return (concat ls ++ r)
    showTypeRep' (NonRec (VariantRep r)) = do
      r <- r
      return ("<" ++ r ++ ">")
    showTypeRep' (NonRec (RecordRep r)) = do
      r <- r
      return ("{" ++ r ++ "}")
    showTypeRep' (RecHead n t) = do
      recId <- genFreshId "t"
      s <- local (HList.modify @"RecIds" @(Map.Map Int Id) (Map.insert n recId)) (showTypeRep' (NonRec t))
      return (show recId ++ " as (" ++ s ++ ")")

idTypeOccur :: (MonadIO m, Fail.MonadFail m) => TypeRep -> m (Map.Map Id Int)
idTypeOccur t = traverseTypeRep (const idTypeOccur') t
  where
    idTypeOccur' (RecBody _) = Map.empty
    idTypeOccur' (RecHead _ t) = idTypeOccur' (NonRec t)
    idTypeOccur' (NonRec (IdTypeRep (TIdRep {name = name}))) = Map.singleton name 1
    idTypeOccur' (NonRec (FunTypeRep arg ret)) = Map.unionWith (+) arg ret
    idTypeOccur' (NonRec (RowRep ls r)) = Map.unionWith (+) (Map.unionsWith (+) ls) r
    idTypeOccur' (NonRec (VariantRep r)) = r
    idTypeOccur' (NonRec (RecordRep r)) = r