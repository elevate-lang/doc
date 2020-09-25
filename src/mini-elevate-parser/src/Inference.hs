{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, GADTs, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances#-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, InstanceSigs, TypeApplications, RankNTypes #-}

module Inference where

import AST
import Data.IORef
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Control.Monad.Fail as Fail
import Data.Functor.Compose
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.RawString.QQ
import Data.Comp.Multi
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Derive
import Data.Comp.Multi.Annotation
import qualified Data.Comp.Ops as O
import Unifier
import Data.Foldable
import qualified UnionFind as UF
import Control.Arrow
import Control.Monad.Trans.Except

class Infer f g m where
  inferAlg :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''Infer])

data SchemeRep = Forall (Set.Set TIdRep) Node

newtype TypeEnv = TypeEnv (Map.Map Id SchemeRep)

class (Monad m) => HasTVar m a where
  ftv :: a -> m (Set.Set TIdRep)

instance (MonadIO m, Fail.MonadFail m) => HasTVar m Node where
  ftv t = traverseNode (const ftv') t
    where
      ftv' (RecBody _) = Set.empty
      ftv' (RecHead _ t) = ftv' (NonRec t)
      ftv' (NonRec (IdTypeRep (TIdRep {name = name, kind = k}))) = Set.singleton (newTId name k)
      ftv' (NonRec (FunTypeRep arg ret)) = Set.union arg ret
      ftv' (NonRec (RowRep ls r)) = Set.union (fold ls) r
      ftv' (NonRec (VariantRep r)) = r
      ftv' (NonRec (RecordRep r)) = r

instance (MonadIO m, Fail.MonadFail m) => HasTVar m SchemeRep where
  ftv (Forall tv t) = Set.difference <$> ftv t <*> return tv

instance (MonadIO m, Fail.MonadFail m) => HasTVar m TypeEnv where
  ftv (TypeEnv env) = fold <$> sequence (Map.map ftv env)

instantiate :: (MonadIO m, Fail.MonadFail m, MonadReader (IORef Int) m) => SchemeRep -> m Node
instantiate (Forall tv t) = do
  n <- ask
  join $ (flip evalStateT Map.empty . flip runReaderT (n, Map.empty)) <$> traverseNode (inst tv) t
  where
    inst :: (Fail.MonadFail m, MonadReader (IORef Int, Map.Map Int Node) m, 
      MonadState (Map.Map TIdRep Node) m, MonadIO m) => 
      Set.Set TIdRep -> Node -> VisitTypeRep (m Node) -> m Node
    inst _ _ (RecBody n) = do
      node <- (Map.lookup n . snd) <$> ask
      case node of
        Just node -> return node
        Nothing -> Fail.fail "impossible"
    inst tv node (NonRec (IdTypeRep i@(TIdRep {kind = k}))) = case Set.member i tv of
      True -> do
        instantiated <- get
        case Map.lookup i instantiated of
          Just node -> return node
          Nothing -> do
            (n, _) <- ask
            node <- genFreshIdType n k
            modify (Map.insert i node)
            return node
      False -> return node
    inst _ _ (NonRec (FunTypeRep arg ret)) = do
      dt <- FunTypeRep <$> arg <*> ret
      freshNode dt
    inst _ _ (NonRec (RowRep ls r)) = do
      dt <- RowRep <$> sequence ls <*> r
      freshNode dt
    inst _ _ (NonRec (VariantRep r)) = do
      dt <- VariantRep <$> r
      freshNode dt
    inst _ _ (NonRec (RecordRep r)) = do
      dt <- RecordRep <$> r
      freshNode dt
    inst tv node (RecHead n t) = do
      ph <- freshNode (IdTypeRep (newTId "#rec#" Type))
      result <- local (second (Map.insert n ph)) (inst tv node (NonRec t))
      liftIO $ UF.union ph result
      return result

generalize :: (MonadReader TypeEnv m, MonadIO m, Fail.MonadFail m) => Node -> m SchemeRep
generalize t = do
  env <- ask
  tv <- Set.difference <$> ftv t <*> ftv env
  return (Forall tv t)

type InferPresSig = Pres

type InferTypeSig = Type :+: Row :+: UnknownType :+: RecVariantType

type InferPatSig = Pat :+: AppPat

type InferExprSig = 
  Expr :+: FunDef InferPresSig InferTypeSig :+: RecDef InferPresSig InferTypeSig :+: 
  RecordOps :+: LabelExpr LabelAsFun :+: Match InferPatSig SimplePat

type TempExprSig =
  Expr :+: FunDef InferPresSig InferTypeSig :+: RecDef InferPresSig InferTypeSig :+: 
  LabelExpr LabelAsFun

type Constraint = (Node, Node)

genFreshName :: (MonadIO m) => IORef Int -> Id -> m String
genFreshName counter pre = do
  freshName <- liftIO $ ((pre ++) . show) <$> readIORef counter
  liftIO $ modifyIORef counter (+ 1)
  return freshName

genFreshIdType :: (MonadIO m) => IORef Int -> KindRep -> m Node
genFreshIdType counter kind = do
  let pre = case kind of {Type -> "t"; _ -> "r"}
  freshName <- genFreshName counter pre
  freshNode (IdTypeRep (newTId freshName kind))

runSolver :: (MonadIO m, Fail.MonadFail m, MonadReader (IORef Int) m) => [Constraint] -> m ()
runSolver = mapM_ (uncurry unify)

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, MonadReader (IORef Int, TypeEnv) m, Expr :<: g, DistAnn g Node h) => Infer Expr h m where
  inferAlg (IdExpr x) = Compose $ do
    (n, TypeEnv env) <- ask
    case Map.lookup x env of
      Nothing -> Fail.fail "variable not found"
      Just scheme -> do
        t <- flip runReaderT n (instantiate scheme)
        return $ iAIdExpr t x
  inferAlg (AppExpr fun arg) = Compose $ do
    fun' <- getCompose fun
    arg' <- getCompose arg
    let (_ O.:&: funType) = projectA (unTerm fun')
        (_ O.:&: argType) = projectA (unTerm arg')
    (n, _) <- ask
    retType <- genFreshIdType n Type
    funTypeUni <- freshNode (FunTypeRep argType retType)
    tell [(funType, funTypeUni)]
    return $ iAAppExpr retType fun' arg'
  inferAlg (LamExpr params body) = Compose $ do
    (n, TypeEnv env) <- ask
    paramTypes <- mapM (const $ genFreshIdType n Type) params
    let newEnv = TypeEnv (Map.union (Map.fromList (zip params (map (Forall Set.empty) paramTypes))) env)
    body' <- local (const (n, newEnv)) (getCompose body)
    let (_ O.:&: bodyType) = projectA (unTerm body')
    lamType <- foldrM ((freshNode .) . FunTypeRep) bodyType paramTypes
    return $ iALamExpr lamType params body'

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, MonadReader (IORef Int, TypeEnv) m, FunDef InferPresSig InferTypeSig :<: g, DistAnn g Node h) => Infer (FunDef InferPresSig InferTypeSig) h m where
  inferAlg (FunDef name c ft fr params rt body e) = Compose $ do
    (n, TypeEnv env) <- ask
    paramTypes <- mapM (const $ genFreshIdType n Type) params
    let newEnv = TypeEnv (Map.union (Map.fromList (zip (map fst params) (map (Forall Set.empty) paramTypes))) env)
    (body', cs) <- listen (local (const (n, newEnv)) (getCompose body))
    let (_ O.:&: bodyType) = projectA (unTerm body')
    funType <- foldrM ((freshNode .) . FunTypeRep) bodyType paramTypes
    -- liftIO $ putStrLn =<< showType funType
    liftIO $ print =<< mapM (\c -> (,) <$> showType (fst c) <*> showType (snd c)) cs
    flip runReaderT n (runSolver cs)
    liftIO $ putStrLn =<< showType funType
    funScheme <- flip runReaderT (TypeEnv env) (generalize funType)
    -- case funScheme of Forall tv _ -> liftIO $ print tv
    let newEnv = TypeEnv (Map.insert name funScheme env)
    e' <- local (const (n, newEnv)) (getCompose e)
    let (_ O.:&: eType) = projectA (unTerm e')
    return $ iAFunDef eType name c ft fr params rt body' e'

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, MonadReader (IORef Int, TypeEnv) m, RecDef InferPresSig InferTypeSig :<: g, DistAnn g Node h) => Infer (RecDef InferPresSig InferTypeSig) h m where
  inferAlg (RecDef name c ft fr params rt body e) = Compose $ do
    (n, TypeEnv env) <- ask
    paramTypes <- mapM (const $ genFreshIdType n Type) params
    self <- genFreshIdType n Type
    let newEnv = TypeEnv (Map.insert name (Forall Set.empty self) (Map.union (Map.fromList (zip (map fst params) (map (Forall Set.empty) paramTypes))) env))
    (body', cs) <- listen (local (const (n, newEnv)) (getCompose body))
    let (_ O.:&: bodyType) = projectA (unTerm body')
    funType <- foldrM ((freshNode .) . FunTypeRep) bodyType paramTypes
    -- liftIO $ putStrLn =<< showType funType
    -- liftIO $ print =<< mapM (\c -> (,) <$> showType (fst c) <*> showType (snd c)) cs
    flip runReaderT n (runSolver ((self, funType) : cs))
    -- liftIO $ putStrLn =<< showType funType
    funScheme <- flip runReaderT (TypeEnv env) (generalize funType)
    -- case funScheme of Forall tv _ -> liftIO $ print tv
    let newEnv = TypeEnv (Map.insert name funScheme env)
    e' <- local (const (n, newEnv)) (getCompose e)
    let (_ O.:&: eType) = projectA (unTerm e')
    return $ iARecDef eType name c ft fr params rt body' e'

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, MonadReader (IORef Int, TypeEnv) m, LabelExpr LabelAsFun :<: g, DistAnn g Node h) => Infer (LabelExpr LabelAsFun) h m where
  inferAlg (LabelApp l e) = Compose $ do
    e' <- getCompose e
    let (_ O.:&: eType) = projectA (unTerm e')
    (n, _) <- ask
    rv <- genFreshIdType n (RowLack (Set.singleton l))
    row <- freshNode (RowRep (Map.singleton l eType) rv)
    t <- freshNode (VariantRep row)
    return $ iALabelApp t l e'

class PatInfer f m where
  patInferAlg :: Alg f (K (m (Node, Maybe Node, Map.Map Id SchemeRep)))

$(derive [liftSum] [''PatInfer])

instance (MonadIO m, Fail.MonadFail m, MonadReader (IORef Int, TypeEnv) m) => PatInfer Pat m where
  patInferAlg (IdPat i) = K $ do
    (n, _) <- ask
    t <- genFreshIdType n Type
    return (t, Nothing, Map.singleton i (Forall Set.empty t))
  patInferAlg (LabelPat l) = K $ do
    (n, _) <- ask
    emptyRow <- freshNode (IdTypeRep (newTId "*" (RowPres Set.empty)))
    unitType <- freshNode (RecordRep emptyRow)
    rv <- genFreshIdType n (RowLack (Set.singleton l))
    row <- freshNode (RowRep (Map.singleton l unitType) rv)
    t <- freshNode (VariantRep row)
    rest <- freshNode (VariantRep rv)
    return (t, Just rest, Map.empty)

instance (MonadIO m, Fail.MonadFail m, MonadReader (IORef Int, TypeEnv) m) => PatInfer AppPat m where
  patInferAlg (AppIdPat l i) = K $ do
    (n, _) <- ask
    idType <- genFreshIdType n Type
    rv <- genFreshIdType n (RowLack (Set.singleton l))
    row <- freshNode (RowRep (Map.singleton l idType) rv)
    t <- freshNode (VariantRep row)
    rest <- freshNode (VariantRep rv)
    return (t, Just rest, Map.singleton i (Forall Set.empty idType))
  patInferAlg _ = K $ Fail.fail "not a simple pattern"

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, MonadReader (IORef Int, TypeEnv) m, RecordOps :<: g, DistAnn g Node h) => Infer RecordOps h m where
  inferAlg (RecordCons fields) = Compose $ do
    let process (l, e) = do
          e' <- getCompose e
          let (_ O.:&: eType) = projectA (unTerm e')
          return ((l, e'), (l, eType))
    fieldsTypes <- mapM process fields
    emptyRow <- freshNode (IdTypeRep (newTId "*" (RowPres Set.empty)))
    row <- freshNode (RowRep (Map.fromList (map snd fieldsTypes)) emptyRow)
    t <- freshNode (RecordRep row)
    let fields' = map fst fieldsTypes
    return $ iARecordCons t fields'
  inferAlg _ = Compose $ Fail.fail "TODO"

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, MonadReader (IORef Int, TypeEnv) m, Match InferPatSig SimplePat :<: g, DistAnn g Node h) => Infer (Match InferPatSig SimplePat) h m where
  inferAlg (Match e cases) = Compose $ do
    e' <- getCompose e
    (n, TypeEnv env) <- ask
    rhsIdType <- genFreshIdType n Type 
    let (_ O.:&: eType) = projectA (unTerm e')
    let process (Nothing, _) _ = Fail.fail "redundant case"
        process (Just t, cases') (p, rhs) = do
          (patType, rest, intro) <- unK (cata patInferAlg p)
          tell [(t, patType)]
          let newEnv = TypeEnv (Map.union intro env)
          rhs' <- local (second (const newEnv)) (getCompose rhs)
          let (_ O.:&: rhsType) = projectA (unTerm rhs')
          tell [(rhsType, rhsIdType)]
          return (rest, (p, rhs') : cases')
    (rest, cases') <- second reverse <$> foldM process (Just eType, []) cases
    emptyRow <- freshNode (IdTypeRep (newTId "*" (RowPres Set.empty)))
    emptyType <- freshNode (VariantRep emptyRow)
    case rest of
      Just t -> tell [(t, emptyType)]
      Nothing -> return ()
    return $ iAMatch rhsIdType e' cases'

type TestSig = RecDef InferPresSig InferTypeSig :+: FunDef InferPresSig InferTypeSig :+: Expr :+: Match InferPatSig SimplePat :+: LabelExpr LabelAsFun :+: RecordOps

testExpr1 :: Fix TestSig EXPR
testExpr1 = iFunDef "test" False [] ([] :: [(Id, Fix InferPresSig PRES)]) [] (iUnknownType :: Fix InferTypeSig TypeKind) (iLamExpr ["x"] (iAppExpr (iIdExpr "x") (iIdExpr "x"))) (iIdExpr "test")

testExpr2 :: Fix TestSig EXPR
testExpr2 = iRecDef "fix" False [] ([] :: [(Id, Fix InferPresSig PRES)]) [] (iUnknownType :: Fix InferTypeSig TypeKind) (iLamExpr ["f"] (iAppExpr (iIdExpr "f") (iAppExpr (iIdExpr "fix") (iIdExpr "f")))) (iIdExpr "fix")

testExpr3 :: Fix TestSig EXPR
testExpr3 = iFunDef "test" False [] ([] :: [(Id, Fix InferPresSig PRES)]) [] (iUnknownType :: Fix InferTypeSig TypeKind) (iLamExpr ["x"] (iMatch (iIdExpr "x") [(iLabelPat "A" :: Fix InferPatSig SimplePat, iLabelApp "B" (iRecordCons [])), (iIdPat "x" :: Fix InferPatSig SimplePat, iIdExpr "x")])) (iIdExpr "test")

testExpr4 :: Fix TestSig EXPR
testExpr4 = 
  iFunDef "flatMap" False [] ([] :: [(Id, Fix InferPresSig PRES)]) [] (iUnknownType :: Fix InferTypeSig TypeKind) (iLamExpr ["x", "f"] (iMatch (iIdExpr "x") [(iAppIdPat "Success" "a" :: Fix InferPatSig SimplePat, (iAppExpr (iIdExpr "f") (iIdExpr "a"))), (iAppIdPat "Failure" "b" :: Fix InferPatSig SimplePat, iLabelApp "Failure" (iIdExpr "b"))])) $

  iFunDef "alter" False [] ([] :: [(Id, Fix InferPresSig PRES)]) [] (iUnknownType :: Fix InferTypeSig TypeKind) (iLamExpr ["x", "y"] (iMatch (iIdExpr "x") [(iAppIdPat "Success" "a" :: Fix InferPatSig SimplePat, iLabelApp "Success" (iIdExpr "a")), (iAppIdPat "Failure" "b" :: Fix InferPatSig SimplePat, iIdExpr "y")])) $

  iFunDef "seq" False [] ([] :: [(Id, Fix InferPresSig PRES)]) [] (iUnknownType :: Fix InferTypeSig TypeKind) (iLamExpr ["fs", "ss", "x"] (iAppExpr (iAppExpr (iIdExpr "flatMap") (iAppExpr (iIdExpr "fs") (iIdExpr "x"))) (iIdExpr "ss"))) $

  iFunDef "lChoice" False [] ([] :: [(Id, Fix InferPresSig PRES)]) [] (iUnknownType :: Fix InferTypeSig TypeKind) (iLamExpr ["fs", "ss", "x"] (iAppExpr (iAppExpr (iIdExpr "alter") (iAppExpr (iIdExpr "fs") (iIdExpr "x"))) (iAppExpr (iIdExpr "ss") (iIdExpr "x")))) $
  
  iFunDef "id" False [] ([] :: [(Id, Fix InferPresSig PRES)]) [] (iUnknownType :: Fix InferTypeSig TypeKind) (iLamExpr ["x"] (iLabelApp "Success" (iIdExpr "x"))) $
  
  iFunDef "try" False [] ([] :: [(Id, Fix InferPresSig PRES)]) [] (iUnknownType :: Fix InferTypeSig TypeKind) (iLamExpr ["s"] (iAppExpr (iAppExpr (iIdExpr "lChoice") (iIdExpr "s")) (iIdExpr "id"))) $
  
  iIdExpr "try"

-- [] :: forall a. [a]
-- length :: forall a. [a] -> Int
-- (length :: [Any] -> Int) ([] :: [Any]) :: Int

testInfer :: Fix TestSig EXPR -> IO ()
testInfer prog = do
  c <- newIORef 0
  r <- runExceptT (fmap fst $ runWriterT (flip runReaderT (c, TypeEnv Map.empty) (getCompose (cata inferAlg prog))))
  case r of
    Right (r :: Fix ((RecDef InferPresSig InferTypeSig :&: Node) :+: (FunDef InferPresSig InferTypeSig :&: Node) :+: (Expr :&: Node) :+: (Match InferPatSig SimplePat :&: Node) :+: (LabelExpr LabelAsFun :&: Node) :+: (RecordOps :&: Node)) EXPR) -> do
      let (_ O.:&: finalType) = projectA (unTerm r)
      putStrLn =<< showType finalType
    Left m -> undefined

{-
type InferPresSig = Pres

type InferTypeSig = Type :+: Row :+: UnknownType :+: RecVariantType

type InferPatSig = Pat :+: AppPat

type InferExprSig = Expr :+: FunDef InferPresSig InferTypeSig :+: RecDef InferPresSig InferTypeSig :+: RecordOps :+: LabelExpr LabelAsFun :+: Match InferPatSig SimplePat

data KindRep = Type | RowLack [Label] | RowPres [Label]

data TIdRep = TIdRep Id KindRep

data TypeRep = 
  IdTypeRep TIdRep |
  FunTypeRep TypeRep TypeRep |
  RowRep (Map.Map Label TypeRep) TIdRep |
  VariantRep TypeRep |
  RecordRep TypeRep |
  RecVariantRep Id TypeRep

data SchemeRep = Forall [TIdRep] TypeRep

newtype TypeEnv = TypeEnv (Map.Map Id SchemeRep)

data MultiEq = MultiEq [TIdRep] [TypeRep] (Maybe Int)

newtype Subst = Subst (Map.Map Id TypeRep)

data InferState = InferState {counter :: Int}

getTId :: TIdRep -> Id
getTId (TIdRep i k) = i

freshTId :: (MonadState InferState m) => TIdRep -> m TIdRep
freshTId (TIdRep i k) = do
  s <- get
  put s{counter = counter s + 1}
  return $ case k of
    Type -> TIdRep ("t" ++ show (counter s)) k
    _ -> TIdRep ("r" ++ show (counter s)) k

instScheme :: (MonadState InferState m) => SchemeRep -> m TypeRep
instScheme (Forall ids t) = do
  ids' <- mapM freshTId ids
  let s = Subst $ Map.fromList (zip (map getTId ids) (map IdTypeRep ids'))
  return undefined

lookupId :: (MonadReader TypeEnv m, Fail.MonadFail m, MonadState InferState m) => Id -> m TypeRep
lookupId i = do
  TypeEnv env <- ask
  case Map.lookup i env of
    Nothing -> Fail.fail ("Cannot find " ++ i ++ " in the environment")
    Just s -> instScheme s

class Infer f g m where
  infer :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''Infer])

instance (
  MonadReader TypeEnv m, 
  MonadWriter [MultiEq] m,
  Fail.MonadFail m,
  MonadState InferState m,
  (Expr :&: TypeRep) :<: g) => 
  Infer Expr g m where
    infer (IdExpr name) = undefined
    infer (AppExpr e1 e2) = undefined
    infer (LamExpr params e) = undefined

-}