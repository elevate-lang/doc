{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, GADTs, TypeFamilies, TypeApplications, DataKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances#-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, InstanceSigs, TypeApplications, RankNTypes #-}

module Infer where

import AST
import qualified Label as L
import Id
import Data.IORef
import Control.Monad.State as State
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
import Data.Foldable
import qualified UnionFind as UF
import Control.Arrow
import Control.Monad.Trans.Except
import TypeRep
import Unifier
import HList as HList

class Infer f g m where
  inferAlg :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''Infer])

data SchemeRep = Forall (Set.Set TIdRep) TypeRep

newtype TypeEnv = TypeEnv (Map.Map Id SchemeRep)

insertTypeEnv :: Id -> SchemeRep -> TypeEnv -> TypeEnv
insertTypeEnv i s (TypeEnv env) = TypeEnv (Map.insert i s env)

unionTypeEnv :: Map.Map Id SchemeRep -> TypeEnv -> TypeEnv
unionTypeEnv m (TypeEnv env) = TypeEnv (Map.union m env)

extendTypeEnv = unionTypeEnv

class (Monad m) => HasTVar m a where
  ftv :: a -> m (Set.Set TIdRep)

instance (MonadIO m, Fail.MonadFail m) => HasTVar m TypeRep where
  ftv t = traverseTypeRep (const ftv') t
    where
      ftv' (RecBody _) = Set.empty
      ftv' (RecHead _ t) = ftv' (NonRec t)
      ftv' (NonRec (IdTypeRep (TIdRep {name = name, kind = k}))) = Set.singleton (tIdRep name k)
      ftv' (NonRec (FunTypeRep arg ret)) = Set.union arg ret
      ftv' (NonRec (RowRep ls r)) = Set.union (fold ls) r
      ftv' (NonRec (VariantRep r)) = r
      ftv' (NonRec (RecordRep r)) = r

instance (MonadIO m, Fail.MonadFail m) => HasTVar m SchemeRep where
  ftv (Forall tv t) = Set.difference <$> ftv t <*> return tv

instance (MonadIO m, Fail.MonadFail m) => HasTVar m TypeEnv where
  ftv (TypeEnv env) = fold <$> sequence (Map.map ftv env)

instantiate :: (MonadIO m, Fail.MonadFail m, 
  Occurs ("NameCounter" :- IORef Int) ts HList, MonadReader (HList ts) m) => 
  SchemeRep -> m TypeRep
instantiate (Forall tv t) = do
  n <- select @"NameCounter" @(IORef Int) <$> ask
  let cxt :: HList '["NameCounter" :- IORef Int, "RecTypes" :- Map.Map Int TypeRep]
      cxt = Field n :| Field Map.empty :| HNil
  join $ (flip evalStateT Map.empty . flip runReaderT cxt) <$> traverseTypeRep (inst tv) t
  where
    inst :: (Fail.MonadFail m,
      Occurs ("NameCounter" :- IORef Int) ts HList, 
      Update ("RecTypes" :- Map.Map Int TypeRep) ts HList,
      MonadReader (HList ts) m,
      MonadState (Map.Map TIdRep TypeRep) m, MonadIO m) => 
      Set.Set TIdRep -> TypeRep -> VisitTypeRep (m TypeRep) -> m TypeRep
    inst _ _ (RecBody n) = do
      node <- (Map.lookup n . select @"RecTypes" @(Map.Map Int TypeRep)) <$> ask
      case node of
        Just node -> return node
        Nothing -> Fail.fail "impossible"
    inst tv node (NonRec (IdTypeRep i@(TIdRep {kind = k}))) = case Set.member i tv of
      True -> do
        instantiated <- get
        case Map.lookup i instantiated of
          Just node -> return node
          Nothing -> do
            node <- genFreshIdType k
            State.modify (Map.insert i node)
            return node
      False -> return node
    inst _ _ (NonRec (FunTypeRep arg ret)) = do
      dt <- FunTypeRep <$> arg <*> ret
      typeRep dt
    inst _ _ (NonRec (RowRep ls r)) = do
      dt <- RowRep <$> sequence ls <*> r
      typeRep dt
    inst _ _ (NonRec (VariantRep r)) = do
      dt <- VariantRep <$> r
      typeRep dt
    inst _ _ (NonRec (RecordRep r)) = do
      dt <- RecordRep <$> r
      typeRep dt
    inst tv node (RecHead n t) = do
      ph <- typeRep (IdTypeRep (tIdRep (strId "#rec#") Type))
      result <- local (HList.modify @"RecTypes" @(Map.Map Int TypeRep) (Map.insert n ph)) (inst tv node (NonRec t))
      liftIO $ UF.union ph result
      return result

generalize :: (MonadIO m, Fail.MonadFail m,
  Occurs ("TypeEnv" :- TypeEnv) ts HList,
  MonadReader (HList ts) m) => TypeRep -> m SchemeRep
generalize t = do
  env <- select @"TypeEnv" @TypeEnv <$> ask
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

type Constraint = (TypeRep, TypeRep)

getType :: (DistAnn e TypeRep e') => Fix e' i -> TypeRep
getType e = let (_ O.:&: t) = projectA (unTerm e) in t

runSolver :: (MonadIO m, Fail.MonadFail m,
  Occurs ("NameCounter" :- IORef Int) ts HList, MonadReader (HList ts) m) => [Constraint] -> m ()
runSolver cs = do
  -- liftIO $ print =<< mapM (\c -> (,) <$> showTypeRep (fst c) <*> showTypeRep (snd c)) cs
  mapM_ (uncurry unify) cs

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, 
  Occurs ("NameCounter" :- IORef Int) ts HList,
  Update ("TypeEnv" :- TypeEnv) ts HList,
  MonadReader (HList ts) m, Expr :<: g, DistAnn g TypeRep h) => Infer Expr h m where

  inferAlg (IdExpr x) = Compose $ do
    TypeEnv env <- select @"TypeEnv" @TypeEnv <$> ask
    case Map.lookup x env of
      Nothing -> Fail.fail "variable not found"
      Just scheme -> do
        t <- instantiate scheme
        return $ iAIdExpr t x

  inferAlg (AppExpr fun arg) = Compose $ do
    fun' <- getCompose fun
    arg' <- getCompose arg
    let funType = getType fun'
        argType = getType arg'
    retType <- genFreshIdType Type
    funTypeUni <- typeRep (FunTypeRep argType retType)
    tell [(funType, funTypeUni)]
    return $ iAAppExpr retType fun' arg'

  inferAlg (LamExpr param body) = Compose $ do
    paramType <- genFreshIdType Type
    let intro = Map.singleton param (Forall Set.empty paramType)
    body' <- local (HList.modify @"TypeEnv" @TypeEnv (extendTypeEnv intro)) (getCompose body)
    let bodyType = getType body'
    lamType <- typeRep (FunTypeRep paramType bodyType)
    -- liftIO $ putStrLn =<< showTypeRep lamType
    return $ iALamExpr lamType param body'

  -- [] :: forall a. [a]
  -- length :: forall a. [a] -> Int
  -- (length :: [b] -> Int) ([] :: [b]) :: Int

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, 
  Occurs ("NameCounter" :- IORef Int) ts HList,
  Update ("TypeEnv" :- TypeEnv) ts HList,
  MonadReader (HList ts) m, FunDef p t :<: g, DistAnn g TypeRep h,
  MonadState TypeEnv m) => 
  Infer (FunDef p t) h m where

  inferAlg (FunDef name c t f e) = Compose $ do
    (f', cs) <- listen (getCompose f)
    let funType = getType f'
    runSolver cs
    -- liftIO $ putStrLn =<< showTypeRep funType
    funScheme <- generalize funType
    let intro = Map.singleton name funScheme
    e' <- local (HList.modify @"TypeEnv" @TypeEnv (extendTypeEnv intro)) (getCompose e)
    let eType = getType e'
    originalEnv <- HList.select @"TypeEnv" @TypeEnv <$> ask
    put (extendTypeEnv intro originalEnv)
    return $ iAFunDef eType name c t f' e'

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, 
  Occurs ("NameCounter" :- IORef Int) ts HList,
  Update ("TypeEnv" :- TypeEnv) ts HList,
  MonadReader (HList ts) m, RecDef p t :<: g, DistAnn g TypeRep h,
  MonadState TypeEnv m) => 
  Infer (RecDef p t) h m where

  inferAlg (RecDef name c t f e) = Compose $ do
    self <- genFreshIdType Type
    let intro = Map.singleton name (Forall Set.empty self)
    (f', cs) <- listen (local (
      HList.modify @"TypeEnv" @TypeEnv (extendTypeEnv intro)) (getCompose f))
    let funType = getType f'
        cs' = (self, funType) : cs
    runSolver cs'
    -- liftIO $ putStrLn =<< showTypeRep funType
    funScheme <- generalize funType
    let intro = Map.singleton name funScheme
    e' <- local (HList.modify @"TypeEnv" @TypeEnv (extendTypeEnv intro)) (getCompose e)
    let eType = getType e'
    originalEnv <- HList.select @"TypeEnv" @TypeEnv <$> ask
    put (extendTypeEnv intro originalEnv)
    return $ iARecDef eType name c t f' e'

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, 
  Occurs ("NameCounter" :- IORef Int) ts HList,
  Update ("TypeEnv" :- TypeEnv) ts HList,
  MonadReader (HList ts) m, LabelExpr LabelAsFun :<: g, DistAnn g TypeRep h) => 
  Infer (LabelExpr LabelAsFun) h m where
  inferAlg (LabelApp l e) = Compose $ do
    e' <- getCompose e
    let eType = getType e'
    rv <- genFreshIdType (RowLack (L.singleton l))
    row <- typeRep (RowRep (Map.singleton l eType) rv)
    t <- typeRep (VariantRep row)
    return $ iALabelApp t l e'

class PatInfer f m where
  patInferAlg :: Alg f (K (m (TypeRep, Maybe TypeRep, Map.Map Id TypeRep)))

$(derive [liftSum] [''PatInfer])

instance (MonadIO m, Fail.MonadFail m,
  Occurs ("NameCounter" :- IORef Int) ts HList,
  MonadReader (HList ts) m) => PatInfer Pat m where

  patInferAlg (IdPat i) = K $ do
    t <- genFreshIdType Type
    return (t, Nothing, Map.singleton i t)

  patInferAlg (LabelPat l) = K $ do
    emptyRow <- typeRep (IdTypeRep (tIdRep (strId "*") (RowPres L.empty)))
    unitType <- typeRep (RecordRep emptyRow)
    rv <- genFreshIdType (RowLack (L.singleton l))
    row <- typeRep (RowRep (Map.singleton l unitType) rv)
    t <- typeRep (VariantRep row)
    rest <- typeRep (VariantRep rv)
    return (t, Just rest, Map.empty)

instance (MonadIO m, Fail.MonadFail m,
  Occurs ("NameCounter" :- IORef Int) ts HList,
  MonadReader (HList ts) m) => PatInfer AppPat m where
  patInferAlg (AppIdPat l i) = K $ do
    idType <- genFreshIdType Type
    rv <- genFreshIdType (RowLack (L.singleton l))
    row <- typeRep (RowRep (Map.singleton l idType) rv)
    t <- typeRep (VariantRep row)
    rest <- typeRep (VariantRep rv)
    return (t, Just rest, Map.singleton i idType)
  patInferAlg _ = K $ Fail.fail "not a simple pattern"

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, 
  Occurs ("NameCounter" :- IORef Int) ts HList,
  Update ("TypeEnv" :- TypeEnv) ts HList,
  MonadReader (HList ts) m, RecordOps :<: g, DistAnn g TypeRep h) => Infer RecordOps h m where
  inferAlg (RecordCons fields) = Compose $ do
    let process (l, e) = do
          e' <- getCompose e
          let eType = getType e'
          return ((l, e'), (l, eType))
    fieldsTypes <- mapM process fields
    emptyRow <- typeRep (IdTypeRep (tIdRep (strId "*") (RowPres L.empty)))
    row <- if length fields == 0 then return emptyRow else typeRep (RowRep (Map.fromList (map snd fieldsTypes)) emptyRow)
    t <- typeRep (RecordRep row)
    let fields' = map fst fieldsTypes
    return $ iARecordCons t fields'
  inferAlg (FieldAccess r l) = Compose $ do
    r' <- getCompose r
    let rType = getType r'
    rv <- genFreshIdType (RowLack (L.singleton l))
    fieldType <- genFreshIdType Type
    row <- typeRep (RowRep (Map.singleton l fieldType) rv)
    rTypeUni <- typeRep (RecordRep row)
    tell [(rType, rTypeUni)]
    return $ iAFieldAccess fieldType r' l
  inferAlg (FieldRemove r l) = Compose $ do
    r' <- getCompose r
    let rType = getType r'
    rv <- genFreshIdType (RowLack (L.singleton l))
    fieldType <- genFreshIdType Type
    row <- typeRep (RowRep (Map.singleton l fieldType) rv)
    rTypeUni <- typeRep (RecordRep row)
    restType <- typeRep (RecordRep rv)
    tell [(rType, rTypeUni)]
    return $ iAFieldRemove restType r' l
  inferAlg (RecordMod r mod) = Compose $ do
    r' <- getCompose r
    let rType = getType r'
    let process (l, e) = do
          e' <- getCompose e
          let eType = getType e'
          return ((l, e'), (l, eType))
    modTypes <- mapM process mod
    rv <- genFreshIdType (RowLack (L.fromList (map fst mod)))
    row <- if length mod == 0 then return rv else typeRep (RowRep (Map.fromList (map snd modTypes)) rv)
    modType <- typeRep (RecordRep row)
    let mod' = map fst modTypes
    tell [(rType, modType)]
    return $ iARecordMod rType r' mod'
  inferAlg (RecordExt r ext) = Compose $ do
    r' <- getCompose r
    let rType = getType r'
    let process (l, e) = do
          e' <- getCompose e
          let eType = getType e'
          return ((l, e'), (l, eType))
    extTypes <- mapM process ext
    rv <- genFreshIdType (RowLack (L.fromList (map fst ext)))
    row <- if length ext == 0 then return rv else typeRep (RowRep (Map.fromList (map snd extTypes)) rv)
    rTypeUni <- typeRep (RecordRep rv)
    extType <- typeRep (RecordRep row)
    let m' = map fst extTypes
    tell [(rType, rTypeUni)]
    return $ iARecordExt extType r' m'

{-
instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, 
  Occurs ("NameCounter" :- IORef Int) ts HList,
  Update ("TypeEnv" :- TypeEnv) ts HList,
  MonadReader (HList ts) m, Match InferPatSig SimplePat :<: g, DistAnn g TypeRep h) => 
  Infer (Match InferPatSig SimplePat) h m where
  inferAlg (Match e cases) = Compose $ do
    e' <- getCompose e
    rhsIdType <- genFreshIdType Type 
    let eType = getType e'
    let process (Nothing, _) _ = Fail.fail "redundant case"
        process (Just t, cases') (p, rhs) = do
          (patType, rest, intro) <- unK (cata patInferAlg p)
          tell [(t, patType)]
          rhs' <- local (HList.modify @"TypeEnv" @TypeEnv (extendTypeEnv intro)) (getCompose rhs)
          let rhsType = getType rhs'
          tell [(rhsType, rhsIdType)]
          return (rest, (p, rhs') : cases')
    (rest, cases') <- second reverse <$> foldM process (Just eType, []) cases
    emptyRow <- typeRep (IdTypeRep (tIdRep (strId "*") (RowPres L.empty)))
    emptyType <- typeRep (VariantRep emptyRow)
    case rest of
      Just t -> tell [(t, emptyType)]
      Nothing -> return ()
    return $ iAMatch rhsIdType e' cases'
-}

isFreeVariant :: (MonadIO m, Fail.MonadFail m,
  Occurs ("TypeEnv" :- TypeEnv) ts HList,
  MonadReader (HList ts) m) => TypeRep -> m Bool
isFreeVariant t = do
  dtv <- liftIO $ UF.find' t
  case structure dtv of
    VariantRep r -> do
      drv <- liftIO $ UF.find' r
      case structure drv of
        IdTypeRep tid -> do
          env <- select @"TypeEnv" @TypeEnv <$> ask
          (not . Set.member tid) <$> ftv env
        _ -> return False
    _ -> return False

sizeOf :: (MonadIO m, Fail.MonadFail m) => TypeRep -> m Int
sizeOf t = traverseTypeRep (const sizeOf') t >>= (return . getSum)
  where 
    sizeOf' (RecBody _) = Sum 1
    sizeOf' (RecHead _ t) = sizeOf' (NonRec t)
    sizeOf' (NonRec (IdTypeRep _)) = Sum 1
    sizeOf' (NonRec (FunTypeRep arg ret)) = arg <> ret
    sizeOf' (NonRec (RowRep ls r)) = fold ls <> r
    sizeOf' (NonRec (VariantRep r)) = r
    sizeOf' (NonRec (RecordRep r)) = r

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, 
  Occurs ("NameCounter" :- IORef Int) ts HList,
  Update ("TypeEnv" :- TypeEnv) ts HList,
  MonadReader (HList ts) m, Match InferPatSig SimplePat :<: g, DistAnn g TypeRep h) => 
  Infer (Match InferPatSig SimplePat) h m where
  inferAlg (Match e cases) = Compose $ do
    emptyRow <- typeRep (IdTypeRep (tIdRep (strId "*") (RowPres L.empty)))
    emptyType <- typeRep (VariantRep emptyRow)
    (e', cs) <- listen (getCompose e)
    runSolver cs
    rhsIdType <- genFreshIdType Type
    let eType = getType e'
    let process (Nothing, _) _ = Fail.fail "redundant case"
        process (Just t, cases') (p, rhs) = do
          (patType, rest, intro) <- unK (cata patInferAlg p)
          (Forall ftvBefore _) <- generalize t
          runSolver [(patType, t)]
          (Forall ftvAfter _) <- generalize t
          rest' <- case rest of
            Just r -> do
              if ftvBefore /= ftvAfter then Fail.fail "impossible pattern"
              else do
                free <- isFreeVariant r
                if free then runSolver [(r, emptyType)] >> return Nothing
                else return (Just r)
            Nothing -> return Nothing
          intro' <- mapM generalize intro
          rhs' <- local (HList.modify @"TypeEnv" @TypeEnv (extendTypeEnv intro')) (getCompose rhs)
          let rhsType = getType rhs'
          tell [(rhsType, rhsIdType)]
          return (rest', (p, rhs') : cases')
    (rest, cases') <- second reverse <$> foldM process (Just eType, []) cases
    case rest of
      Just t -> tell [(t, emptyType)]
      Nothing -> return ()
    return $ iAMatch rhsIdType e' cases'

instance (MonadIO m, Fail.MonadFail m, MonadWriter [Constraint] m, 
  Occurs ("NameCounter" :- IORef Int) ts HList,
  Update ("TypeEnv" :- TypeEnv) ts HList,
  MonadReader (HList ts) m, RHS i :<: g, DistAnn g TypeRep h) => 
  Infer (RHS i) h m where
  inferAlg (RHS i e) = Compose $ do
    e' <- getCompose e
    let eType = getType e'
    return $ iARHS eType i e'

type TestSig = RecDef InferPresSig InferTypeSig :+: FunDef InferPresSig InferTypeSig :+: Expr :+: Match InferPatSig SimplePat :+: LabelExpr LabelAsFun :+: RecordOps

noCons = ([], [] :: [(Id, Fix InferPresSig PRES)], iUnknownType :: Fix InferTypeSig TypeKind)

testExpr1 :: Fix TestSig EXPR
testExpr1 = iFunDef test False noCons (iLamExpr x (iAppExpr (iIdExpr x) (iIdExpr x))) (iIdExpr test)
  where x = strId "x"
        test = strId "test"

testExpr2 :: Fix TestSig EXPR
testExpr2 = iRecDef fix False noCons (iLamExpr f (iAppExpr (iIdExpr f) (iAppExpr (iIdExpr fix) (iIdExpr f)))) (iIdExpr fix)
  where f = strId "f"
        fix = strId "fix"

testExpr3 :: Fix TestSig EXPR
testExpr3 = iFunDef test False noCons (iLamExpr x (iMatch (iIdExpr x) [
  (iLabelPat (L.label "A") :: Fix InferPatSig SimplePat, iLabelApp (L.label "B") (iRecordCons [])), 
  (iIdPat x :: Fix InferPatSig SimplePat, iIdExpr x)])) (iIdExpr test)
  where x = strId "x"
        test = strId "test"

testExpr4 :: Fix TestSig EXPR
testExpr4 = 
  iFunDef flatMap False noCons (iLamExpr x (iLamExpr f (iMatch (iIdExpr x) [
    (iAppIdPat (L.label "Success") a :: Fix InferPatSig SimplePat, (iAppExpr (iIdExpr f) (iIdExpr a))),
    (iAppIdPat (L.label "Failure") b :: Fix InferPatSig SimplePat, iLabelApp (L.label "Failure") (iIdExpr b))]))) $

  iFunDef alter False noCons (iLamExpr x (iLamExpr y (iMatch (iIdExpr x) [
    (iAppIdPat (L.label "Success") a :: Fix InferPatSig SimplePat, iLabelApp (L.label "Success") (iIdExpr a)),
    (iAppIdPat (L.label "Failure") b :: Fix InferPatSig SimplePat, iIdExpr y)]))) $

  iFunDef seq False noCons (iLamExpr fs (iLamExpr ss (iLamExpr x (
    iAppExpr (iAppExpr (iIdExpr flatMap) (iAppExpr (iIdExpr fs) (iIdExpr x))) (iIdExpr ss))))) $

  iFunDef lChoice False noCons (iLamExpr fs (iLamExpr ss (iLamExpr x (
    iAppExpr (iAppExpr (iIdExpr alter) (iAppExpr (iIdExpr fs) (iIdExpr x))) (iAppExpr (iIdExpr ss) (iIdExpr x)))))) $
  
  iFunDef id False noCons (iLamExpr x (iLabelApp (L.label "Success") (iIdExpr x))) $

  iFunDef fail False noCons (iLamExpr x (iLabelApp (L.label "Failure") (iLabelApp (L.label "MSG") (iRecordCons [])))) $
  
  iFunDef try False noCons (iLamExpr s (iAppExpr (iAppExpr (iIdExpr lChoice) (iIdExpr s)) (iIdExpr id))) $
  
  iFunDef test False noCons (iLamExpr x (iMatch (iAppExpr (iAppExpr (iIdExpr try) (iIdExpr fail)) (iIdExpr x)) [
    (iAppIdPat (L.label "Success") a :: Fix InferPatSig SimplePat, iLabelApp (L.label "OK") (iRecordCons []))])) $

  iFunDef test False noCons (iMatch (iLabelApp (L.label "Success") (iRecordCons [])) [
    (iIdPat a :: Fix InferPatSig SimplePat, iRecordCons [
      (L.label "Fst", iAppExpr (iAppExpr (iIdExpr alter) (iIdExpr a)) (iIdExpr a)),
      (L.label "Snd", iMatch (iIdExpr a) [
        (iAppIdPat (L.label "Success") b :: Fix InferPatSig SimplePat, iLabelApp (L.label "OK") (iRecordCons []))])])]) $

  iIdExpr try

  where flatMap = strId "flatMap"
        alter = strId "alter"
        seq = strId "seq"
        lChoice = strId "lChoice"
        id = strId "id"
        fail = strId "fail"
        try = strId "try"
        test = strId "test"
        a = strId "a"
        b = strId "b"
        f = strId "f"
        x = strId "x"
        y = strId "y"
        s = strId "s"
        ss = strId "ss"
        fs = strId "fs"
{-
testInfer :: Fix TestSig EXPR -> IO ()
testInfer prog = do
  c <- newIORef 0
  let cxt :: HList '["NameCounter" :- IORef Int, "TypeEnv" :- TypeEnv]
      cxt = Field c :| Field (TypeEnv Map.empty) :| HNil
  r <- runExceptT (fmap fst $ runWriterT (flip runReaderT cxt (getCompose (cata inferAlg prog))))
  case r of
    Right (r :: Fix ((RecDef InferPresSig InferTypeSig :&: TypeRep) :+: (FunDef InferPresSig InferTypeSig :&: TypeRep) :+: (Expr :&: TypeRep) :+: (Match InferPatSig SimplePat :&: TypeRep) :+: (LabelExpr LabelAsFun :&: TypeRep) :+: (RecordOps :&: TypeRep)) EXPR) -> do
      let finalType = getType r
      putStrLn =<< showTypeRep False finalType
    Left m -> undefined
-}