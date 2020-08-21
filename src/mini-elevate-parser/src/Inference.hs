{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, GADTs, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module Inference where

import AST
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

