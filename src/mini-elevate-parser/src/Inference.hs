{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, GADTs, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Inference where

import AST
import Control.Monad.State
import Data.Functor.Compose
import Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.RawString.QQ
import Data.Comp.Multi
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Derive

type InferPresSig = Pres

type InferTypeSig = Type :+: RowRep :+: UnknownType :+: RecVariantType

type InferPatSig = Pat :+: AppPat

type InferExprSig = Expr :+: FunDef InferPresSig InferTypeSig :+: RecDef InferPresSig InferTypeSig :+: RecordOps :+: LabelExpr LabelAsFun :+: Match InferPatSig SimplePat

type InferSchemeSig = Scheme InferPresSig

newtype PresEnv = PresEnv (Map.Map Id (Fix InferPresSig PRES))

newtype TypEnv = TypeEnv (Map.Map Id (Fix InferSchemeSig SchemeKind))

--(MonadReader) => Fix InferTypeSig TypeKind -> m ()