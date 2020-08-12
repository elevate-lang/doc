{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, GADTs, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Desugar where

import Parser
import AST
import Control.Monad.State
import Data.Functor.Compose
import Data.Set as Set
import Data.Map.Strict as Map
import Text.RawString.QQ
import Data.Comp.Multi
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Derive

class GenRecDef f g m where
  genRecDefAlg :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''GenRecDef])

instance {-# OVERLAPPABLE #-} (MonadState (Set.Set Id) m, HTraversable f, f :<: g) => GenRecDef f g m where
  genRecDefAlg = Compose . fmap inject . hmapM getCompose

instance {-# OVERLAPPABLE #-} (MonadState (Set.Set Id) m, Expr :<: g) => GenRecDef Expr g m where
  genRecDefAlg (IdExpr i) = Compose $ do
    modify (Set.delete i)
    return (iIdExpr i)
  genRecDefAlg e = Compose $ fmap inject (hmapM getCompose e)

instance {-# OVERLAPPABLE #-} (MonadState (Set.Set Id) m, FunDef p t :<: g, RecDef p t :<: g) => GenRecDef (FunDef p t) g m where
  genRecDefAlg (FunDef i c ft fr p r body e) = Compose $ do
    isShadowing <- gets (Set.member i)
    modify (Set.insert i)
    body' <- getCompose body
    isUsed <- not <$> gets (Set.member i)
    e' <- getCompose e
    case isShadowing of
      True -> modify (Set.insert i)
      False -> modify (Set.delete i)
    return $ case isUsed of
      True -> iRecDef i c ft fr p r body' e'
      False -> iFunDef i c ft fr p r body' e'

genRecDef :: Fix ExprSig EXPR -> Fix FullSig EXPR
genRecDef = flip evalState (Set.empty) . getCompose . cata genRecDefAlg

recExample :: String
recExample = [r|
let f x y = {X: A x | Y: let f a b = t in f 2 | Z: let h = f 2 in h} in
let g x = g 1 in g 2
|]

testGenRecDef = case testRun program recExample of
  Right p -> genRecDef p
  Left _ -> error "Parsing Error"