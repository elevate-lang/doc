{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, GADTs, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Desugar where

import Parser
import AST
import Id
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
import qualified Label as L

class TypeDefSubst f g m where
  typeDefSubstAlg :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''TypeDefSubst])

instance {-# OVERLAPPABLE #-} (Monad m, HTraversable f, f :<: g) => TypeDefSubst f g m where
  typeDefSubstAlg = Compose . fmap inject . hmapM getCompose

instance {-# OVERLAPPABLE #-} (Monad m) => TypeDefSubst (TypeDef TypeSig) g m where
  typeDefSubstAlg (TypeDef _ _ e) = e

class LabelFun f g m where
  labelFunAlg :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''LabelFun])

instance {-# OVERLAPPABLE #-} (MonadState (Maybe L.Label) m, HTraversable f, f :<: g) => LabelFun f g m where
  labelFunAlg f = Compose $ do
    g <- fmap inject (hmapM getCompose f)
    put Nothing
    return g

instance {-# OVERLAPPABLE #-} (MonadState (Maybe L.Label) m, 
  LabelExpr LabelAsFun :<: g, RecordOps :<: g) => LabelFun (LabelExpr LabelAsLit) g m where
  labelFunAlg (LabelLit l) = Compose $ do
    put (Just l)
    return (iLabelApp l (iRecordCons []))

instance {-# OVERLAPPABLE #-} (MonadState (Maybe L.Label) m, 
  LabelExpr LabelAsFun :<: g, Expr :<: g) => LabelFun Expr g m where
  labelFunAlg (AppExpr fun arg) = Compose $ do
    fun' <- getCompose fun
    isLabel <- get
    arg' <- getCompose arg
    put Nothing
    case isLabel of
      Just l -> return (iLabelApp l arg')
      Nothing -> return (iAppExpr fun' arg')
  labelFunAlg e = Compose $ do
    e' <- fmap inject (hmapM getCompose e)
    put Nothing
    return e'

class GenRecDef f g m where
  genRecDefAlg :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''GenRecDef])

instance {-# OVERLAPPABLE #-} (MonadState (Set.Set Id) m, HTraversable f, f :<: g) => GenRecDef f g m where
  genRecDefAlg = Compose . fmap inject . hmapM getCompose

instance {-# OVERLAPPABLE #-} (MonadState (Set.Set Id) m, Expr :<: g) => GenRecDef Expr g m where
  genRecDefAlg (IdExpr i) = Compose $ do
    modify (Set.delete i)
    return (iIdExpr i)
  genRecDefAlg (LamExpr param body) = Compose $ do
    isShadowing <- gets (Set.member param)
    body' <- getCompose body
    when isShadowing (modify $ Set.insert param)
    return $ iLamExpr param body'
  genRecDefAlg e = Compose $ fmap inject (hmapM getCompose e)

instance {-# OVERLAPPABLE #-} (MonadState (Set.Set Id) m, FunDef p t :<: g, RecDef p t :<: g) => GenRecDef (FunDef p t) g m where
  genRecDefAlg (FunDef i c t f e) = Compose $ do
    isShadowing <- gets (Set.member i)
    modify (Set.insert i)
    f' <- getCompose f
    isUsed <- gets (not . Set.member i)
    e' <- getCompose e
    modify $ if isShadowing then Set.insert i else Set.delete i
    return $ (if isUsed then iRecDef else iFunDef) i c t f' e'
{-
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
-}