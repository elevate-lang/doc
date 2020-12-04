{-# LANGUAGE TupleSections, QuasiQuotes, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs, TypeApplications, RankNTypes, AllowAmbiguousTypes #-}

module Eval where

import Subst

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Reader
import AST
import Data.Comp.Multi
import Data.Comp.Multi.Derive
import Data.Comp.Multi.HTraversable
import Data.Functor.Compose
import HList as HL
import Data.IORef
import Id
import qualified Control.Monad.Fail as Fail
import Data.Foldable
import Data.List
import PatternElaboration (patId)
import Control.Monad.State as State

class Eval f g m where
  evalAlg :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''Eval])

packEval :: (Occurs (Field "RunEval" Bool) ts HList, 
  MonadReader (HList ts) m, HTraversable f2, f2 :<: f3) => 
  f2 (Compose m (Cxt h f3 a1)) a2 -> m (Cxt h f3 a1 a2) -> Compose m (Cxt h f3 a1) a2
packEval x m = Compose $ do
  run <- select @"RunEval" @Bool <$> ask
  if run then m else do
    fmap (Term . inj) (hmapM getCompose x)

instance (Update ("Subst" :- Subst (Fix g EXPR)) ts HList,
  Update ("RunEval" :- Bool) ts HList,
  MonadReader (HList ts) m, Fail.MonadFail m,
  HFunctor g, Expr :<: g, Eval g g m) => Eval Expr g m where
  evalAlg o@(IdExpr x) = Compose $ do
    subst <- select @"Subst" @(Subst (Fix g EXPR)) <$> ask
    run <- select @"RunEval" @Bool <$> ask
    case Map.lookup x subst of
      Nothing -> if run then Fail.fail "invalid reduction - identifier" else return $ iIdExpr x
      Just e -> return e
  evalAlg o@(AppExpr fun arg) = packEval o $ do
    fun' <- getCompose fun
    arg' <- getCompose arg
    case project fun' :: Maybe (Expr (Fix g) EXPR) of
      Just (LamExpr param body) -> do
        let updateCxt = Map.insert param arg'
        local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose (cata evalAlg body))
      _ -> Fail.fail "invalid reduction - application"
  evalAlg (LamExpr param body) = Compose $ do
    let updateCxt = Map.delete param
    body' <- local (HL.modify @"RunEval" @Bool (const False) . 
      HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose body)
    return $ iLamExpr param body'

instance (Update ("Subst" :- Subst (Fix g EXPR)) ts HList,
  Update ("RunEval" :- Bool) ts HList,
  MonadReader (HList ts) m, Fail.MonadFail m,
  HFunctor g, (FunDef p t) :<: g, Expr :<: g,
  Update ("REPLEnv" :- Subst (Fix g EXPR)) ts' HList,
  MonadState (HList ts') m) => Eval (FunDef p t) g m where
  evalAlg o@(FunDef name c t f e) = Compose $ do
    run <- select @"RunEval" @Bool <$> ask
    if run then do
      f' <- getCompose f
      let updateCxt = Map.insert name f'
      originalEnv <- HL.select @"Subst" @(Subst (Fix g EXPR)) <$> ask
      State.modify (HL.modify @"REPLEnv" @(Subst (Fix g EXPR)) (const $ updateCxt originalEnv))
      local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose e)
    else do
      f' <- getCompose f
      let updateCxt = Map.delete name
      e' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose e)
      return $ iFunDef name c t f' e'

instance (Update ("Subst" :- Subst (Fix g EXPR)) ts HList,
  Update ("RunEval" :- Bool) ts HList,
  MonadReader (HList ts) m, Fail.MonadFail m, MonadIO m,
  HFunctor g, (RecDef p t) :<: g, Expr :<: g, Eval g g m,
  Update ("REPLEnv" :- Subst (Fix g EXPR)) ts' HList,
  MonadState (HList ts') m) => Eval (RecDef p t) g m where
  evalAlg o@(RecDef name c t f e) = Compose $ do
    run <- select @"RunEval" @Bool <$> ask
    if run then do
      let updateCxt = Map.insert name (iIdExpr name)
      f' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose f)
      let updateCxt = Map.insert name (iRecDef name c t f' (iIdExpr name))
      f'' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose (cata evalAlg f'))
      let updateCxt = Map.insert name f''
      originalEnv <- HL.select @"Subst" @(Subst (Fix g EXPR)) <$> ask
      State.modify (HL.modify @"REPLEnv" @(Subst (Fix g EXPR)) (const $ updateCxt originalEnv))
      local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose e)
    else do
      let updateCxt = Map.delete name
      f' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose f)
      e' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose e)
      return $ iRecDef name c t f' e'

instance (Fail.MonadFail m, HFunctor g, RecordOps :<: g, 
  Occurs (Field "RunEval" Bool) ts HList, MonadReader (HList ts) m) => 
  Eval RecordOps g m where
  evalAlg o@(RecordCons r) = packEval o $ do
    r' <- mapM (\(l, e) -> (l,) <$> getCompose e) r
    return $ iRecordCons r'
  evalAlg o@(FieldAccess r l) = packEval o $ do
    r' <- getCompose r
    case project r' :: Maybe (RecordOps (Fix g) EXPR) of
      Just (RecordCons rv) -> case filter ((== l) . fst) rv of
        [(_, v)] -> return v
        _ -> Fail.fail "invalid reduction"
      _ -> Fail.fail "invalid reduction"
  evalAlg o@(FieldRemove r l) = packEval o $ do
    r' <- getCompose r
    case project r' :: Maybe (RecordOps (Fix g) EXPR) of
      Just (RecordCons rv) -> case filter ((== l) . fst) rv of
        [v] -> return $ iRecordCons (deleteBy (\a b -> fst a == fst b) v rv)
        _ -> Fail.fail "invalid reduction"
      _ -> Fail.fail "invalid reduction"
  evalAlg o@(RecordMod r m) = packEval o $ do
    r' <- getCompose r
    m' <- mapM (\(l, e) -> (l,) <$> getCompose e) m
    case project r' :: Maybe (RecordOps (Fix g) EXPR) of
      Just (RecordCons rv) -> return $ iRecordCons (unionBy (\a b -> fst a == fst b) rv m')
      _ -> Fail.fail "invalid reduction"
  evalAlg o@(RecordExt r m) = packEval o $ do
    r' <- getCompose r
    m' <- mapM (\(l, e) -> (l,) <$> getCompose e) m
    case project r' :: Maybe (RecordOps (Fix g) EXPR) of
      Just (RecordCons rv) -> return $ iRecordCons (rv ++ m')
      _ -> Fail.fail "invalid reduction"

instance (Monad m, HFunctor g, (LabelExpr LabelAsFun) :<: g,
  Occurs (Field "RunEval" Bool) ts HList, MonadReader (HList ts) m) => Eval (LabelExpr LabelAsFun) g m where
  evalAlg o@(LabelApp l e) = packEval o $ iLabelApp l <$> getCompose e

instance (Update ("Subst" :- Subst (Fix g EXPR)) ts HList,
  Occurs (Field "RunEval" Bool) ts HList,
  MonadReader (HList ts) m, Fail.MonadFail m, RecordOps :<: g, LabelExpr LabelAsFun :<: g, EqHF g,
  HFunctor g, (Match SimplePatSig SimplePat) :<: g, Expr :<: g) => 
  Eval (Match SimplePatSig SimplePat) g m where
  evalAlg o@(Match e cases) = Compose $ do
    run <- select @"RunEval" @Bool <$> ask
    if run then do
      e' <- getCompose e
      let process [] = Fail.fail "invalid reduction"
          process ((p, rhs) : rest) = caseH (\case
              IdPat i -> do
                let updateCxt = Map.insert i e'
                local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose rhs)
              LabelPat lp -> do
                case project e' :: Maybe (LabelExpr LabelAsFun (Fix g) EXPR) of
                  Just (LabelApp l v) -> if l /= lp then process rest else
                    if v /= iRecordCons [] then Fail.fail "invalid reduction"
                    else getCompose rhs
                  _ -> Fail.fail "invalid reduction"
            ) (\case
              AppIdPat lp i -> do
                case project e' :: Maybe (LabelExpr LabelAsFun (Fix g) EXPR) of
                  Just (LabelApp l v) -> if l /= lp then process rest else do
                    let updateCxt = Map.insert i v
                    local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose rhs)
                  _ -> Fail.fail "invalid reduction"
            ) (unTerm p)
      process cases
    else do
      e' <- getCompose e
      let process (p, rhs) = (p,) <$> caseH (\case
              IdPat i -> do
                let updateCxt = Map.delete i
                local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose rhs)
              LabelPat _ -> getCompose rhs
            ) (\case
              AppIdPat _ i -> do
                let updateCxt = Map.delete i
                local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose rhs)
            ) (unTerm p)
      cases' <- mapM process cases
      return $ iMatch e' cases'
        
instance (Monad m, HFunctor g, RHS i :<: g,
  Occurs (Field "RunEval" Bool) ts HList, MonadReader (HList ts) m) => Eval (RHS i) g m where
  evalAlg o@(RHS _ e) = packEval o $ getCompose e