{-# LANGUAGE TupleSections, QuasiQuotes, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs, TypeApplications, RankNTypes, AllowAmbiguousTypes #-}

module Subst where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Reader
import AST
import Data.Comp.Multi
import Data.Comp.Multi.Derive
import Data.Functor.Compose
import HList as HL
import Data.IORef
import Id
import qualified Control.Monad.Fail as Fail
import Data.Foldable

type Subst e = Map.Map Id e

class LinearPat f m where
  pvAlg :: AlgM m f (K (Set.Set Id))
  
$(derive [liftSum] [''LinearPat])

class SubstPat f g m where
  substpvAlg :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''SubstPat])

instance (Occurs ("SubstPV" :- Subst Id) ts HList,
  MonadReader (HList ts) m, Pat :<: g) => SubstPat Pat g m where
  substpvAlg (IdPat i) = Compose $ do
    subst <- select @"SubstPV" @(Subst Id) <$> ask
    case Map.lookup i subst of
      Just i' -> return $ iIdPat i'
      Nothing -> return $ iIdPat i
  substpvAlg (LabelPat l) = Compose . return $ iLabelPat l

instance (Occurs ("SubstPV" :- Subst Id) ts HList,
  MonadReader (HList ts) m, AppPat :<: g) => SubstPat AppPat g m where
  substpvAlg (AppPat l p) = Compose $ iAppPat l <$> getCompose p
  substpvAlg (AppIdPat l i) = Compose $ do
    subst <- select @"SubstPV" @(Subst Id) <$> ask
    case Map.lookup i subst of
      Just i' -> return $ iAppIdPat l i'
      Nothing -> return $ iAppIdPat l i

instance (Monad m, RecordPat :<: g) => SubstPat RecordPat g m where
  substpvAlg (RecordPat ps) = Compose $ iRecordPat <$> mapM (\(l, Compose p) -> (l,) <$> p) ps

instance (Monad m, MatchAllPat :<: g) => SubstPat MatchAllPat g m where
  substpvAlg MatchAllPat = Compose $ return iMatchAllPat

instance (Fail.MonadFail m) => LinearPat Pat m where
  pvAlg (IdPat i) = return (K (Set.singleton i))
  pvAlg (LabelPat _) = return (K Set.empty)

instance (Fail.MonadFail m) => LinearPat AppPat m where
  pvAlg (AppPat _ p) = return p
  pvAlg (AppIdPat _ i) = return (K (Set.singleton i))

instance (Fail.MonadFail m) => LinearPat RecordPat m where
  pvAlg (RecordPat ps) = K <$> foldM process Set.empty ps
    where process acc (_, now) = let pv = unK now in 
            if not (Set.null (Set.intersection acc pv))
            then Fail.fail "nonlinear pattern"
            else return (Set.union acc pv)
            
instance (Fail.MonadFail m) => LinearPat MatchAllPat m where
  pvAlg MatchAllPat = return (K Set.empty)

class Substitutable f g m where
  substAlg :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''Substitutable])

class ContainFV f where
  fvAlg :: Alg f (K (Set.Set Id))

$(derive [liftSum] [''ContainFV])

instance ContainFV Expr where
  fvAlg (IdExpr x) = K $ Set.singleton x
  fvAlg (AppExpr fun arg) = K $ Set.union (unK fun) (unK arg)
  fvAlg (LamExpr param body) = K $ Set.delete param (unK body)

instance (Update ("Subst" :- Subst (Fix g EXPR)) ts HList,
  MonadIO m, Occurs ("NameCounter" :- IORef Int) ts HList,
  MonadReader (HList ts) m, 
  HFunctor g, Expr :<: g, ContainFV g) => Substitutable Expr g m where
  substAlg (IdExpr x) = Compose $ do
    subst <- select @"Subst" @(Subst (Fix g EXPR)) <$> ask
    case Map.lookup x subst of
      Nothing -> return $ iIdExpr x
      Just e -> return e
  substAlg (AppExpr fun arg) = Compose $ do
    fun' <- getCompose fun
    arg' <- getCompose arg
    return $ iAppExpr fun' arg'
  substAlg (LamExpr param body) = Compose $ do
    subst <- select @"Subst" @(Subst (Fix g EXPR)) <$> ask
    let fv = fold (Map.map (unK . cata fvAlg) subst)
    if Set.member param fv then do
      freshId <- genFreshId "#x"
      let updateCxt = Map.insert param (iIdExpr freshId)
      body' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose body)
      return $ iLamExpr freshId body'
    else do
      let updateCxt = Map.delete param
      body' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose body)
      return $ iLamExpr param body'

instance ContainFV (FunDef p t) where
  fvAlg (FunDef name _ _ f e) = K $ Set.union (unK f) (Set.delete name (unK e))

instance (Update ("Subst" :- Subst (Fix g EXPR)) ts HList,
  MonadIO m, Occurs ("NameCounter" :- IORef Int) ts HList,
  MonadReader (HList ts) m, 
  HFunctor g, (FunDef p t) :<: g, Expr :<: g, ContainFV g) => Substitutable (FunDef p t) g m where
  substAlg (FunDef name c t f e) = Compose $ do
    subst <- select @"Subst" @(Subst (Fix g EXPR)) <$> ask
    let fv = fold (Map.map (unK . cata fvAlg) subst)
    if Set.member name fv then do
      freshId <- genFreshId "#x"
      let updateCxt = Map.insert name (iIdExpr freshId)
      f' <- getCompose f
      e' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose e)
      return $ iFunDef freshId c t f' e'
    else do
      let updateCxt = Map.delete name
      f' <- getCompose f
      e' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose e)
      return $ iFunDef name c t f' e'

instance ContainFV (RecDef p t) where
  fvAlg (RecDef name _ _ f e) = K $ Set.delete name (Set.union (unK f) (unK e))

instance (Update ("Subst" :- Subst (Fix g EXPR)) ts HList,
  MonadIO m, Occurs ("NameCounter" :- IORef Int) ts HList,
  MonadReader (HList ts) m, 
  HFunctor g, (RecDef p t) :<: g, Expr :<: g, ContainFV g) => Substitutable (RecDef p t) g m where
  substAlg (RecDef name c t f e) = Compose $ do
    subst <- select @"Subst" @(Subst (Fix g EXPR)) <$> ask
    let fv = fold (Map.map (unK . cata fvAlg) subst)
    if Set.member name fv then do
      freshId <- genFreshId "#x"
      let updateCxt = Map.insert name (iIdExpr freshId)
      f' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose f)
      e' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose e)
      return $ iRecDef freshId c t f' e'
    else do
      let updateCxt = Map.delete name
      f' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose f)
      e' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose e)
      return $ iRecDef name c t f' e'

instance ContainFV RecordOps where
  fvAlg (RecordCons r) = K $ Set.unions (map (unK . snd) r)
  fvAlg (FieldAccess r _) = r
  fvAlg (FieldRemove r _) = r
  fvAlg (RecordMod r m) = K $ Set.union (unK r) (Set.unions (map (unK . snd) m))
  fvAlg (RecordExt r m) = K $ Set.union (unK r) (Set.unions (map (unK . snd) m))

instance (Monad m, HFunctor g, RecordOps :<: g) => Substitutable RecordOps g m where
  substAlg (RecordCons r) = Compose $ do
    r' <- mapM (\(l, e) -> (l,) <$> getCompose e) r
    return $ iRecordCons r'
  substAlg (FieldAccess r l) = Compose $ do
    r' <- getCompose r
    return $ iFieldAccess r' l
  substAlg (FieldRemove r l) = Compose $ do
    r' <- getCompose r
    return $ iFieldRemove r' l
  substAlg (RecordMod r m) = Compose $ do
    r' <- getCompose r
    m' <- mapM (\(l, e) -> (l,) <$> getCompose e) m
    return $ iRecordMod r' m'
  substAlg (RecordExt r m) = Compose $ do
    r' <- getCompose r
    m' <- mapM (\(l, e) -> (l,) <$> getCompose e) m
    return $ iRecordExt r' m'

instance ContainFV (LabelExpr LabelAsFun) where
  fvAlg (LabelApp _ e) = e

instance (Monad m, HFunctor g, (LabelExpr LabelAsFun) :<: g) => Substitutable (LabelExpr LabelAsFun) g m where
  substAlg (LabelApp l e) = Compose $ iLabelApp l <$> getCompose e

instance (HTraversable p, LinearPat p Maybe) => ContainFV (Match p l) where
  fvAlg (Match e cases) = K $ Set.union (unK e) (foldl process Set.empty cases)
    where process acc (p, rhs)= case cataM pvAlg p of
            Just pv -> Set.union acc (Set.difference (unK rhs) (unK pv))
            Nothing -> error "impossible"

instance (Update ("Subst" :- Subst (Fix g EXPR)) ts HList,
  MonadIO m, Occurs ("NameCounter" :- IORef Int) ts HList,
  MonadReader (HList ts) m, Fail.MonadFail m,
  HTraversable p, LinearPat p m,
  SubstPat p p (Reader (HList '["SubstPV" :- Subst Id])),
  HFunctor g, (Match p l) :<: g, Expr :<: g, ContainFV g) => 
  Substitutable (Match p l) g m where
  substAlg (Match e cases) = Compose $ do
    e' <- getCompose e
    subst <- select @"Subst" @(Subst (Fix g EXPR)) <$> ask
    let fv = fold (Map.map (unK . cata fvAlg) subst)
    let process (p, rhs) = do
          (K pv) <- cataM pvAlg p
          let common = Set.intersection pv fv
          if not (Set.null common) then do
            freshIdSubst <- sequence (Map.fromSet (const (genFreshId "#x")) common)
            let updateCxt = Map.union (Map.map iIdExpr freshIdSubst)
            rhs' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose rhs)
            let cxtPV :: HList '["SubstPV" :- Subst Id]
                cxtPV = Field freshIdSubst :| HNil
                p' = flip runReader cxtPV . getCompose $ cata substpvAlg p
            return (p', rhs')
          else do
            let updateCxt cxt = foldl (\acc now -> Map.delete now acc) cxt pv
            rhs' <- local (HL.modify @"Subst" @(Subst (Fix g EXPR)) updateCxt) (getCompose rhs)
            return (p, rhs')
    cases' <- mapM process cases
    return $ iMatch e' cases'

instance ContainFV (RHS i) where
  fvAlg (RHS _ e) = e

instance (Monad m, HFunctor g, RHS i :<: g) => Substitutable (RHS i) g m where
  substAlg (RHS i e) = Compose $ iRHS i <$> getCompose e
