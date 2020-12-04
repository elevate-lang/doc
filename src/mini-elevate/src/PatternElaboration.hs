{-# LANGUAGE TupleSections, QuasiQuotes, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs, TypeApplications, RankNTypes, AllowAmbiguousTypes #-}

-- variable | label [variable]
module PatternElaboration where

import Data.Maybe
import Data.Either
import Numeric.Natural
import Text.RawString.QQ
import qualified Data.Vec.Lazy as VL
import Data.Type.Nat hiding (toNatural, cata)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State as MS
import Control.Monad.Writer
import Control.Arrow
import AST
import Data.Comp.Multi
import Data.Comp.Multi.Derive
import Data.Comp.Multi.Show
import Data.Comp.Multi.Equality
import Data.Comp.Multi.Ordering
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Ops
import Data.Comp.Multi.Term
import Data.Functor.Compose
import Data.Comp.Multi.Projection
import Parser
import HList as HL
import Data.IORef
import Id
import Label as L
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans.Except
import Util

import HList as HL
import Data.IORef
import Data.List
import Data.Foldable
import qualified Data.Map.Ordered.Strict as OMap
import Subst

data AccessForm = IAccess Id | IFAccess Id Label | IFRAccess Id Label | IRAccess Id deriving (Eq, Ord, Show)

toRAccess :: AccessForm -> AccessForm
toRAccess (IAccess i) = IRAccess i
toRAccess (IFAccess i l) = IFRAccess i l
toRAccess a = a

accessToAST :: (Expr :<: e, RecordOps :<: e) => AccessForm -> Fix e EXPR
accessToAST (IAccess i) = iIdExpr i
accessToAST (IFAccess i l) = iFieldAccess (iIdExpr i) l
accessToAST (IFRAccess i l) = iRecordMod (iFieldAccess (iIdExpr i) l) []
accessToAST (IRAccess i) = iRecordMod (iIdExpr i) []

accessId :: AccessForm -> Id
accessId (IAccess i) = i
accessId (IFAccess i _) = i
accessId (IFRAccess i _) = i
accessId (IRAccess i) = i

accessSubst :: Subst Id -> AccessForm -> AccessForm
accessSubst s (IAccess i) = IAccess (Map.findWithDefault i i s)
accessSubst s (IFAccess i f) = IFAccess (Map.findWithDefault i i s) f
accessSubst s (IFRAccess i f) = IFRAccess (Map.findWithDefault i i s) f
accessSubst s (IRAccess i) = IRAccess (Map.findWithDefault i i s)

accessMerge :: (Fail.MonadFail m) => AccessForm -> AccessForm -> m AccessForm
accessMerge (IAccess ia) (IAccess ib) = 
  if ia == ib then return (IAccess ia) else Fail.fail "cannot merge"
accessMerge (IFAccess ia fa) (IFAccess ib fb) = 
  if (ia == ib) && (fa == fb) then return (IFAccess ia fa) else Fail.fail "cannot merge"
accessMerge (IFRAccess ia fa) (IFRAccess ib fb) = 
  if (ia == ib) && (fa == fb) then return (IFRAccess ia fa) else Fail.fail "cannot merge"
accessMerge (IRAccess ia) (IRAccess ib) =
  if ia == ib then return (IRAccess ia) else Fail.fail "cannot merge"
accessMerge (IRAccess ia) (IAccess ib) =
  if ia == ib then return (IRAccess ia) else Fail.fail "cannot merge"
accessMerge (IAccess ia) (IRAccess ib) =
  if ia == ib then return (IRAccess ia) else Fail.fail "cannot merge"
accessMerge (IFRAccess ia fa) (IFAccess ib fb) = 
  if (ia == ib) && (fa == fb) then return (IFRAccess ia fa) else Fail.fail "cannot merge"
accessMerge (IFAccess ia fa) (IFRAccess ib fb) = 
  if (ia == ib) && (fa == fb) then return (IFRAccess ia fa) else Fail.fail "cannot merge"
accessMerge _ _ = Fail.fail "cannot merge"

type MatchId = [Int]

data ListModel

data TreeModel

data MatchChain :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> * -> (* -> *) -> * -> * where
  RHSExpr :: Fix e EXPR -> MatchChain e p l self m
  MatchChainList :: AccessForm -> 
    (Fix p l, self ListModel) -> MatchChain e p l self ListModel
  MatchChainTree :: AccessForm -> 
    [(Fix p l, self TreeModel)] -> MatchChain e p l self TreeModel

$(derive [makeHFunctor, makeHFoldable, makeHTraversable][''MatchChain])

iRHSExpr e = Term $ RHSExpr e

iARHSExpr l e = Term $ RHSExpr e :&: l
iMatchChainList a mcs = Term $ MatchChainList a mcs

iAMatchChainList l a mcs = Term $ MatchChainList a mcs :&: l

iMatchChainTree a bs = Term $ MatchChainTree a bs

iAMatchChainTree l a bs = Term $ MatchChainTree a bs :&: l

data PatProp = NonVar | Var deriving (Show, Eq, Ord)

instance Semigroup PatProp where
  Var <> Var = Var
  NonVar <> _ = NonVar
  _ <> NonVar = NonVar
  
instance Monoid PatProp where
  mempty = Var

type TaggedMatchChainSig e p l = MatchChain e p l :&: MatchId

type JudgedMatchChain e = [(PatProp, TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel)]

matchChainToList :: 
  Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel ->
  [TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel]
matchChainToList mc = case unTerm mc of
  MatchChainList a (p, xs) :&: ma -> (MatchChainList a (p, K ()) :&: ma) : matchChainToList xs
  RHSExpr rhs :&: ma -> [RHSExpr rhs :&: ma]

listToMatchChain :: forall e.
  [TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel] ->
  Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel
listToMatchChain ls = foldr roll (error "empty chain") ls
  where roll now acc = case now of
          RHSExpr e :&: rhsId -> iARHSExpr rhsId e :: Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel
          MatchChainList a (p, _) :&: matchId -> iAMatchChainList matchId a (p, acc)

instance (ShowHF e, HFunctor e) => Show (TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel) where
  show m = case m of
    RHSExpr rhs :&: i -> "RHSExpr " ++ (show rhs) ++ (show i)
    MatchChainList a (p, K ()) :&: i -> "MatchChainList " ++ (show a) ++ " " ++ (show p) ++ (show i)

patExpansion :: forall e ts m. (Occurs ("NameCounter" :- IORef Int) ts HList,
  Update ("RHSId" :- MatchId) ts HList,
  Update ("MatchId" :- MatchId) ts HList,
  Update ("LabelSet" :- Set.Set Label) ts HList,
  Update ("Var" :- Id) ts HList,
  Update ("RHSTerm" :- Fix e EXPR) ts HList, 
  MonadReader (HList ts) m,
  MonadWriter PatProp m,
  MonadIO m, Fail.MonadFail m) =>
  AccessForm -> Fix ComplexPatSig ComplexPat -> m (JudgedMatchChain e)
patExpansion delta p = do
  le <- select @"RHSId" @(MatchId) <$> ask
  l <- select @"MatchId" @(MatchId) <$> ask
  rhs <- select @"RHSTerm" @(Fix e EXPR) <$> ask
  runCaseOn p [
    patCase @MatchAllPat (\case
      _ -> do
        -- get fresh name and update counter
        freshId <- genFreshId "#a"
        let newChain = iAMatchChainList l delta 
              (iIdPat freshId :: Fix SimplePatSig SimplePat, iARHSExpr le rhs)
        return $ zip [Var, Var] (matchChainToList newChain)
    ),
    patCase @Pat (\case
      IdPat v -> do
        let newChain = iAMatchChainList l delta 
              (iIdPat v :: Fix SimplePatSig SimplePat, iARHSExpr le rhs)
        return $ zip [Var, Var] (matchChainToList newChain)
      LabelPat label -> do
        let newChain = iAMatchChainList l delta 
              (iLabelPat label :: Fix SimplePatSig SimplePat, iARHSExpr le rhs)
        tell NonVar
        return $ zip [NonVar, Var] (matchChainToList newChain)
    ),
    patCase @AppPat (\case
      -- l pi
      AppPat label p -> do
        freshId <- runCaseOn p [
          patCase @Pat (\case
            IdPat v -> return v
            _ -> genFreshId "#a"
          ),
          byDefault $ genFreshId "#a"]
        let updateCxt = 
              HL.modify @"MatchId" @(MatchId) (++ [0]) .
              HL.modify @"Var" @(Id) (const freshId) .
              HL.modify @"LabelSet" @(Set.Set Label) (const Set.empty)
        chain <- runCaseOn p [
          patCase @Pat (\case
            IdPat _ -> return $ zip [Var] (matchChainToList (iARHSExpr le rhs))
            _ -> local updateCxt (patExpansion (IAccess freshId) p)
          ),
          patCase @MatchAllPat (const . return $
            zip [Var] (matchChainToList (iARHSExpr le rhs))
          ),
          byDefault $ local updateCxt (patExpansion (IAccess freshId) p)]
        tell NonVar
        return $ (NonVar, MatchChainList delta (
          iAppIdPat label freshId :: Fix SimplePatSig SimplePat, K ()) :&: l) : chain
    ),
    patCase @RecordPat (\case
      RecordPat ps -> case ps of
        -- Match all record
        [] -> do
          -- get fresh name and update counter
          freshId <- genFreshId "#a"
          let newChain = iAMatchChainList l (toRAccess delta) 
                (iIdPat freshId :: Fix SimplePatSig SimplePat, iARHSExpr le rhs)
          return $ zip [Var, Var] (matchChainToList newChain)
        ((label, pattern) : xs) -> do
          x <- select @"Var" @(Id) <$> ask
          case delta of 
            IAccess v | v == x -> do
              labelSet <- select @"LabelSet" @(Set.Set Label) <$> ask
              -- check if label is in label set
              if Set.member label labelSet
              then Fail.fail ("Error: label duplication: " ++ (show label))
              else case xs of
                [] -> patExpansion (IFAccess v label) pattern
                _ -> do
                  let newlabelSet = Set.insert label labelSet
                      l' = init l ++ [last l + 1]
                      updateCxt = 
                        HL.modify @"MatchId" @(MatchId) (const l') .
                        HL.modify @"LabelSet" @(Set.Set Label) (const newlabelSet)
                  chain1 <- local updateCxt (patExpansion (IAccess v) (iRecordPat xs))
                  chain2 <- patExpansion (IFAccess v label) pattern
                  return (init chain2 ++ chain1)
            _ -> do
              -- get fresh name and update counter
              freshId <- genFreshId "#a"
              let updateCxt = 
                    HL.modify @"MatchId" @(MatchId) (++ [0]) .
                    HL.modify @"LabelSet" @(Set.Set Label) (const Set.empty) .
                    HL.modify @"Var" @(Id) (const freshId)
              (chain, prop) <- listen (local updateCxt (patExpansion (IAccess freshId) (iRecordPat ps)))
              return $ (prop, MatchChainList (toRAccess delta) (
                iIdPat freshId :: Fix SimplePatSig SimplePat, K ()) :&: l) : chain
    )]
  where
    patCase :: forall f y. (f :<: ComplexPatSig) => 
      (f (Fix ComplexPatSig) ComplexPat -> y) -> Fix ComplexPatSig ComplexPat -> Maybe y
    patCase = flip lCase

matchChainReversing :: (t ~ [TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel]) => (t, t) -> t
matchChainReversing ([], chainAccum) = chainAccum
matchChainReversing ((c : chain), chainAccum) = case c of
  RHSExpr rhs :&: i -> chainAccum
  MatchChainList a (p, K ()) :&: i -> matchChainReversing (chain, (c : chainAccum))

matchChainGrouping :: (t ~ [TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel]) =>
  (JudgedMatchChain e, t) -> t
matchChainGrouping (tchain, chain) = case tchain of 
  [] -> (error "empty chainT")
  (prop, c) : xs -> case prop of
    Var -> case c of
      RHSExpr rhs :&: i -> matchChainReversing (chain, [c])
      MatchChainList a (p, K ()) :&: i -> matchChainGrouping (xs, (c : chain))
    NonVar -> c : (matchChainGrouping (xs, chain))

instance (HTraversable e, ContainFV e) => ContainFV (TaggedMatchChainSig e SimplePatSig SimplePat) where
  fvAlg (MatchChainList a (p, c) :&: _) = K $ case cataM pvAlg p of
            Just pv -> Set.insert (accessId a) (Set.difference (unK c) (unK pv))
            Nothing -> error "impossible"
  fvAlg (MatchChainTree a bs :&: _) = K $ Set.insert (accessId a) (fold (map process bs))
    where process (p, b) = case cataM pvAlg p of
            Just pv -> Set.difference (unK b) (unK pv)
            Nothing -> error "impossible"
  fvAlg (RHSExpr e :&: _) = K $ unK (cata fvAlg e)

instance (Update ("Subst" :- Subst (Fix e EXPR)) ts HList,
  MonadIO m, Occurs ("NameCounter" :- IORef Int) ts HList,
  MonadReader (HList ts) m, Fail.MonadFail m, HTraversable e, Substitutable e e m,
  (TaggedMatchChainSig e SimplePatSig SimplePat) ~ g, Expr :<: e, ContainFV e) => 
  Substitutable (TaggedMatchChainSig e SimplePatSig SimplePat) g m where
  substAlg (MatchChainList a (p, c) :&: mcid) = Compose $ do
    subst <- select @"Subst" @(Subst (Fix e EXPR)) <$> ask
    let ia = accessId a
    a' <- case Map.lookup ia subst of
      Just s -> case project s :: Maybe (Expr (Fix e) EXPR) of
        Just (IdExpr i) -> return (accessSubst (Map.singleton ia i) a)
        _ -> Fail.fail "impossible"
      Nothing -> return a
    let fv = fold (Map.map (unK . cata fvAlg) subst)
    (K pv) <- cataM pvAlg p
    case Set.size pv of
      0 -> do
        c' <- getCompose c
        return $ iAMatchChainList mcid a' (p, c')
      1 -> do
        let v = head (Set.toList pv)
        if Set.member v fv then do
          freshId <- genFreshId "#x"
          let updateCxt = Map.insert v (iIdExpr freshId)
          c' <- local (HL.modify @"Subst" @(Subst (Fix e EXPR)) updateCxt) (getCompose c)
          let cxtPV :: HList '["SubstPV" :- Subst Id]
              cxtPV = Field (Map.singleton v freshId) :| HNil
              p' :: Fix SimplePatSig SimplePat
              p' = flip runReader cxtPV . getCompose $ cata substpvAlg p
          return $ iAMatchChainList mcid a' (p', c')
        else do
          let updateCxt = Map.delete v
          c' <- local (HL.modify @"Subst" @(Subst (Fix e EXPR)) updateCxt) (getCompose c)
          return $ iAMatchChainList mcid a' (p, c')
      _ -> Fail.fail "impossible"
  substAlg (MatchChainTree a bs :&: mtid) = Compose $ do
    subst <- select @"Subst" @(Subst (Fix e EXPR)) <$> ask
    let ia = accessId a
    a' <- case Map.lookup ia subst of
      Just s -> case project s :: Maybe (Expr (Fix e) EXPR) of
        Just (IdExpr i) -> return (accessSubst (Map.singleton ia i) a)
        _ -> Fail.fail "impossible"
      Nothing -> return a
    let fv = fold (Map.map (unK . cata fvAlg) subst)
    let process (p, b) = do
          (K pv) <- cataM pvAlg p
          case Set.size pv of
            0 -> do
              b' <- getCompose b
              return $ (p, b')
            1 -> do
              let v = head (Set.toList pv)
              if Set.member v fv then do
                freshId <- genFreshId "#x"
                let updateCxt = Map.insert v (iIdExpr freshId)
                b' <- local (HL.modify @"Subst" @(Subst (Fix e EXPR)) updateCxt) (getCompose b)
                let cxtPV :: HList '["SubstPV" :- Subst Id]
                    cxtPV = Field (Map.singleton v freshId) :| HNil
                    p' :: Fix SimplePatSig SimplePat
                    p' = flip runReader cxtPV . getCompose $ cata substpvAlg p
                return (p', b')
              else do
                let updateCxt = Map.delete v
                b' <- local (HL.modify @"Subst" @(Subst (Fix e EXPR)) updateCxt) (getCompose b)
                return (p, b')
            _ -> Fail.fail "impossible"
    bs' <- mapM process bs
    return $ iAMatchChainTree mtid a' bs'
  substAlg (RHSExpr e :&: rhsid) = Compose $ do
    e' <- getCompose (cata substAlg e) :: m (Fix e EXPR)
    return $ iARHSExpr rhsid e'

selectCandidate :: (t ~ Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel) =>
  AccessForm -> MatchId -> t -> t
selectCandidate a m mc = head $ selectRun a m mc id []
  where
    selectRun :: (t ~ Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel) =>
      AccessForm -> MatchId -> t -> (t -> t) -> [t] -> [t]
    selectRun a m mc f c = case (unTerm mc) of
      MatchChainList a' (p, mc') :&: m' | let cons = (\mc -> iAMatchChainList m' a' (p, mc)) ->
        if length m == length m' then case accessMerge a a' of
          Just _ -> [(cons . f) mc']
          Nothing -> selectRun a m mc' (f . cons) (c ++ [(cons . f) mc'])
        else
          selectRun a m mc' (f . cons) c
      RHSExpr e :&: m' -> c ++ [f (iARHSExpr m' e)]

mergePat :: (Fail.MonadFail m) => Fix SimplePatSig SimplePat -> Fix SimplePatSig SimplePat -> m (Subst Id)
mergePat a b = caseH
  (\case
    IdPat ia -> caseH (\case
      IdPat ib -> return (Map.singleton ia ib)
      _ -> Fail.fail "cannot merge"
      ) (const $ Fail.fail "cannot merge") (unTerm b)
    LabelPat la -> caseH (\case
      LabelPat lb | la == lb -> return Map.empty
      _ -> Fail.fail "cannot merge"
      ) (const $ Fail.fail "cannot merge") (unTerm b)
  )
  (\case
    AppIdPat la ia -> caseH (const $ Fail.fail "cannot merge") (\case
      AppIdPat lb ib | la == lb -> return (Map.singleton ia ib)
      _ -> Fail.fail "cannot merge"
      ) (unTerm b)
  ) (unTerm a)

patProp :: Fix SimplePatSig SimplePat -> PatProp
patProp p = caseH (\case
  IdPat _ -> Var
  LabelPat _ -> NonVar
  ) (const NonVar) (unTerm p)

patToExpr :: (Expr :<: e, LabelExpr LabelAsFun :<: e, RecordOps :<: e) => Fix SimplePatSig SimplePat -> Fix e EXPR
patToExpr p = caseH (\case
  IdPat i -> iIdExpr i
  LabelPat l -> iLabelApp l (iRecordCons [])
  ) (\case
  AppIdPat l i -> iLabelApp l (iIdExpr i)
  ) (unTerm p)

patId :: (Fail.MonadFail m) => Fix SimplePatSig SimplePat -> m Id
patId p = caseH (\case
  IdPat i -> return i
  LabelPat _ -> Fail.fail "no identifier found"
  ) (\case
  AppIdPat _ i -> return i
  ) (unTerm p)

matchChainListToTree :: Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel ->
  Fix (TaggedMatchChainSig e SimplePatSig SimplePat) TreeModel
matchChainListToTree mc = case unTerm mc of
  MatchChainList a (p, cs) :&: m -> iAMatchChainTree m a [(p, matchChainListToTree cs)]
  RHSExpr e :&: m -> iARHSExpr m e

getRHSId :: Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel -> MatchId
getRHSId mc = case unTerm mc of
  RHSExpr _ :&: m -> m
  MatchChainList _ (_, cs) :&: _ -> getRHSId cs

matchChainMerging :: forall e ts ts' m. (Update ("Subst" :- Subst (Fix e EXPR)) ts HList,
  MonadIO m, Occurs ("NameCounter" :- IORef Int) ts HList,
  MonadReader (HList ts) m, Fail.MonadFail m, HTraversable e, Substitutable e e m, 
  Expr :<: e, ContainFV e, LabelExpr LabelAsFun :<: e, RecordOps :<: e, 
  Update ("RHSOccur" :- Map.Map MatchId Int) ts' HList,
  MonadState (HList ts') m) =>
  Fix (TaggedMatchChainSig e SimplePatSig SimplePat) TreeModel ->
  Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel ->
  m (Fix (TaggedMatchChainSig e SimplePatSig SimplePat) TreeModel)
matchChainMerging mt mc = case unTerm mt of
  MatchChainTree mta bs :&: mtid -> case unTerm (selectCandidate mta mtid mc) of
    candidate@(MatchChainList mca (mcp, mcs) :&: mcid) | length mtid == length mcid -> 
      case accessMerge mta mca of
        Just a -> case patProp mcp of
          NonVar -> do
            let process [] = do
                  MS.modify (HL.modify @"RHSOccur" @(Map.Map MatchId Int) (Map.adjust (+ 1) (getRHSId mcs)))
                  return [(mcp, matchChainListToTree mcs)]
                process ((p, rhs) : rest) = case mergePat mcp p of
                  Just s -> do
                    let vs = Map.map iIdExpr s :: Subst (Fix e EXPR)
                    mcs' <- local (HL.modify @"Subst" @(Subst (Fix e EXPR)) (const vs)) 
                      (getCompose $ cata substAlg mcs)
                    ((: rest) . (p,)) <$> matchChainMerging rhs mcs'
                  Nothing -> case patProp p of
                    Var -> if length rest /= 0 then Fail.fail "impossible" else do
                      pi <- patId p
                      rhs' <- matchChainMerging rhs (iAMatchChainList mcid (IAccess pi) (mcp, mcs))
                      return [(p, rhs')]
                    NonVar -> ((p, rhs) :) <$> process rest
            bs' <- process bs
            return $ iAMatchChainTree mtid a bs'
          Var -> do
            mcpi <- patId mcp
            let process (p, rhs) = do
                  let vs = Map.singleton mcpi (patToExpr p) :: Subst (Fix e EXPR)
                  mcs' <- local (HL.modify @"Subst" @(Subst (Fix e EXPR)) (const vs)) 
                    (getCompose $ cata substAlg mcs)
                  (p,) <$> matchChainMerging rhs mcs'
            bs' <- mapM process bs
            case patProp (fst $ last bs) of
              Var -> return $ iAMatchChainTree mtid a bs'
              NonVar -> do
                MS.modify (HL.modify @"RHSOccur" @(Map.Map MatchId Int) (Map.adjust (+ 1) (getRHSId mcs)))
                return $ iAMatchChainTree mtid a (bs' ++ [(mcp, matchChainListToTree mcs)])
        Nothing -> do
          freshId <- genFreshId "#x"
          matchChainMerging mt (iAMatchChainList mtid mta (iIdPat freshId, Term candidate))
    candidate -> do
      freshId <- genFreshId "#x"
      matchChainMerging mt (iAMatchChainList mtid mta (iIdPat freshId, Term candidate))
  rhs -> return $ Term rhs

matchChainToExpr :: (Expr :<: e, RecordOps :<: e, Match SimplePatSig SimplePat :<: e, RHS MatchId :<: e) => 
  Fix (TaggedMatchChainSig e SimplePatSig SimplePat) TreeModel -> Fix e EXPR
matchChainToExpr mc = case unTerm mc of
  MatchChainTree a bs :&: _ -> iMatch (accessToAST a) (map (second matchChainToExpr) bs)
  RHSExpr e :&: m -> iRHS m e

type Matched = OMap.OMap Id (Either (Fix SimplePatSig SimplePat) [(Label, Fix SimplePatSig SimplePat)])

accessLabel :: AccessForm -> Maybe Label
accessLabel (IAccess _) = Nothing
accessLabel (IFAccess _ l) = Just l
accessLabel (IFRAccess _ l) = Just l
accessLabel (IRAccess _) = Nothing

matchChainRefining :: forall e ts m. (Update ("Subst" :- Subst (Fix e EXPR)) ts HList,
  Update ("Matched" :- Matched) ts HList,
  MonadIO m, Occurs ("NameCounter" :- IORef Int) ts HList,
  MonadReader (HList ts) m, Fail.MonadFail m, HTraversable e, Substitutable e e m, 
  Expr :<: e, ContainFV e, LabelExpr LabelAsFun :<: e, RecordOps :<: e) =>
  Fix (TaggedMatchChainSig e SimplePatSig SimplePat) TreeModel ->
  m (Fix (TaggedMatchChainSig e SimplePatSig SimplePat) TreeModel)
matchChainRefining mt = case unTerm mt of
  MatchChainTree mta bs :&: mtid -> do
    matched <- select @"Matched" @Matched <$> ask
    let x = accessId mta
        process (p, rhs) = case accessLabel mta of
          Just l -> case OMap.lookup x matched of
            Just r -> case r of
              Left _ -> Fail.fail "impossible"
              Right ps -> (p,) <$> local (HL.modify  @"Matched" @Matched (OMap.|> (x, Right ((l, p) : ps)))) 
                (matchChainRefining rhs)
            Nothing -> (p,) <$> local (HL.modify  @"Matched" @Matched (OMap.|> (x, Right [(l, p)]))) 
              (matchChainRefining rhs)
          Nothing -> (p,) <$> local (HL.modify  @"Matched" @Matched (OMap.|> (x, Left p))) 
            (matchChainRefining rhs)
    bs' <- mapM process bs
    return $ iAMatchChainTree mtid mta bs'
  RHSExpr e :&: rhsid -> do
    matched <- select @"Matched" @Matched <$> ask
    let process acc (x, ps) = do
          let x' = case ps of
                Left p -> patToExpr p
                Right fs -> iRecordExt (foldr (\(l, _) acc -> iFieldRemove acc l) (iIdExpr x) fs) (map (second patToExpr) fs)
              s = Map.singleton x x' :: Subst (Fix e EXPR)
          local (HL.modify @"Subst" @(Subst (Fix e EXPR)) (const s)) (getCompose $ cata substAlg acc)
    e' <- foldM process e (OMap.assocs matched)
    return $ iARHSExpr rhsid e'

class PatElab f g m where
  patElabAlg :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''PatElab])

instance {-# OVERLAPPABLE #-} (Monad m, HTraversable f, f :<: g) => PatElab f g m where
  patElabAlg = Compose . fmap inject . hmapM getCompose

instance {-# OVERLAPPABLE #-} (MonadIO m, Occurs ("NameCounter" :- IORef Int) ts HList,
  MonadReader (HList ts) m, Fail.MonadFail m, 
  Update ("MatchCounter" :- Int) ts' HList,
  Update ("RHSOccur" :- Map.Map MatchId Int) ts' HList,
  MonadState (HList ts') m,
  Match SimplePatSig SimplePat :<: g, RHS MatchId :<: g, HTraversable g,
  Substitutable g g (ReaderT (HList 
    '["NameCounter" :- IORef Int, "Subst" :- Subst (Fix g EXPR)]) m),
  Substitutable g g (ReaderT (HList 
    '["NameCounter" :- IORef Int, "Subst" :- Subst (Fix g EXPR), "Matched" :- Matched]) m), 
  Expr :<: g, ContainFV g, LabelExpr LabelAsFun :<: g, RecordOps :<: g) => 
  PatElab (Match ComplexPatSig ComplexPat) g m where
  patElabAlg (Match e []) = Compose $ do
    e' <- getCompose e
    return $ iMatch e' ([] :: [(Fix SimplePatSig SimplePat, Fix g EXPR)])
  patElabAlg (Match e cs) = Compose $ do
    matchCounter <- HL.select @"MatchCounter" @Int <$> MS.get
    MS.modify (HL.modify @"MatchCounter" @Int (+ 1))
    freshId <- genFreshId "#x"
    let placeholder = IAccess freshId
    nameCounter <- HL.select @"NameCounter" @(IORef Int) <$> ask
    let eachCase rhsCounter (p, rhs) = do
          cataM pvAlg p
          rhs' <- getCompose rhs
          let rhsId = [matchCounter, rhsCounter]
              cxt :: HList '["NameCounter" :- IORef Int, "RHSId" :- MatchId, "MatchId" :- MatchId,
                             "LabelSet" :- Set.Set Label, "Var" :- Id, "RHSTerm" :- Fix g EXPR]
              cxt = Field nameCounter :| Field rhsId :| Field [0] :| 
                    Field Set.empty :| Field (strId "") :| Field rhs' :| HNil
          expanded <- (fst <$>) . runWriterT . flip runReaderT cxt $ patExpansion @g placeholder p
          MS.modify (HL.modify @"RHSOccur" @(Map.Map MatchId Int) (Map.insert rhsId 0))
          return $ (listToMatchChain (matchChainGrouping (expanded, [])), rhsId)
        process (rhsCounter, acc) now = do
          (now', _) <- eachCase rhsCounter now
          let cxt :: HList '["NameCounter" :- IORef Int, "Subst" :- Subst (Fix g EXPR)]
              cxt = Field nameCounter :| Field Map.empty :| HNil
          acc' <- flip runReaderT cxt $ matchChainMerging @g acc now'
          return (rhsCounter + 1, acc')
    (c', firstRHSId) <- first matchChainListToTree <$> eachCase 0 (head cs)
    MS.modify (HL.modify @"RHSOccur" @(Map.Map MatchId Int) (Map.adjust (+ 1) firstRHSId))
    (_, cs') <- foldM process (1, c') (tail cs)
    rhsOccur <- HL.select @"RHSOccur" @(Map.Map MatchId Int) <$> get
    if Map.filter (== 0) rhsOccur /= Map.empty then Fail.fail "unused RHS"
    else do
      let cxt :: HList '["NameCounter" :- IORef Int, "Subst" :- Subst (Fix g EXPR), "Matched" :- Matched]
          cxt = Field nameCounter :| Field Map.empty :| Field OMap.empty :| HNil
      rcs' <- flip runReaderT cxt $ matchChainRefining @g cs'
      e' <- getCompose e
      let er = case unTerm cs' of
            MatchChainTree (IRAccess _) _ :&: _ -> iRecordMod e' []
            MatchChainTree (IFRAccess _ _) _ :&: _ -> iRecordMod e' []
            _ -> e'
      case project (matchChainToExpr rcs') :: Maybe (Match SimplePatSig SimplePat (Fix g) EXPR) of
       Just (Match _ cs') -> return $ iMatch er cs'
       Nothing -> Fail.fail "impossible"

class RHSCount f g m where
  rhsCountAlg :: Alg f (Compose m (Fix g))

$(derive [liftSum] [''RHSCount])

instance {-# OVERLAPPABLE #-} (Monad m, HTraversable f, f :<: g) => RHSCount f g m where
  rhsCountAlg = Compose . fmap inject . hmapM getCompose

instance {-# OVERLAPPABLE #-} (Fail.MonadFail m, 
  Update ("RHSOccur" :- Map.Map MatchId Int) ts' HList,
  MonadState (HList ts') m, RHS MatchId :<: g) => RHSCount (RHS MatchId) g m where
  rhsCountAlg (RHS i e) = Compose $ do
    e' <- getCompose e
    MS.modify (HL.modify @"RHSOccur" @(Map.Map MatchId Int) (Map.adjust (+ 1) i))
    return $ iRHS i e'

