{-# LANGUAGE TupleSections, QuasiQuotes, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs, TypeApplications, RankNTypes #-}

-- variable | label [variable]
module PatternElaboration where

import Data.Maybe
import Data.Either
import Numeric.Natural
import Text.RawString.QQ
import qualified Data.Vec.Lazy as VL
import Data.Type.Nat hiding (toNatural, cata)
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.Fail as Fail
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

data AccessForm = IAccess Id | IFAccess Id Label | IFRAccess Id Label | IRAccess Id deriving (Eq, Ord, Show)

toRAccess :: AccessForm -> AccessForm
toRAccess (IAccess i) = IRAccess i
toRAccess (IFAccess i l) = IFRAccess i l
toRAccess a = a

accessId :: AccessForm -> Id
accessId (IAccess i) = i
accessId (IFAccess i _) = i
accessId (IFRAccess i _) = i
accessId (IRAccess i) = i

accessMerge :: AccessForm -> AccessForm -> Maybe AccessForm
accessMerge = undefined

accessToAST :: (Expr :<: e, RecordOps :<: e) => AccessForm -> Fix e EXPR
accessToAST (IAccess i) = iIdExpr i
accessToAST (IFAccess i l) = iFieldAccess (iIdExpr i) l
accessToAST (IFRAccess i l) = iRecordMod (iFieldAccess (iIdExpr i) l) []
accessToAST (IRAccess i) = iRecordMod (iIdExpr i) []

type MatchId = [Natural]

type RHSId = MatchId

-- type ComplexPatSig = Pat :+: AppPat :+: RecordPat :+: MatchAllPat
type ComplexPatSig = RecordPat :+: AppPat :+: Pat :+: MatchAllPat

type ComplexMatch = Match ComplexPatSig ComplexPat

type SimplePatSig = Pat :+: AppPat

type SimpleMatch = Match SimplePatSig SimplePat

data ListModel

data TreeModel

data RHSExpr :: ((* -> *) -> * -> *) -> (* -> *) -> * -> * where
  RHSExpr :: Fix e EXPR -> RHSExpr e self m

data MatchChain :: ((* -> *) -> * -> *) -> * -> (* -> *) -> * -> * where
  MatchChainList :: AccessForm -> 
    (Fix p l, self ListModel) -> MatchChain p l self ListModel
  MatchChainTree :: AccessForm -> 
    VL.Vec (S n) (Fix p l, self TreeModel) -> MatchChain p l self TreeModel

type MatchChainSig e p l = RHSExpr e :+: MatchChain p l

data PatProp = NonVar | Var deriving (Show, Eq, Ord)

type TaggedMatchChainSig e p l = (RHSExpr e :&: RHSId) :+: (MatchChain p l :&: MatchId)

type JudgedMatchChain e = (Int, [(PatProp, TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel)])

$(derive [makeHFunctor, makeHFoldable, makeHTraversable][''MatchChain, ''RHSExpr])

$(derive [smartConstructors, smartAConstructors][''MatchChain, ''RHSExpr])

instance (ShowHF a_anTR, HFunctor a_anTR) => ShowHF (MatchChain a_anTR b_anTS) where
  showHF (MatchChainList x_aogN x_aogO)
    = (K $ (showConstr
              "MatchChainList")
              [show x_aogN, show (second unK x_aogO)])
  showHF (MatchChainTree x_aogP x_aogQ)
    = (K $ (showConstr
              "MatchChainTree")
              [show x_aogP, show $ VL.map (second unK) x_aogQ])

instance (ShowHF a_anSL, HFunctor a_anSL) => ShowHF (RHSExpr a_anSL) where
  showHF (RHSExpr x_aogU)
    = (K $ (showConstr "RHSExpr") [show x_aogU])

lCase :: forall g i f y. (f :<: g) => Fix g i -> (f (Fix g) i -> y) -> Maybe y
lCase x f = case project x :: Maybe (f (Fix g) i) of
  Just x' -> Just (f x')
  Nothing -> Nothing

runCase :: [Maybe a] -> a
runCase ls = case [x | Just x <- ls] of
  (a : _) -> a
  _ -> error "Unexpected Case"

runCaseOn :: a -> [a -> Maybe b] -> b
runCaseOn a = runCase . map ($ a)

matchChainToList :: 
  Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel ->
  [TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel]
matchChainToList mc = caseH (return . inj . hfmap (const (K ()))) (\case
    (MatchChainList a (p, xs) :&: ma) -> inj (MatchChainList a (p, K ()) :&: ma) : matchChainToList xs
  ) (unTerm mc)

instance (ShowHF e, HFunctor e) => Show (TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel) where
  show m = caseH (\((RHSExpr rhs) :&: i) -> "RHSExpr " ++ (show rhs) ++ (show i)) (\case
    ((MatchChainList a (p, K ())) :&: i) -> "MatchChainList " ++ (show a) ++ " " ++ (show p) ++ (show i)) m

{- PATTERN EXPANSION -}
patExpansion :: forall ts e m. (MonadState MatchId m,
  Occurs ("NameCounter" :- IORef Int) ts HList,
  Update ("RHSId" :- MatchId) ts HList,
  {- Update ("MatchId" :- MatchId) ts HList,-}
  Update ("LabelSet" :- Set.Set Label) ts HList,
  Update ("Var" :- Id) ts HList,
  Update ("RHSTerm" :- Fix e EXPR) ts HList, 
  MonadReader (HList ts) m,
  MonadIO m, Fail.MonadFail m) =>
  AccessForm -> Fix ComplexPatSig ComplexPat -> m (Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel)
patExpansion delta p = runCase [
  patCase @MatchAllPat p (\case
    _ -> do
      -- get fresh name and update counter
      counter <- select @"NameCounter" @(IORef Int) <$> ask
      freshName <- liftIO $ (("#a" ++) . show) <$> readIORef counter
      liftIO $ modifyIORef counter (+ 1)
      le <- select @"RHSId" @(MatchId) <$> ask
      -- l <- select @"MatchId" @(MatchId) <$> ask
      l <- get
      rhs <- select @"RHSTerm" @(Fix e EXPR) <$> ask
      return $ iAMatchChainList l delta (iIdPat freshName :: Fix SimplePatSig SimplePat, iARHSExpr le rhs)
  ),
  patCase @Pat p (\case
    IdPat v -> do
      le <- select @"RHSId" @(MatchId) <$> ask
      -- l <- select @"MatchId" @(MatchId) <$> ask
      l <- get
      rhs <- select @"RHSTerm" @(Fix e EXPR) <$> ask
      return $ iAMatchChainList l delta (iIdPat v :: Fix SimplePatSig SimplePat, iARHSExpr le rhs)
    LabelPat label -> do
      le <- select @"RHSId" @(MatchId) <$> ask
      -- l <- select @"MatchId" @(MatchId) <$> ask
      l <- get
      rhs <- select @"RHSTerm" @(Fix e EXPR) <$> ask
      return $ iAMatchChainList l delta (iLabelPat label :: Fix SimplePatSig SimplePat, iARHSExpr le rhs)
  ),
  patCase @AppPat p (\case
    -- l v
    AppIdPat label v -> do
      le <- select @"RHSId" @(MatchId) <$> ask
      -- l <- select @"MatchId" @(MatchId) <$> ask
      l <- get
      rhs <- select @"RHSTerm" @(Fix e EXPR) <$> ask
      return $ iAMatchChainList l delta (iAppIdPat label v :: Fix SimplePatSig SimplePat, iARHSExpr le rhs)
    -- l pi
    AppPat label p -> 
      runCase [
        patCase @Pat p (\case
          _ -> do
            -- get fresh name and update counter
            counter <- select @"NameCounter" @(IORef Int) <$> ask
            freshName <- liftIO $ (("#a" ++) . show) <$> readIORef counter
            liftIO $ modifyIORef counter (+ 1)
            le <- select @"RHSId" @(MatchId) <$> ask
            -- l <- select @"MatchId" @(MatchId) <$> ask
            l <- get
            let l' = l ++ [0]
            put l'
            cxt <- {- HL.modify @"MatchId" @(MatchId) (\l -> l') <$> -} HL.modify @"Var" @(Id) (\a -> freshName) <$> HL.modify @"LabelSet" @(Set.Set Label) (\s -> Set.empty) <$> ask
            chain <- local (\r -> cxt) (patExpansion (IAccess freshName) p)
            return $ iAMatchChainList l delta (iAppIdPat label freshName :: Fix SimplePatSig SimplePat, chain)
        ),
        patCase @AppPat p (\case
          _ -> do
            -- get fresh name and update counter
            counter <- select @"NameCounter" @(IORef Int) <$> ask
            freshName <- liftIO $ (("#a" ++) . show) <$> readIORef counter
            liftIO $ modifyIORef counter (+ 1)
            le <- select @"RHSId" @(MatchId) <$> ask
            -- l <- select @"MatchId" @(MatchId) <$> ask
            l <- get
            let l' = l ++ [0]
            put l'
            cxt <- {- HL.modify @"MatchId" @(MatchId) (\l -> l') <$> -} HL.modify @"Var" @(Id) (\a -> freshName) <$> HL.modify @"LabelSet" @(Set.Set Label) (\s -> Set.empty) <$> ask
            chain <- local (\r -> cxt) (patExpansion (IAccess freshName) p)
            return $ iAMatchChainList l delta (iAppIdPat label freshName :: Fix SimplePatSig SimplePat, chain)
        ),
        patCase @RecordPat p (\case
          _ -> do
            -- get fresh name and update counter
            counter <- select @"NameCounter" @(IORef Int) <$> ask
            freshName <- liftIO $ (("#a" ++) . show) <$> readIORef counter
            liftIO $ modifyIORef counter (+ 1)
            le <- select @"RHSId" @(MatchId) <$> ask
            -- l <- select @"MatchId" @(MatchId) <$> ask
            l <- get
            let l' = l ++ [0]
            put l'
            cxt <- {- HL.modify @"MatchId" @(MatchId) (\l -> l') <$> -} HL.modify @"Var" @(Id) (\a -> freshName) <$> HL.modify @"LabelSet" @(Set.Set Label) (\s -> Set.empty) <$> ask
            chain <- local (\r -> cxt) (patExpansion (IAccess freshName) p)
            return $ iAMatchChainList l delta (iAppIdPat label freshName :: Fix SimplePatSig SimplePat, chain)
        ),
        -- l MatchAllPat
        patCase @MatchAllPat p (\case
          _ -> do
            -- get fresh name and update counter
            counter <- select @"NameCounter" @(IORef Int) <$> ask
            freshName <- liftIO $ (("#a" ++) . show) <$> readIORef counter
            liftIO $ modifyIORef counter (+ 1)
            le <- select @"RHSId" @(MatchId) <$> ask
            -- l <- select @"MatchId" @(MatchId) <$> ask
            l <- get
            rhs <- select @"RHSTerm" @(Fix e EXPR) <$> ask
            return $ iAMatchChainList l delta (iAppIdPat label freshName :: Fix SimplePatSig SimplePat, iARHSExpr le rhs)
        )]     
  ),
  patCase @RecordPat p (\case
    RecordPat ps -> case ps of
      -- Match all record
      [] -> do
        -- get fresh name and update counter
        counter <- select @"NameCounter" @(IORef Int) <$> ask
        freshName <- liftIO $ (("#a" ++) . show) <$> readIORef counter
        liftIO $ modifyIORef counter (+ 1)
        le <- select @"RHSId" @(MatchId) <$> ask
        -- l <- select @"MatchId" @(MatchId) <$> ask
        l <- get
        rhs <- select @"RHSTerm" @(Fix e EXPR) <$> ask
        return $ iAMatchChainList l (toRAccess delta) (iIdPat freshName :: Fix SimplePatSig SimplePat, iARHSExpr le rhs)
      ((label, pattern) : xs) -> case delta of 
        (IAccess v) -> do
          x <- select @"Var" @(Id) <$> ask
          if v == x
            then do
              case xs of
                [] -> do
                  labelSet <- select @"LabelSet" @(Set.Set Label) <$> ask
                  -- check if label is in label set
                  if Set.member label labelSet
                    then Fail.fail ("Error: label duplication: " ++ (show label))
                    else do
                      chain <- (patExpansion (IFAccess v label) pattern)
                      return chain
                _ -> do
                  labelSet <- select @"LabelSet" @(Set.Set Label) <$> ask
                  -- check if label is in label set
                  if Set.member label labelSet
                    then Fail.fail ("Error: label duplication: " ++ (show label))
                    else do
                      let newlabelSet = Set.insert label labelSet
                      chain2 <- patExpansion (IFRAccess v label) pattern
                      le <- select @"RHSId" @(MatchId) <$> ask
                      -- l <- select @"MatchId" @(MatchId) <$> ask
                      l <- get
                      let l' = (Prelude.take (length l - 1) l) ++ [(last l + 1)]
                      put l'
                      cxt <- {- HL.modify @"MatchId" @(MatchId) (\l -> l') <$> -} HL.modify @"LabelSet" @(Set.Set Label) (\s -> newlabelSet) <$> ask
                      -- we can't change the order here cuz the match id need to be updated before generating chain1
                      chain1 <- local (\r -> cxt) (patExpansion (IAccess v) (iRecordPat xs))
                      return (replaceTail chain2 chain1)
            else do
              -- get fresh name and update counter
              counter <- select @"NameCounter" @(IORef Int) <$> ask
              freshName <- liftIO $ (("#a" ++) . show) <$> readIORef counter
              liftIO $ modifyIORef counter (+ 1)
              le <- select @"RHSId" @(MatchId) <$> ask
              -- l <- select @"MatchId" @(MatchId) <$> ask
              l <- get
              let l' = l ++ [0]
              put l'
              cxt <- {-HL.modify @"MatchId" @(MatchId) (\l -> l') <$> -} HL.modify @"LabelSet" @(Set.Set Label) (\s -> Set.empty) <$> HL.modify @"Var" @(Id) (\a -> freshName) <$> ask
              chain <- local (\r -> cxt) (patExpansion (IAccess freshName) (iRecordPat ps))
              return $ iAMatchChainList l (toRAccess delta) (iIdPat freshName :: Fix SimplePatSig SimplePat, chain)
        _ -> do
          -- get fresh name and update counter
          counter <- select @"NameCounter" @(IORef Int) <$> ask
          freshName <- liftIO $ (("#a" ++) . show) <$> readIORef counter
          liftIO $ modifyIORef counter (+ 1)
          le <- select @"RHSId" @(MatchId) <$> ask
          -- l <- select @"MatchId" @(MatchId) <$> ask
          l <- get
          let l' = l ++ [0]
          put l'
          cxt <- {- HL.modify @"MatchId" @(MatchId) (\l -> l') <$> -} HL.modify @"LabelSet" @(Set.Set Label) (\s -> Set.empty) <$> HL.modify @"Var" @(Id) (\a -> freshName) <$> ask
          chain <- local (\r -> cxt) (patExpansion (IAccess freshName) (iRecordPat ps))
          return $ iAMatchChainList l (toRAccess delta) (iIdPat freshName :: Fix SimplePatSig SimplePat, chain)
  )]
  where
    patCase :: forall f y. (f :<: ComplexPatSig) => 
      Fix ComplexPatSig ComplexPat -> (f (Fix ComplexPatSig) ComplexPat -> y) -> Maybe y
    patCase = lCase

replaceTail :: (t ~ Fix ((RHSExpr e :&: ra) :+: (MatchChain p l :&: ma)) ListModel) => t -> t -> t
replaceTail a b = caseH (const b) (\case
    (MatchChainList a (p, xs) :&: ma) -> inject (MatchChainList a (p, replaceTail xs b) :&: ma)
  ) (unTerm a)

{- MATCH CHAIN SORTING-}
matchChainTagging :: forall e. Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel -> JudgedMatchChain e
matchChainTagging chain = runCase [
  chainCase @(RHSExpr e :&: RHSId) chain (\case
    _ -> (0, [(Var, hfmap convert (unTerm chain))])
  ),
  chainCase @(MatchChain SimplePatSig SimplePat :&: MatchId) chain (\case
    MatchChainList delta1 (p1, xs) :&: l1 -> runCase [
      chainCase @(RHSExpr e :&: RHSId) xs (\case
        _ -> runCase [
          patCase @Pat p1 (\case
            -- Var v
            IdPat v -> ((length l1 - 1), [(Var, hfmap convert (unTerm chain))] ++ snd (matchChainTagging xs))
            -- NonVar l
            LabelPat label -> (0, [(NonVar, hfmap convert (unTerm chain))] ++ snd (matchChainTagging xs))
          ),
          patCase @AppPat p1 (\case
            -- NonVar (l v)
            _ -> (0, [(NonVar, hfmap convert (unTerm chain))] ++ snd (matchChainTagging xs))
          )]
      ),
      chainCase @(MatchChain SimplePatSig SimplePat :&: MatchId) xs (\case
        MatchChainList delta2 (p2, ys) :&: l2 -> runCase [
          patCase @Pat p2 (\case
            -- Var
            IdPat v -> 
              if l2 == l1 ++ [last l2]
                then case (matchChainTagging xs) of
                  (0, txs) -> (0, [(NonVar, hfmap convert (unTerm chain))] ++ txs) -- the special case regardless pattern property
                  (c, txs) -> (c - 1, [(Var, hfmap convert (unTerm chain))] ++ txs)
                else if (length l2) == (length l1) && (take (length l2 - 1) l2) == (take (length l1 - 1) l1)
                  then (fst (matchChainTagging xs), [(Var, hfmap convert (unTerm chain))] ++ snd (matchChainTagging xs))
                  else (fst (matchChainTagging xs) + (length l1 - length l2), [(Var, hfmap convert (unTerm chain))] ++ snd (matchChainTagging xs)) 
            -- NonVar
            LabelPat label -> (0, [(NonVar, hfmap convert (unTerm chain))] ++ snd (matchChainTagging xs))
          ),
          patCase @AppPat p2 (\case
            -- NonVar
            _ -> (0, [(NonVar, hfmap convert (unTerm chain))] ++ snd (matchChainTagging xs))
          )]
      )]
  )]
  where
    chainCase :: forall f y e. (f :<: TaggedMatchChainSig e SimplePatSig SimplePat) => 
      Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel -> (f (Fix (TaggedMatchChainSig e SimplePatSig SimplePat)) ListModel -> y) -> Maybe y
    chainCase = lCase
    patCase :: forall f y. (f :<: SimplePatSig) => 
      Fix SimplePatSig SimplePat -> (f (Fix SimplePatSig) SimplePat -> y) -> Maybe y
    patCase = lCase
    convert :: forall f i. Term f i -> K () i
    convert (Term f) = K ()

-- WIP
matchChainReversing :: forall e. (Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel, Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel) -> Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel
matchChainReversing (chain, chainAccum) = runCase [
  chainCase @(RHSExpr e :&: RHSId) chain (\case
    _ -> chainAccum
  ),
  -- (Match delta with <pi => chain>, chainAccum)
  chainCase @(MatchChain SimplePatSig SimplePat :&: MatchId) chain (\case
    -- (chain, Match delta with <pi => chainAccum>)
    MatchChainList delta (p, xs) :&: l -> matchChainReversing (xs, (exchange chain chainAccum))
  )]
  where
    chainCase :: forall f y e. (f :<: TaggedMatchChainSig e SimplePatSig SimplePat) => 
      Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel -> (f (Fix (TaggedMatchChainSig e SimplePatSig SimplePat)) ListModel -> y) -> Maybe y
    chainCase = lCase
    exchange :: (t ~ Fix ((RHSExpr e :&: ra) :+: (MatchChain p l :&: ma)) ListModel) => t -> t -> t
    exchange a b = caseH (const b) (\case
        (MatchChainList a (p, xs) :&: ma) -> inject (MatchChainList a (p, b) :&: ma)
      ) (unTerm a)

-- TODO match chain grouping
{- TESTING -}
astToAccess :: Fix ExprSig EXPR -> AccessForm
astToAccess expr = runCase [
  exprCase @Expr expr (\case
    IdExpr i -> IAccess i
    _ -> error "Unexpected expression"
  ),
  exprCase @RecordOps expr (\case
    FieldAccess e l -> runCase [
      exprCase @Expr e (\case
        IdExpr i -> IFAccess i l
        _ -> error "Unexpected expression"
      )]
    RecordMod e xs -> runCase [
      exprCase @Expr e (\case
        IdExpr i -> IRAccess i
        _ -> error "Unexpected expression"
      ),
      exprCase @RecordOps e (\case
        FieldAccess e1 l1 -> runCase [
          exprCase @Expr e1 (\case
            IdExpr i -> IFRAccess i l1
            _ -> error "Unexpected expression"
          )]
        _ -> error "Unexpected expression"
      )]
  )]
  where
    exprCase :: forall f y. (f :<: ExprSig) => 
      Fix ExprSig EXPR -> (f (Fix ExprSig) EXPR -> y) -> Maybe y
    exprCase = lCase

-- Test expansion
executePatternExpansion :: (MonadIO m, Fail.MonadFail m) => 
  Fix ExprSig EXPR -> Natural -> m ([Fix (TaggedMatchChainSig ExprSig SimplePatSig SimplePat) ListModel])
executePatternExpansion expr n = runCase [
  exprCase @(Match PatSig ComplexPat) expr (\case
    Match exp list -> case list of
      [] -> return []
      ((p, l) : xs) -> do
        c <- liftIO $ newIORef 0
        let cxt :: HList '["NameCounter" :- IORef Int, "RHSId" :- MatchId, "LabelSet" :- Set.Set Label, "Var" :- Id, "RHSTerm" :- Fix ExprSig EXPR]
            cxt = Field c :| Field [n] :| Field Set.empty :| Field "" :| Field l :| HNil
        tail <- (executePatternExpansion (iMatch exp xs) (n + 1))
        (flip evalStateT [n] . flip runReaderT cxt $ patExpansion (astToAccess exp) p) >>= (\x -> return (x : tail))
  ),
  exprCase @Expr expr (\case
    _ -> Fail.fail "Not a match expression"
  )]
  where
    exprCase :: forall f y. (f :<: ExprSig) => 
      Fix ExprSig EXPR -> (f (Fix ExprSig) EXPR -> y) -> Maybe y
    exprCase = lCase

-- Test tagging
executePatExpAndTag match = do 
  list <- (executePatternExpansion match 0)
  return (map matchChainTagging list)

patternElaboration :: Fix ExprSig EXPR -> Fix ExprSig EXPR
patternElaboration m = undefined

matchString1 :: String
matchString1 = [r|match exp with <
  {Snd: F | Trd: T} => 1 |
  {Fst: F | Snd: T} => 2 |
  {Trd: F} => 3 |
  {Trd: T} => 4
>|]

matchString2 :: String
matchString2 = [r|match x with <
  App {Fun: App {Fun: Primitive Map | Arg: f} | Arg: App {Fun: App {Fun: Primitive Map | Arg: g} | Arg: x}} => 
  Success (App {Fun: App {Fun: Primitive Map | Arg: Lam {Param: 0 | Body: App {Fun: f | Arg: App {Fun: g | Arg: Id {Name: 0}}}}} | Arg: x})
  | _ => Failure 1
>|]

matchExample1 :: Fix ExprSig EXPR
matchExample1 = head (rights [testRun match matchString1])

matchExample2 :: Fix ExprSig EXPR
matchExample2 = head (rights [testRun match matchString2])

{-
class PatExpansion f e p m where
  patExpansionAlg :: Alg f (Compose m (K (Fix (TaggedMatchChainSig e p SimplePat) ListModel, Maybe Id)))

instance (PatExpansion f e p m, PatExpansion h e p m) => PatExpansion ((:+:) f h) e p m where
  patExpansionAlg x = caseH patExpansionAlg patExpansionAlg x

instance (MonadState PEState m, MonadReader (PERead e) m, Pat :<: p) => PatExpansion MatchAllPat e p m where
  patExpansionAlg (MatchAllPat :&: a) = Compose $ do
    n <- getFreshNameId
    let freshName = "#a" ++ show (n :: Int)
    setFreshNameId (n + 1)
    (le, l) <- getIds
    (_, rhs) <- ask
    return . K $ (iAMatchChainList l a (iIdPat freshName :: Fix p SimplePat, iARHSExpr le rhs), Just freshName)

instance (MonadState PEState m, MonadReader (PERead e) m, Pat :<: p, AppPat :<: p) =>
  PatExpansion AppPat e p m where
  patExpansionAlg (AppPat label p :&: a) = Compose $ do

    K c <- getCompose p
    case snd c of
      Just v -> do
        (le, l) <- getIds
        (_, rhs) <- ask
        return . K $ (iAMatchChainList l a (iAppIdPat label v :: Fix p SimplePat, iARHSExpr le rhs), Nothing)
      Nothing -> return . K $ undefined
  patExpansionAlg _ = error "impossible"
-}
{-
patternExpansion delta (AppPattern label pattern) e = case pattern of
  -- Match label variable (l v)
  (IdPattern v) -> do
    (le, l) <- getIds
    return . Roll $ MatchChainList l delta (AppPattern label (IdPattern v),  Roll $ RHSTerm le e)
  -- Match label pattern (l pi)
  _ -> do
    n <- getFreshNameId
    let freshName = "#a" ++ show (n :: Int)
    -- update fresh name counter
    setFreshNameId (n + 1)
    (le, l) <- getIds
    let l' = l ++ [0]
    setIds (le, l')
    chain <- patternExpansion (TermId freshName) pattern e
    return . Roll $ MatchChainList l delta (AppPattern label (IdPattern freshName), chain)
-- The result will always be a match chain list model

patternExpansion :: (
  MonadState PEState m,
  MonadReader (PERead e) m
  ) => CoalgM m (MatchChainSig e SimplePatSig SimplePat) (Fix (MatchChainSig e ComplexPatSig ComplexPat))
-- MatchAllPattern

patternExpansion delta MatchAllPattern e = do
  n <- getFreshNameId
  let freshName = "#a" ++ show (n :: Int)
  -- update fresh name counter
  setFreshNameId (n + 1)
  (le, l) <- getIds
  return . Roll $ MatchChainList l delta (IdPattern freshName, Roll $ RHSTerm le e)
-- Match variable
patternExpansion delta (IdPattern v) e = do
  (le, l) <- getIds
  return . Roll $ MatchChainList l delta (IdPattern v, Roll $ RHSTerm le e)
-- Match label
patternExpansion delta (LabelPattern label) e = do
  (le, l) <- getIds
  return . Roll $ MatchChainList l delta (LabelPattern label, Roll $ RHSTerm le e)
-- Match app
patternExpansion delta (AppPattern label pattern) e = case pattern of
  -- Match label variable (l v)
  (IdPattern v) -> do
    (le, l) <- getIds
    return . Roll $ MatchChainList l delta (AppPattern label (IdPattern v),  Roll $ RHSTerm le e)
  -- Match label pattern (l pi)
  _ -> do
    n <- getFreshNameId
    let freshName = "#a" ++ show (n :: Int)
    -- update fresh name counter
    setFreshNameId (n + 1)
    (le, l) <- getIds
    let l' = l ++ [0]
    setIds (le, l')
    chain <- patternExpansion (TermId freshName) pattern e
    return . Roll $ MatchChainList l delta (AppPattern label (IdPattern freshName), chain)
-- Match record
patternExpansion delta (RecordPattern list) e = case list of
  -- Match all record
  [] -> do
    n <- getFreshNameId
    let freshName = "#a" ++ show (n :: Int)
    -- update fresh name counter
    setFreshNameId (n + 1)
    (le, l) <- getIds
    return . Roll $ MatchChainList l (RecordMod delta []) (IdPattern freshName,  Roll $ RHSTerm le e)
  ((label, pattern) : xs) -> case delta of 
    (TermId v) -> case xs of
      [] -> do
        labelSet <- getLabels
        -- TODO check if label is in label set
        let newlabelSet = insert label labelSet
        setLabels labelSet
        chain <- patternExpansion (FieldAccess (TermId v) label) pattern e
        return chain
      _ -> do
        labelSet <- getLabels
        -- TODO check if label is in label set
        let newlabelSet = insert label labelSet
        setLabels labelSet
        let tempTermId = "temp"
        chain2 <- patternExpansion (FieldAccess (TermId v) label) pattern (TermId tempTermId)
        (le, l) <- getIds
        let l' = (Prelude.take (length l - 1) l) ++ [(last l + 1)]
        setIds (le, l')
        chain1 <- patternExpansion (TermId v) (RecordPattern xs) e
        return (replaceTail chain2 chain1)
    _ -> do
      n <- getFreshNameId
      let freshName = "#a" ++ show (n :: Int)
      -- update fresh name counter
      setFreshNameId (n + 1)
      (le, l) <- getIds
      let l' = l ++ [0]
      setIds (le, l')
      chain <- patternExpansion (TermId freshName) (RecordPattern list) e
      return . Roll $ MatchChainList l (RecordMod delta []) (IdPattern freshName, chain)

replaceTail :: MatchChainList -> MatchChainList -> MatchChainList
replaceTail a b = 
  let (MatchChainList l delta (pattern, rhs)) = unroll a
  in case (unroll rhs) of
    (RHSTerm mid e) -> Roll (MatchChainList l delta (pattern, b))
    _ -> Roll (MatchChainList l delta (pattern, (replaceTail rhs b))) 

--testPtExp :: ReaderT Id (StateT (Set.Set Label) (StateT (MatchId, MatchId) (State Int))) MatchChainList
--testPtExp = patternExpansion (TermId "n") MatchAllPattern (TermId "rhs")

testExp = flip evalState (Set.empty, ([0], [5]), 10) . flip runReaderT "" $ patternExpansion (TermId "n") MatchAllPattern (TermId "rhs")
testExp2 = flip evalState (Set.empty, ([0], [5]), 10) . flip runReaderT "" $ patternExpansion (TermId "n") (AppPattern "l" (AppPattern "ll" (IdPattern "x"))) (TermId "rhs")

executePatternExpansion :: Term -> Natural -> [MatchChainList]
executePatternExpansion t n = case t of 
  Match delta list -> case list of
    [] -> []
    ((pattern, e) : xs) -> (flip evalState (Set.empty, ([n], [n]), 0) . flip runReaderT "" $ patternExpansion delta pattern e) : (executePatternExpansion (Match delta xs) (n + 1))
  _ -> []

-- helper functions
toNatural :: Int -> Natural
toNatural a = (fromInteger (toInteger a)) :: Natural

{- takes a Term, processes the Match Term, results in a Term-}
patternElaboration :: Term -> Term
patternElaboration t = t


{- an example match -}
matchString :: String
matchString = [r|match exp with <
  {Snd: F | Trd: T} => 1 |
  {Fst: F | Snd: T} => 2 |
  {Trd: F} => 3 |
  {Trd: T} => 4
>|]

matchExample = Match (TermId "exp") [(RecordPattern [("Snd",LabelPattern "F"),("Trd",LabelPattern "T")],App (Label "1") (RecordCons [])),(RecordPattern [("Fst",LabelPattern "F"),("Snd",LabelPattern "T")],App (Label "2") (RecordCons [])),(RecordPattern [("Trd",LabelPattern "F")],App (Label "3") (RecordCons [])),(RecordPattern [("Trd",LabelPattern "T")],App (Label "4") (RecordCons []))]-- testRun match matchString
matchFusionExample = (Match (TermId "x") [(AppPattern "App" (RecordPattern [("Fun",AppPattern "App" (RecordPattern [("Fun",AppPattern "Primitive" (LabelPattern "Map")),("Arg",IdPattern "f")])),("Arg",AppPattern "App" (RecordPattern [("Fun",AppPattern "App" (RecordPattern [("Fun",AppPattern "Primitive" (LabelPattern "Map")),("Arg",IdPattern "g")])),("Arg",IdPattern "x")]))]),App (Label "Success") (App (Label "App") (RecordCons [("Fun",App (Label "App") (RecordCons [("Fun",App (Label "Primitive") (Label "Map")),("Arg",App (Label "Lam") (RecordCons [("Param",App (Label "0") (RecordCons [])),("Body",App (Label "App") (RecordCons [("Fun",TermId "f"),("Arg",App (Label "App") (RecordCons [("Fun",TermId "g"),("Arg",App (Label "Id") (RecordCons [("Name",App (Label "0") (RecordCons []))]))]))]))]))])),("Arg",TermId "x")]))),(MatchAllPattern,App (Label "Failure") (App (Label "1") (RecordCons [])))]) 
-}