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
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
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
import Label
import qualified Control.Monad.Fail as Fail
import Control.Monad.Trans.Except
import Util

import HList as HL
import Data.IORef
import Data.List

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

type MatchId = [Int]

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

type JudgedMatchChain e = [(PatProp, TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel)]

$(derive [makeHFunctor, makeHFoldable, makeHTraversable][''MatchChain, ''RHSExpr])

$(derive [smartConstructors, smartAConstructors][''MatchChain, ''RHSExpr])

instance Semigroup PatProp where
  Var <> Var = Var
  NonVar <> _ = NonVar
  _ <> NonVar = NonVar
  
instance Monoid PatProp where
  mempty = Var

instance (ShowHF a_anTR, HFunctor a_anTR) => ShowHF (MatchChain a_anTR b_anTS) where
  showHF (MatchChainList x_aogN x_aogO)
    = (K $ (showConstr
              "MatchChainList")
              [show x_aogN, "(" ++ show (fst x_aogO) ++ ", " ++ unK (snd x_aogO) ++ ")"])
  showHF (MatchChainTree x_aogP x_aogQ)
    = (K $ (showConstr
              "MatchChainTree")
              [show x_aogP, intercalate ", " . VL.toList $ VL.map (\(a, b) -> "(" ++ show a ++ ", " ++ unK b ++ ")") x_aogQ])


instance (ShowHF a_anSL, HFunctor a_anSL) => ShowHF (RHSExpr a_anSL) where
  showHF (RHSExpr x_aogU)
    = (K $ (showConstr "RHSExpr") [show x_aogU])

matchChainToList :: 
  Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel ->
  [TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel]
matchChainToList mc = caseH (return . inj . hfmap (const (K ()))) (\case
    (MatchChainList a (p, xs) :&: ma) -> inj (MatchChainList a (p, K ()) :&: ma) : matchChainToList xs
  ) (unTerm mc)

listToMatchChain :: forall e.
  [TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel] ->
  Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel
listToMatchChain ls = foldr roll (error "empty chain") ls
  where roll now acc = caseH
          (\(RHSExpr e :&: rhsId) -> iARHSExpr rhsId e :: Fix (TaggedMatchChainSig e SimplePatSig SimplePat) ListModel)
          (\(MatchChainList a (p, _) :&: matchId) -> iAMatchChainList matchId a (p, acc)) now

instance (ShowHF e, HFunctor e) => Show (TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel) where
  show m = caseH (\((RHSExpr rhs) :&: i) -> "RHSExpr " ++ (show rhs) ++ (show i)) (\case
    ((MatchChainList a (p, K ())) :&: i) -> "MatchChainList " ++ (show a) ++ " " ++ (show p) ++ (show i)) m

patExpansion :: forall ts e m. (Occurs ("NameCounter" :- IORef Int) ts HList,
  Update ("RHSId" :- RHSId) ts HList,
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
        return $ (NonVar, inj (MatchChainList delta (
          iAppIdPat label freshId :: Fix SimplePatSig SimplePat, K ()) :&: l)) : chain
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
              return $ (prop, inj (MatchChainList (toRAccess delta) (
                iIdPat freshId :: Fix SimplePatSig SimplePat, K ()) :&: l)) : chain
    )]
  where
    patCase :: forall f y. (f :<: ComplexPatSig) => 
      (f (Fix ComplexPatSig) ComplexPat -> y) -> Fix ComplexPatSig ComplexPat -> Maybe y
    patCase = flip lCase

matchChainReversing :: (t ~ [TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel]) => (t, t) -> t
matchChainReversing ([], chainAccum) = chainAccum
matchChainReversing ((c : chain), chainAccum) = caseH 
  (\((RHSExpr rhs) :&: i) -> chainAccum) 
  (\((MatchChainList a (p, K ())) :&: i) -> matchChainReversing (chain, (c : chainAccum))) c

matchChainGrouping :: (t ~ [TaggedMatchChainSig e SimplePatSig SimplePat (K ()) ListModel]) =>
  (JudgedMatchChain e, t) -> t
matchChainGrouping (tchain, chain) = case tchain of 
  [] -> (error "empty chainT")
  (prop, c) : xs -> case prop of
    Var -> caseH (\case
      ((RHSExpr rhs) :&: i) -> matchChainReversing (chain, [c])) (\case
      ((MatchChainList a (p, K ())) :&: i) -> matchChainGrouping (xs, (c : chain))) c
    NonVar -> c : (matchChainGrouping (xs, chain))

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

executePatternExpansion :: (MonadIO m, Fail.MonadFail m) => 
  Fix ExprSig EXPR -> Int -> m ([JudgedMatchChain ExprSig])
executePatternExpansion expr n = runCase [
  exprCase @(Match PatSig ComplexPat) expr (\case
    Match exp list -> case list of
      [] -> return []
      ((p, l) : xs) -> do
        c <- liftIO $ newIORef 0
        let cxt :: HList '["NameCounter" :- IORef Int, "RHSId" :- RHSId, "MatchId" :- MatchId, "LabelSet" :- Set.Set Label, "Var" :- Id, "RHSTerm" :- Fix ExprSig EXPR]
            cxt = Field c :| Field [n] :| Field [n] :| Field Set.empty :| Field (strId "") :| Field l :| HNil
        tail <- (executePatternExpansion (iMatch exp xs) (n + 1))
        ((fst <$>) . runWriterT . flip runReaderT cxt $ patExpansion (astToAccess exp) p) >>= (\x -> return (x : tail))
  ),
  exprCase @Expr expr (\case
    _ -> Fail.fail "Not a match expression"
  )]
  where
    exprCase :: forall f y. (f :<: ExprSig) => 
      Fix ExprSig EXPR -> (f (Fix ExprSig) EXPR -> y) -> Maybe y
    exprCase = lCase

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

testPE :: Fix ExprSig EXPR -> IO ()
testPE p = do
  r <- runExceptT (executePatternExpansion p 0)
  case r of
    Right r -> print r
    Left m -> putStrLn m

testSorting :: Fix ExprSig EXPR -> IO () 
testSorting p = do
  r <- runExceptT (executePatternExpansion p 0)
  case r of
    Right r -> do
      print (map (\xs -> listToMatchChain (matchChainGrouping (xs, []))) r)
    Left m -> putStrLn m

{-
(NonVar,MatchChainList IRAccess exp (IdPat #a0)[0])
(NonVar,MatchChainList IFAccess #a0 Snd (LabelPat F)[0,0])
(NonVar,MatchChainList IFAccess #a0 Trd (LabelPat T)[0,1])
(Var,RHSExpr)

(NonVar,MatchChainList IRAccess exp (IdPat #a0)[1])
(NonVar,MatchChainList IFAccess #a0 Fst (LabelPat F)[1,0])
(NonVar,MatchChainList IFAccess #a0 Snd (LabelPat T)[1,1])
(Var,RHSExpr)

(NonVar,MatchChainList IRAccess exp (IdPat #a0)[2])
(NonVar,MatchChainList IFAccess #a0 Trd (LabelPat F)[2,0])
(Var,RHSExpr)

(NonVar,MatchChainList IAccess x (AppIdPat App #a0)[0])
(NonVar,MatchChainList IFAccess #a0 Fun (AppIdPat App #a4)[0,0])
(NonVar,MatchChainList IFAccess #a4 Fun (AppIdPat Primitive #a5)[0,0,0])
(NonVar,MatchChainList IAccess #a5 (LabelPat Map)[0,0,0,0])
(Var,MatchChainList IFAccess #a4 Arg (IdPat f)[0,0,1])
(NonVar,MatchChainList IFAccess #a0 Arg (AppIdPat App #a1)[0,1])
(NonVar,MatchChainList IFAccess #a1 Fun (AppIdPat App #a2)[0,1,0])
(NonVar,MatchChainList IFAccess #a2 Fun (AppIdPat Primitive #a3)[0,1,0,0])
(NonVar,MatchChainList IAccess #a3 (LabelPat Map)[0,1,0,0,0])
(Var,MatchChainList IFAccess #a2 Arg (IdPat g)[0,1,0,1])
(Var,MatchChainList IFAccess #a1 Arg (IdPat x)[0,1,1])
(Var,RHSExpr)

(Var,MatchChainList IAccess x (IdPat #a0)[1]),
(Var,RHSExpr)
-}

{- 
*** sorting example one ***
[(MatchChainList IRAccess exp ((IdPat #a0), 
 (MatchChainList IFAccess #a0 Snd ((LabelPat F), 
 (MatchChainList IFAccess #a0 Trd ((LabelPat T), 
 (RHSExpr (AppExpr (LabelLit 1) (RecordCons ))) :&: [0])) :&: [0,1])) :&: [0,0])) :&: [0],

 (MatchChainList IRAccess exp ((IdPat #a0), 
 (MatchChainList IFAccess #a0 Fst ((LabelPat F), 
 (MatchChainList IFAccess #a0 Snd ((LabelPat T), 
 (RHSExpr (AppExpr (LabelLit 2) (RecordCons ))) :&: [1])) :&: [1,1])) :&: [1,0])) :&: [1],

 (MatchChainList IRAccess exp ((IdPat #a0), (MatchChainList IFAccess #a0 Trd ((LabelPat F), 
 (RHSExpr (AppExpr (LabelLit 3) (RecordCons ))) :&: [2])) :&: [2,0])) :&: [2],

 (MatchChainList IRAccess exp ((IdPat #a0), (MatchChainList IFAccess #a0 Trd ((LabelPat T), 
 (RHSExpr (AppExpr (LabelLit 4) (RecordCons ))) :&: [3])) :&: [3,0])) :&: [3]]

*** sorting example two ***
[(MatchChainList IAccess x ((AppIdPat App #a0), 
 (MatchChainList IFAccess #a0 Fun ((AppIdPat App #a4), 
 (MatchChainList IFAccess #a4 Fun ((AppIdPat Primitive #a5), 
 (MatchChainList IAccess #a5 ((LabelPat Map), 
 (MatchChainList IFAccess #a0 Arg ((AppIdPat App #a1), 
 (MatchChainList IFAccess #a1 Fun ((AppIdPat App #a2), 
 (MatchChainList IFAccess #a2 Fun ((AppIdPat Primitive #a3), 
 (MatchChainList IAccess #a3 ((LabelPat Map), 
 (MatchChainList IFAccess #a4 Arg ((IdPat f), 
 (MatchChainList IFAccess #a2 Arg ((IdPat g), 
 (MatchChainList IFAccess #a1 Arg ((IdPat x), 
 (RHSExpr (AppExpr (LabelLit Success) (AppExpr (LabelLit App) (RecordCons (Fun, (AppExpr (LabelLit App) (RecordCons (Fun, (AppExpr (LabelLit Primitive) (LabelLit Map))), (Arg, (AppExpr (LabelLit Lam) (RecordCons (Param, (AppExpr (LabelLit 0) (RecordCons ))), (Body, (AppExpr (LabelLit App) (RecordCons (Fun, (IdExpr f)), (Arg, (AppExpr (LabelLit App) (RecordCons (Fun, (IdExpr g)), (Arg, (AppExpr (LabelLit Id) (RecordCons (Name, (AppExpr (LabelLit 0) (RecordCons )))))))))))))))))), (Arg, (IdExpr x)))))) 
 :&: [0])) :&: [0,1,1])) :&: [0,1,0,1])) :&: [0,0,1])) :&: [0,1,0,0,0])) :&: [0,1,0,0])) :&: [0,1,0])) :&: [0,1])) :&: [0,0,0,0])) :&: [0,0,0])) :&: [0,0])) :&: [0],
 
 (MatchChainList IAccess x ((IdPat #a0), 
 (RHSExpr (AppExpr (LabelLit Failure) (AppExpr (LabelLit 1) (RecordCons )))) :&: [1])) :&: [1]]
-}