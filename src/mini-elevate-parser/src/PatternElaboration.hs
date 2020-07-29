{-# LANGUAGE TupleSections, QuasiQuotes, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-overlapping-patterns #-}


-- variable | label [variable]
module PatternElaboration where
import Parser
import Data.Maybe
import Data.Either
import Numeric.Natural
import Text.RawString.QQ
import qualified Data.Vec.Lazy as VL
import Data.Type.Nat hiding (toNatural)
import Data.Set as Set
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Arrow

type MatchId = [Natural]

type PatternProperty = Bool

{- MatchChain := term l | match l exp with [...] -}
-- data MatchChain = RHSTerm MatchId Term | MatchChain MatchId Term [(Pattern, MatchChain)] deriving (Show, Eq, Ord)

data MatchChainModel = List | Tree

newtype Fix (f :: k -> * -> *) (p :: k) = Roll {unroll :: f p (Fix f p)}

type Algebra f a = f a -> a

catap :: (Functor (f p)) => Algebra (f p) a -> Fix f p -> a
catap f = unroll >>> fmap (catap f) >>> f

instance Show (Fix MatchChain m) where
  show = catap alg
    where alg :: Algebra (MatchChain m) String 
          alg (RHSTerm mid e) = "RHS " ++ show mid ++ " " ++ show e
          alg (MatchChainList mid e (p, rest)) = 
            "MatchChainList " ++ show mid ++ " " ++ show e ++ " " ++ show p ++ " " ++ rest
          alg (MatchChainTree mid e v) = "MatchChainTree " ++ show mid ++ ""

data MatchChain :: MatchChainModel -> * -> * where
  RHSTerm :: MatchId -> Term -> MatchChain m self
  MatchChainList :: MatchId -> Term -> (Pattern, self) -> MatchChain List self
  MatchChainTree :: MatchId -> Term -> VL.Vec (S n) (Pattern, self) -> MatchChain Tree self

deriving instance (Show self) => Show (MatchChain m self)

deriving instance Functor (MatchChain m)

type MatchChainList = Fix MatchChain List

type MatchChainTree = Fix MatchChain Tree

data TaggedMatchChainList' m self = Up (MatchChain m self) | Down (MatchChain m self)

type TaggedMatchChainList = Fix TaggedMatchChainList' List

type Context = (Set.Set Label, (MatchId, MatchId), Int)

getFreshNameId :: (MonadState Context m) => m Int
getFreshNameId = do
  (_, _, n) <- get
  return n

setFreshNameId :: (MonadState Context m) => Int -> m Int
setFreshNameId n = do
  (labels, ids, _) <- get
  put (labels, ids, n)
  return n

getIds :: (MonadState Context m) => m (MatchId, MatchId)
getIds = do
  (_, ids, _) <- get
  return ids

setIds :: (MonadState Context m) => (MatchId, MatchId) -> m (MatchId, MatchId)
setIds newids = do
  (labels, _, n) <- get
  put (labels, newids, n)
  return newids

-- The result will always be a match chain list model
patternExpansion :: (
  MonadState Context m,
  MonadReader Id m
  ) => Term -> Pattern -> Term -> m MatchChainList
-- MatchAllPattern
patternExpansion delta MatchAllPattern e = do
  n <- getFreshNameId
  let freshName = "#a" ++ show (n :: Int)
  (le, l) <- getIds
  return . Roll $ MatchChainList l delta (IdPattern freshName, Roll $ RHSTerm le e)
-- Match variable
patternExpansion delta (IdPattern v) e = do
  (le, l) <- getIds
  return . Roll $ MatchChainList l delta (IdPattern v, Roll $ RHSTerm le e)
-- Match lables
patternExpansion delta (LabelPattern label) e = do
  (le, l) <- getIds
  return . Roll $ MatchChainList l delta (LabelPattern label, Roll $ RHSTerm le e)
-- Match app
patternExpansion delta (AppPattern label pattern) e = case pattern of
  (IdPattern v) -> do
    (le, l) <- getIds
    return . Roll $ MatchChainList l delta (AppPattern label (IdPattern v),  Roll $ RHSTerm le e)
  _ -> do
    n <- getFreshNameId
    let freshName = "#a" ++ show (n :: Int)
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
    (le, l) <- getIds
    return . Roll $ MatchChainList l (RecordMod delta []) (IdPattern freshName,  Roll $ RHSTerm le e)
  ((label, pattern) : xs) -> case delta of 
    (TermId v) -> case xs of
      [] -> do
        chain <- patternExpansion (FieldAccess (TermId v) label) pattern e
        return chain
      _ -> do
        let tempTermId = "temp"
        chain2 <- patternExpansion (FieldAccess (TermId v) label) pattern (TermId tempTermId)
        (le, l) <- getIds
        let l' = (Prelude.take (length l - 1) l) ++ [(last l + 1)]
        setIds (le, l')
        chain1 <- patternExpansion (TermId v) (RecordPattern xs) e
        return chain2
    _ -> do
      n <- getFreshNameId
      let freshName = "#a" ++ show (n :: Int)
      (le, l) <- getIds
      let l' = l ++ [0]
      setIds (le, l')
      chain <- patternExpansion (TermId freshName) (RecordPattern list) e
      return . Roll $ MatchChainList l (RecordMod delta []) (IdPattern freshName, chain)

replaceTail :: MatchChainList -> MatchChainList -> MatchChainList
replaceTail a b = a

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

-- matchExample = testRun match matchString
matchExample = (Match (TermId "x") [(AppPattern "App" (RecordPattern [("Fun",AppPattern "App" (RecordPattern [("Fun",AppPattern "Primitive" (LabelPattern "Map")),("Arg",IdPattern "f")])),("Arg",AppPattern "App" (RecordPattern [("Fun",AppPattern "App" (RecordPattern [("Fun",AppPattern "Primitive" (LabelPattern "Map")),("Arg",IdPattern "g")])),("Arg",IdPattern "x")]))]),App (Label "Success") (App (Label "App") (RecordCons [("Fun",App (Label "App") (RecordCons [("Fun",App (Label "Primitive") (Label "Map")),("Arg",App (Label "Lam") (RecordCons [("Param",App (Label "0") (RecordCons [])),("Body",App (Label "App") (RecordCons [("Fun",TermId "f"),("Arg",App (Label "App") (RecordCons [("Fun",TermId "g"),("Arg",App (Label "Id") (RecordCons [("Name",App (Label "0") (RecordCons []))]))]))]))]))])),("Arg",TermId "x")]))),(MatchAllPattern,App (Label "Failure") (App (Label "1") (RecordCons [])))]) 