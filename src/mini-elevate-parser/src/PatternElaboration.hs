{-# LANGUAGE TupleSections, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
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

type MatchId = [Natural]

type PatternProperty = Bool

data MatchChain = MatchChain MatchId Term [(Pattern, (Either Term MatchChain))] deriving (Show, Eq, Ord)

originalMatchChain :: Term -> Maybe [(Pattern, Term)]
originalMatchChain t = case t of
  Match _ list -> Just list
  _ -> Nothing
    
{- Take a match term and expand it into match chain -}
patternExpansion :: Term -> [MatchChain]
patternExpansion t = case t of
  Match exp list -> case list of
    [] -> []
    (x : xs) -> case x of
      (RecordPattern patterns, term) -> (recordExpansion patterns term (toNatural (length patterns))) ++ (patternExpansion (Match exp xs))
      (MatchAllPattern, term) -> [] -- TODO, what should be the result of match all?
      (p, term) -> (MatchChain [0] exp [(p, (Left term))]) : (patternExpansion (Match exp xs))
  _ -> []

{- Further expand record patterns -}
-- TODO, revisit the match id in nested record patterns
recordExpansion :: [(Label, Pattern)] -> Term -> Natural -> [MatchChain]
recordExpansion [] t _ = []
recordExpansion (x : xs) t n = case x of
  (l, RecordPattern patterns) -> 
    (MatchChain [(n - (toNatural (length xs)) - 1)] (FieldAccess (Label (fresh (length [(n - (toNatural (length xs)) - 1)]))) l) (pair (IdPattern "chain") (recordExpansion patterns t (toNatural (length patterns))))) : (recordExpansion xs t n)
  (l, p) -> 
    (MatchChain [(n - (toNatural (length xs)) - 1)] (FieldAccess (Label (fresh (length [(n - (toNatural (length xs)) - 1)]))) l) [(p, Left t)]) : (recordExpansion xs t n)

{- Introduce a fresh variable -}
fresh :: Int -> String
fresh n = "a" ++ (show (n - 1))

-- helper functions
toNatural :: Int -> Natural
toNatural a = (fromInteger (toInteger a)) :: Natural

pair :: Pattern -> [MatchChain] -> [(Pattern, Either Term MatchChain)]
pair p [] = []
pair p (x : xs) = (p, Right x) : (pair p xs)


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

matchExample = testRun match matchString
-- matchExample = (Match (TermId "x") [(AppPattern "App" (RecordPattern [("Fun",AppPattern "App" (RecordPattern [("Fun",AppPattern "Primitive" (LabelPattern "Map")),("Arg",IdPattern "f")])),("Arg",AppPattern "App" (RecordPattern [("Fun",AppPattern "App" (RecordPattern [("Fun",AppPattern "Primitive" (LabelPattern "Map")),("Arg",IdPattern "g")])),("Arg",IdPattern "x")]))]),App (Label "Success") (App (Label "App") (RecordCons [("Fun",App (Label "App") (RecordCons [("Fun",App (Label "Primitive") (Label "Map")),("Arg",App (Label "Lam") (RecordCons [("Param",App (Label "0") (RecordCons [])),("Body",App (Label "App") (RecordCons [("Fun",TermId "f"),("Arg",App (Label "App") (RecordCons [("Fun",TermId "g"),("Arg",App (Label "Id") (RecordCons [("Name",App (Label "0") (RecordCons []))]))]))]))]))])),("Arg",TermId "x")]))),(MatchAllPattern,App (Label "Failure") (App (Label "1") (RecordCons [])))]) 