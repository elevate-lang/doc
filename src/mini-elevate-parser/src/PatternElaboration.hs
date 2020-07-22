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

{- MatchChain := term l | match l exp with [...] -}
data MatchChain = RHSTerm MatchId Term | MatchChain MatchId Term [(Pattern, MatchChain)] deriving (Show, Eq, Ord)
    
{- Take a match term and expand it into match chain -}
patternExpansion :: Term -> [MatchChain]
patternExpansion t = case t of
  Match exp list -> case list of
    [] -> []
    ((p, t) : xs) -> (patternCases exp 0 p t) : (patternExpansion (Match exp xs))
  _ -> []

patternCases :: Term -> Natural -> Pattern -> Term -> MatchChain
patternCases exp n (IdPattern id) rhsTerm = MatchChain [n] exp [((IdPattern id), (RHSTerm [n] rhsTerm))]
patternCases exp n (LabelPattern label) rhsTerm = MatchChain [n] exp [((LabelPattern label), (RHSTerm [n] rhsTerm))]
patternCases exp n MatchAllPattern rhsTerm = MatchChain [n] (FieldAccess exp ("{}")) [((IdPattern "#fresh"), (RHSTerm [n] rhsTerm))]
patternCases exp n (AppPattern label pattern) rhsTerm = MatchChain [n] exp [((AppPattern label (IdPattern "#fresh")), (patternCases (TermId "#fresh") n pattern rhsTerm))]
patternCases exp n (RecordPattern patterns) rhsTerm = recordExpansion patterns (IdPattern "#fresh") "fresh" rhsTerm (toNatural (length patterns)) (MatchChain [n] (RecordMod exp []) [])

{- Further expand record patterns -}
-- TODO, revisit the match id in nested record patterns
recordExpansion :: [(Label, Pattern)] -> Pattern -> Id -> Term -> Natural -> MatchChain -> MatchChain
recordExpansion [] prevP id t n chain = chain
recordExpansion (x : xs) prevP id t n chain = case x of
  (l, RecordPattern patterns) -> 
    chain
    -- (MatchChain [(n - (toNatural (length xs)) - 1)] (FieldAccess (Label (fresh (length [(n - (toNatural (length xs)) - 1)]))) l) (pair (IdPattern "chain") (recordExpansion patterns t (toNatural (length patterns))))) : (recordExpansion xs t n)
  (l, p) -> case xs of
    [] -> 
        addToMatchChain chain (prevP, MatchChain [(n - (toNatural (length xs)) - 1)] (FieldAccess (TermId id) l) [(p, RHSTerm [(n - (toNatural (length xs)) - 1)] t)])
    xss ->
      addToMatchChain chain (prevP, (recordExpansion xs p id t n (MatchChain [(n - (toNatural (length xs)) - 1)] (FieldAccess (TermId id) l) [])))

addToMatchChain :: MatchChain -> (Pattern, MatchChain) -> MatchChain
addToMatchChain (RHSTerm id t) pair = RHSTerm id t
addToMatchChain (MatchChain id t list) pair =  MatchChain id t (list ++ [pair])


{- Introduce a fresh variable -}
fresh :: Int -> String
fresh n = "#fresh" ++ (show (n - 1))

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