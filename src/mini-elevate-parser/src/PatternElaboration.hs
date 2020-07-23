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
patternExpansion :: Term -> Natural -> [MatchChain]
patternExpansion t n = case t of
  Match exp list -> case list of
    [] -> []
    ((p, t) : xs) -> (patternCases exp n p t) : (patternExpansion (Match exp xs) (n + 1))
  _ -> []

{- Five cases of the pattern expansion -}
patternCases :: Term -> Natural -> Pattern -> Term -> MatchChain
patternCases exp n (IdPattern id) rhsTerm = MatchChain [n] exp [((IdPattern id), (RHSTerm [n] rhsTerm))]
patternCases exp n (LabelPattern label) rhsTerm = MatchChain [n] exp [((LabelPattern label), (RHSTerm [n] rhsTerm))]
patternCases exp n MatchAllPattern rhsTerm = MatchChain [n] (FieldAccess exp ("{}")) [((IdPattern (fresh [n])), (RHSTerm [n] rhsTerm))]
patternCases exp n (AppPattern label pattern) rhsTerm = MatchChain [n] exp [((AppPattern label (IdPattern (fresh [n]))), (patternCases (TermId (fresh [n])) n pattern rhsTerm))]
patternCases exp n (RecordPattern patterns) rhsTerm = MatchChain [n] (RecordMod exp []) [((IdPattern (fresh [n])), (recordExpansionList (preorder patterns (TermId (fresh [n]), True) [n] 0) n rhsTerm))]

{- Expand the flatten record into a match chain -}
recordExpansionList :: [((Term, Bool), Label, Pattern, MatchId)] -> Natural -> Term -> MatchChain
recordExpansionList [] n rhsTerm = (RHSTerm [n] rhsTerm)
recordExpansionList (((t, isRecord), l, p, matchId) : xs) n rhsTerm = case t of
  RecordMod tt ll -> MatchChain matchId (RecordMod tt ll) [(p, (recordExpansionList xs n rhsTerm))]
  _ -> case isRecord of
    True -> MatchChain matchId (FieldAccess t l) [(p, (recordExpansionList xs n rhsTerm))]
    False -> MatchChain matchId t [((AppPattern l p), (recordExpansionList xs n rhsTerm))]

{- Flatten a record list -}
preorder :: [(Label, Pattern)] -> (Term, Bool) -> MatchId -> Natural -> [((Term, Bool), Label, Pattern, MatchId)]
preorder [] (parentTerm, isRecord) matchId n = []
preorder (x : xs) (parentTerm, isRecord) matchId n = case x of
  (l, RecordPattern patterns) -> ((RecordMod parentTerm [], isRecord), l, (IdPattern (fresh (matchId ++ [n]))), (matchId ++ [n])) : (preorder patterns ((TermId (fresh (matchId ++ [n]))), True) (matchId ++ [n, 0]) 0) ++ (preorder xs (parentTerm, True) matchId (n + 1))
  (l, AppPattern label pattern) -> case pattern of 
    RecordPattern pps -> ((parentTerm, isRecord), l, (AppPattern label (IdPattern (fresh (matchId ++ [n])))), (matchId ++ [n])) : ((RecordMod (TermId (fresh (matchId ++ [n]))) [], False), l, (IdPattern (fresh (matchId ++ [n, 0]))), (matchId ++ [n, 0])) : (preorder pps ((TermId (fresh (matchId ++ [n, 0]))), True) (matchId ++ [n, 0]) 0) ++ (preorder xs (parentTerm, True) matchId (n + 1))
    AppPattern ll pp -> ((parentTerm, isRecord), l, (AppPattern label (IdPattern (fresh (matchId ++ [n])))), (matchId ++ [n])) : (preorder [(ll, pp)] ((TermId (fresh (matchId ++ [n]))), False) (matchId ++ [n]) 0) ++ (preorder xs (parentTerm, True) matchId (n + 1))
    p -> ((parentTerm, isRecord), l, (AppPattern label (IdPattern (fresh (matchId ++ [n])))), (matchId ++ [n])) : [(((TermId (fresh (matchId ++ [n]))), False), label, p, matchId ++ [n, 0])] ++ (preorder xs (parentTerm, True) matchId (n + 1))
  (l, p) -> ((parentTerm, isRecord), l, p, (matchId ++ [n]) ) : (preorder xs (parentTerm, isRecord) matchId (n + 1) )

{- Introduce a fresh variable -}
-- TODO use state monad, current it is build using matchId
fresh :: [Natural] -> String
fresh xs = foldl (++) "#fresh" (map show xs)

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