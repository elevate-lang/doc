{-# LANGUAGE TupleSections, QuasiQuotes, TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, 
             FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs, TypeApplications, RankNTypes, AllowAmbiguousTypes #-}

module TestStrategies where

import Text.Parsec hiding (label)
import Text.Parsec.Char
import Data.Functor.Identity
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Text.RawString.QQ
import Data.List as List
import Data.Comp.Multi
import AST
import Id
import qualified Label as L

import AST
import Parser

{- 
 Basic 
 ref : https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala
-}

basic :: String
basic = [r| 
type RewriteResult = forall a b. <Success: a | Failure: b | *> in
type Strategy = forall p q. p -> RewriteResult q Nat in

let id = 
  lam x = Success x in

let fail = 
  lam x = Failure fail in

let flatMapSuccess rr f = 
  match rr with <
    Success a => f a
  | Failure b => Failure b
  > in

let flatMapFailure rr f =
  match rr with <
    Success a => Success a
  | Failure s => f s
  > in

let seq fs ss =
  lam x = flatMapSuccess (fs x) ss in

let lChoice fs ss =
  lam x = flatMapFailure (fs x) (lam y = ss x) in

let try s = lChoice s id in

let repeat s = try (seq s (repeat s)) in _
|]

{- Traversal -}
traversal :: String
traversal = [r|
  let mapSuccess rr f = 
    match rr with <
      Success a => Success (f a)
    | Failure b => Failure b
    > in

  let function s e =
    match e with <
      App {Fun: f | Arg: x} =>
      Success (mapSuccess (s f) (App {Fun: f | Arg: _}))
    | _ => Failure s
    > in

  let argument s e =
    match e with <
      App {Fun: f | Arg: x} =>
      Success (mapSuccess (s f) (App {Fun: f | Arg: _}))
    | _ => Failure s
    > in _
|]

{- Algorithmic and movement -}

-- Add id : https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L120
addId :: String
addId = [r|
let addId = lam e = Success (App {Fun: Primitive Id | Arg: e}) in _
|]

-- Transpose move : https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/movement.scala#L50
transposeMove :: String
transposeMove = [r|
let transposeMove =
  lam x = match x with <
    App {Fun: App {Fun: Primitive Map | Arg: App {Fun: Primitive Map | Arg: f}} | Arg: App {Fun: Primitive Transpose | Arg: y}} =>
    Success (App {Fun: Primitive Transpose | Arg: App {Fun: App {Fun: Primitive Map | Arg: App {Fun: Primitive Map | Arg: f}} | Arg: y}})
  | _ => Failure 1
  > in _
|]

-- Id to transpose : https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L132
-- app ((\x -> transpose (transpose x)), arg)
idToTranspose :: String
idToTranspose = [r|
let idToTranspose =
  lam x = match x with <
    App {Fun: Primitive Id | Arg: arg} =>
    Success (App {Fun: Lam {Param: 0 | Body: App {Fun: Primitive Transpose | Arg: App {Fun: Primitive Transpose | Arg: Id {Name: 0}}}} | Arg: arg})
  | _ => Failure 1
  > in _
|]

-- Split join : https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L29
splitJoin :: String
splitJoin = [r|
let splitJoin = 
  lam x = match x with <
    App {Fun: Primitive Map | Arg: f} => 
    Success (App {Fun: Primitive Join | Arg: App {Fun: App {Fun: Primitive Map | Arg: App {Fun: Primitive Map | Arg: f}} | Arg: Primitive (Split n)}})
  | _ => Failure 1
  > in _
|]

-- Map fusion : https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala#L41
mapFusion :: String
mapFusion = [r|
let mapFusion = 
  lam x = match x with <
    App {Fun: App {Fun: Primitive Map | Arg: f} | Arg: App {Fun: App {Fun: Primitive Map | Arg: g} | Arg: x}} => 
    Success (App {Fun: App {Fun: Primitive Map | Arg: Lam {Param: 0 | Body: App {Fun: f | Arg: App {Fun: g | Arg: Id {Name: 0}}}}} | Arg: x})
  | _ => Failure 1
  > in _
|]