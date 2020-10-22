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
  ELEVATE Programs in mini-ELEVATE
  refs:
  Basic: https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/basic.scala
  Traversal (RISE): https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/traversal.scala
  Traversal (ELEVATE): https://github.com/elevate-lang/elevate/blob/master/src/main/scala/elevate/core/strategies/traversal.scala
  Algorithmic: https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/algorithmic.scala
  Movement: https://github.com/rise-lang/shine/blob/master/src/main/scala/rise/elevate/rules/movement.scala
  TODO: The DepLambda cases in body are omitted here
-}
elevatePrograms :: String
elevatePrograms = [r|
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

let repeat s = try (seq s (repeat s)) in 
  
let mapSuccess rr f = 
  match rr with <
    Success a => Success (f a)
  | Failure b => Failure b
  > in
  
let body s e =
  match e with <
    Lam {Param: Nat | Body: App {Fun: f | Arg: Var {Name: Nat}} | Arg: x} =>
    Success (mapSuccess (s f) (Lam {Param: _ | Body: _ | Arg: x}))
  | _ => Failure s
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
  > in

let topDown s p = (lChoice s (one (topDown s))) p in
  
let bottomUp s p = (lChoice (one (bottomUp s)) s) p in

let allTopdown s p = (seq s (all (allTopdown s))) p in

let allBottomup s p = (seq (all (allBottomup s)) s) p in
  
let tryAll s p = (seq (all (tryAll (try s))) s) p in

let addId = lam e = Success (App {Fun: Primitive Id | Arg: e}) in

let normalize s = repeat (topDown s) in

let not s e =
    match (s e) with <
    Success _ => Failure (not s)
  | Failure _ => Success e
  > in

let isEqualTo x p =
  let cond = riseEq p x in
  match cond with <
    True => Success p
  | False => Failure (isEqualTo x)
  > in

let contains x p = topDown (isEqualTo x) p in

let etaReduction = 
  lam x = match x with <
    Lam {Param: Nat | Body: App {Fun: f | Arg: Var {Name: name}} | Arg: x1} => match x1 with <
      name => 
        let cond = contains x1 f in
        match cond with <
          False => Success f
        | True => Failure etaReduction
        >
    | _ => Failure etaReduction
    >
  | _ => Failure etaReduction
  > in

let etaAbstraction f = match f with <
  Fun f =>
    let x = Var {Name: eta} in
    Success (Lam {Param: 0 | Body: App {Fun: f | Arg: Var {Name: 0}} | Arg: x})
| _ => Failure etaAbstraction
> in

let transposeMove =
  lam x = match x with <
    App {Fun: App {Fun: Primitive Map | Arg: App {Fun: Primitive Map | Arg: f}} | Arg: App {Fun: Primitive Transpose | Arg: y}} =>
    Success (App {Fun: Primitive Transpose | Arg: App {Fun: App {Fun: Primitive Map | Arg: App {Fun: Primitive Map | Arg: f}} | Arg: y}})
  | _ => Failure 1
  > in

let idToTranspose =
  lam x = match x with <
    App {Fun: Primitive Id | Arg: arg} =>
    Success (App {Fun: Lam {Param: 0 | Body: App {Fun: Primitive Transpose | Arg: App {Fun: Primitive Transpose | Arg: Var {Name: 0}}}} | Arg: arg})
  | _ => Failure 1
  > in

let splitJoin = 
  lam x = match x with <
    App {Fun: Primitive Map | Arg: f} => 
    Success (App {Fun: Primitive Join | Arg: App {Fun: App {Fun: Primitive Map | Arg: App {Fun: Primitive Map | Arg: f}} | Arg: Primitive (Split n)}})
  | _ => Failure 1
  > in

let mapFusion = 
  lam x = match x with <
    App {Fun: App {Fun: Primitive Map | Arg: f} | Arg: App {Fun: App {Fun: Primitive Map | Arg: g} | Arg: x}} => 
    Success (App {Fun: App {Fun: Primitive Map | Arg: Lam {Param: 0 | Body: App {Fun: f | Arg: App {Fun: g | Arg: Var {Name: 0}}}}} | Arg: x})
  | _ => Failure 1
  > in

let mapFission = 
  lam x = match x with <
    App {Fun: Primitive Map | Arg: Lam {Param: Nat | Body: App {Fun: f | Arg: App {Fun: g | Arg : Var {Name: Nat}}} | Arg: x}} =>
    let cond1 = contains x f in
    match cond1 with <
      False => 
        let cond2 = isIdentifier (App {Fun: g | Arg: x}) in
        match cond2 with <
          False => App {Fun: App {Fun: Primitive Map | Arg: f} | Arg: App {Fun: Primitive Map | Arg: Lam {Param: 0 | Body: App {Fun: g | Arg: Var {Name: 0}} | Arg: x}}}
        | True => Failure 1
        > 
    | True => Failure 1
    >
  | _ => Failure 1
  > in

let fuseReduceMap =
  lam x = match x with <
    App {Fun: App {Fun: App {Fun: Primitive Reduce | Arg: op}| Arg: init} | Arg: App {Fun: App {Fun: Primitive Map | Arg: f} | Arg: mapArg}} =>
    Success _
  | _ => Failure 1
  > in

let fuseReduceSeqMap =
  lam x = match x with <
    App {Fun: App {Fun: App {Fun: Primitive Reduce | Arg: op}| Arg: init} | Arg: App {Fun: App {Fun: Primitive Map | Arg: f} | Arg: mapArg}} =>
    Success _
  | _ => Failure 1
  > in _
|]