{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, GADTs, TypeFamilies, TypeApplications, DataKinds #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances#-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, InstanceSigs, TypeApplications, RankNTypes #-}

module Run where

import AST
import Parser
import Desugar
import PatternElaboration
import Infer
import TypeRep
import Data.Comp.Multi
import Text.Parsec hiding (label)
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State as MS
import Control.Monad.Writer
import HList as HL
import Data.Functor.Compose
import Data.Functor.Identity
import Text.RawString.QQ

type ParsedSig = ExprSig

type GenRecSig = Expr :+: FunDef PresSig TypeSig :+: RecDef PresSig TypeSig :+: TypeDef TypeSig :+: RecordOps :+: LabelExpr LabelAsLit :+: Match PatSig ComplexPat

type LabelFunSig = FullSig

type TypeDefSubstSig = Expr :+: FunDef PresSig TypeSig :+: RecDef PresSig TypeSig :+: RecordOps :+: LabelExpr LabelAsFun :+: Match PatSig ComplexPat

type ElabSig = Expr :+: FunDef PresSig TypeSig :+: RecDef PresSig TypeSig :+: RecordOps :+: LabelExpr LabelAsFun :+: Match (Pat :+: AppPat) SimplePat :+: RHS MatchId

type InferSig = (Expr :&: TypeRep) :+: (FunDef PresSig TypeSig :&: TypeRep) :+: (RecDef PresSig TypeSig :&: TypeRep) :+: (RecordOps :&: TypeRep) :+: (LabelExpr LabelAsFun :&: TypeRep) :+: (Match (Pat :+: AppPat) SimplePat :&: TypeRep) :+: (RHS MatchId :&: TypeRep)

run :: String -> IO ()
run s = do
  nameCounter <- newIORef 0
  case parse program "" s of
    Left err -> print err
    Right p -> do
      let genRec :: Fix GenRecSig EXPR
          genRec = flip evalState (Set.empty) (getCompose (cata genRecDefAlg p))
          labelFun :: Fix LabelFunSig EXPR
          labelFun = flip evalState Nothing (getCompose (cata labelFunAlg genRec))
          typeDefSubst :: Fix TypeDefSubstSig EXPR
          typeDefSubst = runIdentity (getCompose (cata typeDefSubstAlg labelFun))
          rCxt :: HList '["NameCounter" :- IORef Int]
          rCxt = Field nameCounter :| HNil
          sCxt :: HList '["MatchCounter" :- Int, "RHSOccur" :- Map.Map MatchId Int]
          sCxt = Field 0 :| Field Map.empty :| HNil
      elab <- flip evalStateT sCxt $ flip runReaderT rCxt $ 
              getCompose (cata patElabAlg typeDefSubst) :: IO (Fix ElabSig EXPR)
      -- print elab
      let cxt :: HList '["NameCounter" :- IORef Int, "TypeEnv" :- TypeEnv]
          cxt = Field nameCounter :| Field (TypeEnv Map.empty) :| HNil
      (r, cs) <- runWriterT (flip runReaderT cxt (getCompose (cata inferAlg elab))) :: IO (Fix InferSig EXPR, [Constraint])
      flip runReaderT cxt (runSolver cs)
      putStrLn =<< showTypeRep (getType r)

elevate :: String
elevate = [r|
let id = 
  lam x = Success x in

let fail = 
  lam x = Failure 1 in

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

let idToTranspose =
  lam x = match x with <
    App {Fun: Primitive Id | Arg: arg} =>
    Success (App {Fun: Lam {Param: 0 | Body: App {Fun: Primitive Transpose | Arg: App {Fun: Primitive Transpose | Arg: Id {Name: 0}}}} | Arg: arg})
  | _ => Failure 1
  > in

let mapFusion = 
  lam x = match x with <
    App {Fun: App {Fun: Primitive Map | Arg: f} | Arg: App {Fun: App {Fun: Primitive Map | Arg: g} | Arg: x}} => 
    Success (App {Fun: App {Fun: Primitive Map | Arg: Lam {Param: 0 | Body: App {Fun: f | Arg: App {Fun: g | Arg: Id {Name: 0}}}}} | Arg: x})
  | _ => Failure 1
  > in 

{
  Id: id |
  Fail: fail |
  FlatMapSuccess: flatMapSuccess |
  FlatMapFailure: flatMapFailure |
  Seq: seq |
  LChoice: lChoice |
  Try: try |
  Repeat: repeat |
  IdToTranspose: idToTranspose |
  MapFusion: mapFusion
}
|]

{-
{

Fail: t228 -> <Failure: <1: {*} | r230: ~{1}> | r231: ~{Failure}> | 

FlatMapFailure: <Success: t236 | Failure: t237 | *> -> (t237 -> <Success: t236 | r239: ~{Success}>) -> <Success: t236 | r239: ~{Success}> | 

FlatMapSuccess: <Success: t232 | Failure: t233 | *> -> (t232 -> <Failure: t233 | r235: ~{Failure}>) -> <Failure: t233 | r235: ~{Failure}> | 

Id: t226 -> <Success: t226 | r227: ~{Success}> | 

IdToTranspose: <App: {Fun: <Primitive: <Id: {*} | r259: ~{Id}> | r260: ~{Primitive}> | Arg: t261 | r262: ~{Arg, Fun}} | r263: ~{App}> -> <Failure: <1: {*} | r264: ~{1}> | Success: <App: {Arg: t261 | Fun: <Lam: {Body: <App: {Arg: <App: {Arg: <Id: {Name: <0: {*} | r265: ~{0}> | *} | r266: ~{Id}> | Fun: <Primitive: <Transpose: {*} | r267: ~{Transpose}> | r268: ~{Primitive}> | *} | r269: ~{App}> | Fun: <Primitive: <Transpose: {*} | r270: ~{Transpose}> | r271: ~{Primitive}> | *} | r272: ~{App}> | Param: <0: {*} | r273: ~{0}> | *} | r274: ~{Lam}> | *} | r275: ~{App}> | r276: ~{Failure, Success}> | 

LChoice: (t245 -> <Success: t246 | Failure: t247 | *>) -> (t245 -> <Success: t246 | r249: ~{Success}>) -> t245 -> <Success: t246 | r249: ~{Success}> | 

MapFusion: <App: {Fun: <App: {Fun: <Primitive: <Map: {*} | r278: ~{Map}> | r279: ~{Primitive}> | Arg: t280 | r281: ~{Arg, Fun}} | r282: ~{App}> | Arg: <App: {Fun: <App: {Fun: <Primitive: <Map: {*} | r283: ~{Map}> | r284: ~{Primitive}> | Arg: t285 | r286: ~{Arg, Fun}} | r287: ~{App}> | Arg: t288 | r289: ~{Arg, Fun}} | r290: ~{App}> | r291: ~{Arg, Fun}} | r292: ~{App}> -> <Failure: <1: {*} | r293: ~{1}> | Success: <App: {Arg: t288 | Fun: <App: {Arg: <Lam: {Body: <App: {Arg: <App: {Arg: <Id: {Name: <0: {*} | r294: ~{0}> | *} | r295: ~{Id}> | Fun: t285 | *} | r296: ~{App}> | Fun: t280 | *} | r297: ~{App}> | Param: <0: {*} | r298: ~{0}> | *} | r299: ~{Lam}> | Fun: <Primitive: <Map: {*} | r300: ~{Map}> | r301: ~{Primitive}> | *} | r302: ~{App}> | *} | r303: ~{App}> | r304: ~{Failure, Success}> | 

Repeat: (t254 -> <Success: t254 | Failure: t255 | *>) -> t254 -> <Success: t254 | Failure: t255 | *> | 

Seq: (t240 -> <Success: t241 | Failure: t242 | *>) -> (t241 -> <Failure: t242 | r244: ~{Failure}>) -> t240 -> <Failure: t242 | r244: ~{Failure}> | 

Try: (t250 -> <Success: t250 | Failure: t251 | *>) -> t250 -> <Success: t250 | r253: ~{Success}> | 

*}
-}