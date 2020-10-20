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
    
{Repeat: repeat}
|]