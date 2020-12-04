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
import qualified Control.Monad.Fail as Fail
import System.IO
import Print
import Control.Arrow
import Subst
import Eval

type ParsedSig = ExprSig

type GenRecSig = Expr :+: FunDef PresSig TypeSig :+: RecDef PresSig TypeSig :+: TypeDef TypeSig :+: RecordOps :+: LabelExpr LabelAsLit :+: Match PatSig ComplexPat

type LabelFunSig = FullSig

type TypeDefSubstSig = Expr :+: FunDef PresSig TypeSig :+: RecDef PresSig TypeSig :+: RecordOps :+: LabelExpr LabelAsFun :+: Match PatSig ComplexPat

type ElabSig = Expr :+: FunDef PresSig TypeSig :+: RecDef PresSig TypeSig :+: RecordOps :+: LabelExpr LabelAsFun :+: Match (Pat :+: AppPat) SimplePat :+: RHS MatchId

type InferSig = (Expr :&: TypeRep) :+: (FunDef PresSig TypeSig :&: TypeRep) :+: (RecDef PresSig TypeSig :&: TypeRep) :+: (RecordOps :&: TypeRep) :+: (LabelExpr LabelAsFun :&: TypeRep) :+: (Match (Pat :+: AppPat) SimplePat :&: TypeRep) :+: (RHS MatchId :&: TypeRep)

run :: (Update ("TypeEnv" :- TypeEnv) ts HList, Update ("EvalEnv" :- Subst (Fix ElabSig EXPR)) ts HList, 
  MonadState (HList ts) m, MonadIO m, Fail.MonadFail m) => String -> m ()
run s = do
  nameCounter <- liftIO $ newIORef 0
  case parse program "" s of
    Left err -> liftIO $ print err
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
      (elab, rhsOccur) <- liftIO $ (fmap (second (HL.select @"RHSOccur" @(Map.Map MatchId Int))) $
        flip runStateT sCxt $ flip runReaderT rCxt $
        getCompose (cata patElabAlg typeDefSubst) :: IO (Fix ElabSig EXPR, Map.Map MatchId Int))
      let sCxt :: HList '["RHSOccur" :- Map.Map MatchId Int]
          sCxt = Field (Map.map (const 0) rhsOccur) :| HNil
      (elab, rhsOccur) <- liftIO $ (fmap (second (HL.select @"RHSOccur" @(Map.Map MatchId Int))) $
        flip runStateT sCxt $ getCompose (cata rhsCountAlg elab) :: IO (Fix ElabSig EXPR, Map.Map MatchId Int))
      liftIO $ putStrLn "After pattern elaboration:"
      (liftIO . putStrLn) =<< flip evalStateT Nothing (unK $ cata printAlg elab)
      liftIO $ putStrLn ""
      -- liftIO $ print rhsOccur
      env <- select @"TypeEnv" @TypeEnv <$> get
      let cxt :: HList '["NameCounter" :- IORef Int, "TypeEnv" :- TypeEnv, "RunInfer" :- Bool, "ErrMsg" :- String]
          cxt = Field nameCounter :| Field env :| Field True :| Field "" :| HNil
          stat :: HList '["RHSOccur" :- Map.Map MatchId Int, "REPLEnv" :- TypeEnv]
          stat = Field rhsOccur :| Field env :| HNil
      ((r, cs), newEnv) <- liftIO $ (fmap (second (HL.select @"REPLEnv" @TypeEnv)) $
        flip runStateT stat (runWriterT (flip runReaderT cxt (getCompose (cata inferAlg elab)))) :: IO ((Fix InferSig EXPR, [Constraint]), TypeEnv))
      flip runReaderT cxt (runSolver cs)
      liftIO $ putStrLn "After type inference:"
      (liftIO . putStrLn) =<< flip evalStateT Nothing (unK $ cata (liftA printAlg) r)
      typeStr <- showTypeRep True (getType r)
      liftIO $ putStrLn ("type: " ++ typeStr ++ "\n")
      MS.modify (HL.modify @"TypeEnv" @TypeEnv (const newEnv))
      evalEnv <- select @"EvalEnv" @(Subst (Fix ElabSig EXPR)) <$> get
      let cxt :: HList '["Subst" :- Subst (Fix ElabSig EXPR), "RunEval" :- Bool]
          cxt = Field evalEnv :| Field True :| HNil
          stat :: HList '["REPLEnv" :- Subst (Fix ElabSig EXPR)]
          stat = Field evalEnv :| HNil
      (v, newEvalEnv) <- liftIO $ (fmap (second (HL.select @"REPLEnv" @(Subst (Fix ElabSig EXPR)))) $
        flip runStateT stat (flip runReaderT cxt (getCompose (cata (liftA evalAlg) r))) :: IO (Fix ElabSig EXPR, Subst (Fix ElabSig EXPR)))
      liftIO $ putStrLn "After evaluation:"
      (liftIO . putStrLn) =<< flip evalStateT Nothing (unK $ cata printAlg v)
      MS.modify (HL.modify @"EvalEnv" @(Subst (Fix ElabSig EXPR)) (const newEvalEnv))


readProg :: IO String
readProg = do
  s <- getLine
  if s == "" then return "" else ((s ++ "\n") ++) <$> readProg

repl :: (Update ("TypeEnv" :- TypeEnv) ts HList, Update ("EvalEnv" :- Subst (Fix ElabSig EXPR)) ts HList, 
  MonadState (HList ts) m, MonadIO m, Fail.MonadFail m) => m ()
repl = do
  prog <- liftIO $ putStr "> " >> hFlush stdout >> readProg
  if prog == ":q\n" then return () else run prog >> repl

runREPL :: IO ()
runREPL = flip evalStateT stat repl
  where stat :: HList '["TypeEnv" :- TypeEnv, "EvalEnv" :- Subst (Fix ElabSig EXPR)]
        stat = Field (TypeEnv Map.empty) :| Field Map.empty :| HNil

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

let rise x = match x with <
  Id x => Id x.{Name: x.Name} |
  Lam x => Lam x.{Param: x.Param | Body: rise x.Body} |
  App x => App x.{Fun: rise x.Fun | Arg: rise x.Arg} |
  Primitive x => Primitive (match x with < Map => Map | Zip => Zip | _ => x >) |
  DepLam x => match x.Kind with <
    Nat => DepLam x.{Param: x.Param | Body: rise x.Body} |
    Data => DepLam x.{Param: x.Param | Body: rise x.Body} |
    _ => DepLam x.{Param: x.Param | Body: rise x.Body}
  >
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
  MapFusion: mapFusion |
  SeqFusion: seq mapFusion mapFusion |
  Rise: rise
}
|]