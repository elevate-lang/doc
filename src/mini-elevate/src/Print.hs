{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, GADTs, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Print where

import AST
import Id
import Control.Monad.State
import Data.Functor.Compose
import Data.Set as Set
import Data.Map.Strict as Map
import Text.RawString.QQ
import Data.Comp.Multi
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Derive
import qualified Label as L
import Id
import Data.List as List
import qualified Control.Monad.Fail as Fail

class Print f m where
  printAlg :: Alg f ((K (m String)))

$(derive [liftSum] [''Print])

parens :: String -> String
parens s = "(" ++ s ++ ")"

instance (MonadState (Maybe Bool) m) => Print Expr m where
  printAlg (IdExpr x) = K $ return $ getName x
  printAlg (AppExpr fun arg) = K $ do
    pos <- get
    put (Just True)
    arg' <- unK arg
    put (Just False)
    fun' <- unK fun
    put pos
    let str = fun' ++ " " ++ arg'
    return $ if pos == Just True then parens str else str
  printAlg (LamExpr param body) = K $ do
    pos <- get
    put Nothing
    body' <- unK body
    put pos
    let str = "lam " ++ getName param ++ " = " ++ body'
    return $ if pos == Nothing then str else parens str

instance (MonadState (Maybe Bool) m) => Print (FunDef p t) m where
  printAlg (FunDef name _ _ f e) = K $ do
    pos <- get
    put Nothing
    f' <- unK f
    put Nothing
    e' <- unK e
    put pos
    let str = "let " ++ getName name ++ " = " ++ f' ++ " in " ++ e'
    return $ if pos == Nothing then str else parens str

instance (MonadState (Maybe Bool) m) => Print (RecDef p t) m where
  printAlg (RecDef name _ _ f e) = K $ do
    pos <- get
    put Nothing
    f' <- unK f
    put Nothing
    e' <- unK e
    put pos
    let str = "rec " ++ getName name ++ " = " ++ f' ++ " in " ++ e'
    return $ if pos == Nothing then str else parens str

instance (MonadState (Maybe Bool) m) => Print RecordOps m where
  printAlg rops = case rops of
    RecordCons fields -> K $ printRecordForm fields
    FieldAccess r l -> K $ (++ ("." ++ show l)) <$> unK r
    FieldRemove r l -> K $ (++ (".-" ++ show l)) <$> unK r
    RecordMod r mod -> K $ do
      r' <- unK r
      mod' <- printRecordForm mod
      return $ r' ++ "." ++ mod'
    RecordExt r ext -> K $ do
      r' <- unK r
      ext' <- printRecordForm ext
      return $ r' ++ ".+" ++ ext'
    where printRecordForm rf = do
            pos <- get
            str <- List.intercalate " | " <$> 
              mapM (\(l, e) -> (\e' -> show l ++ ": " ++ e') <$> (put Nothing >> unK e)) rf
            put pos
            return $ "{" ++ str ++ "}"

instance (MonadState (Maybe Bool) m) => Print (LabelExpr LabelAsFun) m where
  printAlg (LabelApp l e) = K $ do
    pos <- get
    put (Just True)
    e' <- unK e
    put pos
    let str = show l ++ " " ++ e'
    return $ if pos == Just True then parens str else str

instance (Monad m) => Print Pat m where
  printAlg (IdPat x) = K $ return $ getName x
  printAlg (LabelPat l) = K $ return $ show l

instance (Fail.MonadFail m) => Print AppPat m where
  printAlg (AppIdPat l x) = K $ return $ (show l ++ " " ++ getName x)
  printAlg (AppPat _ _) = K $ Fail.fail "impossible"

instance (MonadState (Maybe Bool) m, HTraversable p, Print p m) => Print (Match p SimplePat) m where
  printAlg (Match e cases) = K $ do
    pos <- get
    put (Just False)
    e' <- unK e
    let printCase (p, rhs) = do
          put Nothing
          rhs' <- unK rhs
          p' <- unK (cata printAlg p)
          return (p' ++ " => " ++ rhs')
    cases' <- List.intercalate " | " <$> mapM printCase cases
    put pos
    let str = "match " ++ e' ++ " with <" ++ cases' ++ ">"
    return $ if pos == Nothing then str else parens str

instance (Monad m, Show i) => Print (RHS i) m where
  printAlg (RHS i e) = K $ {-((show i ++ " ") ++) <$>-} unK e