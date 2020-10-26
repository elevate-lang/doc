{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeApplications #-}

module Unifier where

import qualified Label as L
import Id
import TypeRep
import HList as HList
import Data.IORef
import Control.Monad
import Control.Exception
import qualified UnionFind as UF
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Control.Monad.Fail as Fail
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Foldable
import Data.List

unify :: (MonadIO m, Fail.MonadFail m, 
  Occurs ("NameCounter" :- IORef Int) ts HList, MonadReader (HList ts) m) => TypeRep -> TypeRep -> m KindRep
unify m n = do
  a <- liftIO $ UF.repr m
  b <- liftIO $ UF.repr n
  if (a == b) then do
    da <- liftIO $ UF.find a
    dav <- liftIO $ readIORef da
    case structure dav of
      FunTypeRep arga reta -> if isType dav then return Type else do
        liftIO $ writeIORef da (dav {isType = True})
        argk <- unify arga arga
        retk <- unify reta reta
        case (argk, retk) of
          (Type, Type) -> return Type
          _ -> Fail.fail "kind check fails"
      VariantRep ra -> if isType dav then return Type else do
        liftIO $ writeIORef da (dav {isType = True})
        rk <- unify ra ra
        case rk of
          Type -> Fail.fail "kind check fails"
          _ -> return Type
      RecordRep ra -> if isType dav then return Type else do
        liftIO $ writeIORef da (dav {isType = True})
        rk <- unify ra ra
        case rk of
          Type -> Fail.fail "kind check fails"
          _ -> return Type
      RowRep lsa ra -> do
        liftIO $ writeIORef da (dav {isType = False})
        lsa' <- mapM (\x -> unify x x) lsa
        rk <- unify ra ra
        if not (null (Map.filter (/= Type) lsa'))
        then Fail.fail "kind check fails"
        else case rk of
          Type -> Fail.fail "kind check fails"
          _ -> extendKind (Map.keysSet lsa') rk
      IdTypeRep (TIdRep {kind = k}) -> do
        case k of
          Type -> liftIO $ writeIORef da (dav {isType = True})
          _ -> liftIO $ writeIORef da (dav {isType = False})
        return k
      -- _ -> Fail.fail "kind check fails"
  else do
    da <- liftIO $ UF.find a
    db <- liftIO $ UF.find b
    dav <- liftIO $ readIORef da
    dbv <- liftIO $ readIORef db
    case (structure dav, structure dbv) of
      (IdTypeRep (TIdRep {kind = ak}), IdTypeRep (TIdRep {kind = bk})) -> do
          if kindInstCond ak bk
          then liftIO $ UF.union a b >> return bk
          else 
            if kindInstCond bk ak 
            then liftIO $ UF.union b a >> return ak
            else do
              rvk <- mergeKind ak bk
              rv <- genFreshIdType rvk
              liftIO $ UF.union a rv
              liftIO $ UF.union b rv
              return rvk
      (IdTypeRep (TIdRep {kind = k}), _) -> do
        bk <- unify b b
        if kindInstCond k bk
        then liftIO $ UF.union a b >> return bk
        else do
          row <- typeRep (RowRep Map.empty a)
          unify row b
      (_, IdTypeRep (TIdRep {kind = k})) -> do
        ak <- unify a a
        if kindInstCond k ak
        then liftIO $ UF.union b a >> return ak
        else do
          row <- typeRep (RowRep Map.empty b)
          unify row a
      (FunTypeRep arga reta, FunTypeRep argb retb) -> do
        liftIO $ UF.union a b
        argk <- unify arga argb
        retk <- unify reta retb
        case (argk, retk) of
          (Type, Type) -> return Type
          _ -> Fail.fail "kind check fails"
      (VariantRep ra, VariantRep rb) -> do
        liftIO $ UF.union a b
        rk <- unify ra rb
        case rk of
          Type -> Fail.fail "kind check fails"
          _ -> return Type
      (RecordRep ra, RecordRep rb) -> do
        liftIO $ UF.union a b
        rk <- unify ra rb
        case rk of
          Type -> Fail.fail "kind check fails"
          _ -> return Type
      (RowRep lsa ra, RowRep lsb rb) -> do
        liftIO $ UF.union a b
        commonLabels <- sequence (Map.intersectionWith unify lsa lsb)
        if not (Map.null (Map.filter (/= Type) commonLabels))
        then Fail.fail "kind check fails"
        else do
          let resa = Map.difference lsa commonLabels
          let resb = Map.difference lsb commonLabels
          ra' <- liftIO $ UF.repr ra
          rb' <- liftIO $ UF.repr rb
          if (ra' == rb') then
            if not (Map.null resa && Map.null resb)
            then Fail.fail "cannot unify"
            else do
              rk <- unify ra' ra'
              case rk of
                Type -> Fail.fail "kind check fails"
                _ -> extendKind (Map.keysSet commonLabels) rk
          else do
            (ra'', rb'') <- if Map.null resa && Map.null resb then return (ra', rb') else do
              dra <- liftIO $ UF.find ra'
              drb <- liftIO $ UF.find rb'
              drav <- liftIO $ readIORef dra
              drbv <- liftIO $ readIORef drb
              case (structure drav, structure drbv) of
                (RowRep _ _, RowRep _ _) -> do
                  newra <- extendRow resa (structure drav)
                  newrb <- extendRow resb (structure drbv)
                  ra' <- typeRepFromDesc (drav {structure = newra})
                  rb' <- typeRepFromDesc (drbv {structure = newrb})
                  return (ra', rb')
                (RowRep _ _, IdTypeRep _) -> do
                  newra <- extendRow resa (structure drav)
                  ra' <- typeRepFromDesc (drav {structure = newra})
                  rb' <- if Map.null resb then return rb' else typeRep (RowRep resb rb')
                  return (ra', rb')
                (IdTypeRep _, RowRep _ _) -> do
                  newrb <- extendRow resb (structure drbv)
                  rb' <- typeRepFromDesc (drbv {structure = newrb})
                  ra' <- if Map.null resa then return ra' else typeRep (RowRep resa ra')
                  return (ra', rb')
                (IdTypeRep (TIdRep {kind = ak}), IdTypeRep (TIdRep {kind = bk})) -> do
                    extendKind (Map.keysSet resa) ak -- kind check
                    extendKind (Map.keysSet resb) bk -- kind check
                    rvk <- mergeKind ak bk
                    rv <- genFreshIdType rvk
                    ub <- if Map.null resa then return rv else typeRep (RowRep resa rv)
                    ua <- if Map.null resb then return rv else typeRep (RowRep resb rv)
                    ubk <- extendKind (Map.keysSet resa) rvk
                    uak <- extendKind (Map.keysSet resb) rvk
                    if kindInstCond ak uak then liftIO $ UF.union ra' ua else Fail.fail "kind check fails"
                    if kindInstCond bk ubk then liftIO $ UF.union rb' ub else Fail.fail "kind check fails"
                    ra' <- if Map.null resa then return ra' else typeRep (RowRep resa ra')
                    rb' <- if Map.null resb then return rb' else typeRep (RowRep resb rb')
                    return (ra', rb')
            rk <- unify ra'' rb''
            if Map.null commonLabels
            then
              liftIO $ UF.change a =<< UF.find' ra''
            else
              liftIO $ UF.change a (typeDesc (RowRep (Map.intersection lsa commonLabels) ra''))
            case rk of
              Type -> Fail.fail "kind check fails"
              _ -> extendKind (Map.keysSet commonLabels) rk
      _ -> Fail.fail "cannot unify"