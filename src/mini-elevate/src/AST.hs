{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable#-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DataKinds, GADTs, PolyKinds #-}
{-# LANGUAGE KindSignatures, PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

module AST where

import Data.Comp.Multi
import Data.Comp.Multi.Derive
import Data.Comp.Multi.Show
import Data.Comp.Multi.Equality
import Data.Comp.Multi.Ordering
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HTraversable
import Data.Comp.Multi.HFunctor
import Data.Comp.Multi.Ops
import Data.Comp.Multi.Term
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import Control.Arrow
import Id
import qualified Label as L

type Fix f = Term f

data PRES

data Pres :: (* -> *) -> (* -> *) where
  Pres :: [L.Label] -> Pres p PRES
  Lack :: [L.Label] -> Pres p PRES

data UnknownPres :: (* -> *) -> (* -> *) where
  UnknownPres :: UnknownPres p PRES

data TypeKind
  
data RowKind

data SchemeKind

data Row :: (* -> *) -> * -> * where
  IdRow :: Id -> Row t RowKind
  EmptyRow :: Row t RowKind
  ExtendRow :: (L.Label, t TypeKind) -> t RowKind -> Row t RowKind

data Type :: (* -> *) -> * -> * where
  IdType :: Id -> Type t TypeKind
  VariantType :: t RowKind -> Type t TypeKind
  RecordType :: t RowKind -> Type t TypeKind
  FunType :: t TypeKind -> t TypeKind -> Type t TypeKind

data UnknownType :: (* -> *) -> * -> * where
  UnknownType :: UnknownType t TypeKind

data RecVariantType :: (* -> *) -> * -> * where
  RecVariantType :: Id -> t RowKind -> RecVariantType t TypeKind

data Scheme :: ((* -> *) -> * -> *) -> (* -> *) -> * -> * where
  Scheme :: [Id] -> [(Id, Fix p PRES)] -> t TypeKind -> Scheme p t SchemeKind

data IdScheme :: (* -> *) -> * -> * where
  IdScheme :: Id -> IdScheme t SchemeKind

data SchemeInst :: (* -> *) -> * -> * where
  SchemeInst :: t SchemeKind -> [t TypeKind] -> [t RowKind] -> SchemeInst t TypeKind

data ComplexPat

data SimplePat

data Pat :: (* -> *) -> * -> * where
  IdPat :: Id -> Pat p l
  LabelPat :: L.Label -> Pat p l

data AppPat :: (* -> *) -> * -> * where
  AppPat :: L.Label -> p ComplexPat -> AppPat p ComplexPat
  AppIdPat :: L.Label -> Id -> AppPat p SimplePat

data RecordPat :: (* -> *) -> * -> * where
  RecordPat :: [(L.Label, p ComplexPat)] -> RecordPat p ComplexPat

data MatchAllPat :: (* -> *) -> * -> * where
  MatchAllPat :: MatchAllPat p ComplexPat

data EXPR

data Expr :: (* -> *) -> * -> * where
  IdExpr :: Id -> Expr e EXPR
  AppExpr :: e EXPR -> e EXPR -> Expr e EXPR
  LamExpr :: Id -> e EXPR -> Expr e EXPR

data FunDef :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> * where
  FunDef :: Id -> Bool -> ([Id], [(Id, Fix p PRES)], Fix t TypeKind) -> 
    e EXPR -> e EXPR -> FunDef p t e EXPR

data RecDef :: ((* -> *) -> * -> *) -> ((* -> *) -> * -> *) -> (* -> *) -> * -> * where
  RecDef :: Id -> Bool -> ([Id], [(Id, Fix p PRES)], Fix t TypeKind) -> 
    e EXPR -> e EXPR -> RecDef p t e EXPR

data TypeDef :: ((* -> *) -> * -> *) -> (* -> *) -> * -> * where
  TypeDef :: Id -> Fix t SchemeKind -> e EXPR -> TypeDef t e EXPR

data RecordOps :: (* -> *) -> * -> * where
  RecordCons :: [(L.Label, e EXPR)] -> RecordOps e EXPR
  FieldAccess :: e EXPR -> L.Label -> RecordOps e EXPR
  FieldRemove :: e EXPR -> L.Label -> RecordOps e EXPR
  RecordMod :: e EXPR -> [(L.Label, e EXPR)] -> RecordOps e EXPR
  RecordExt :: e EXPR -> [(L.Label, e EXPR)] -> RecordOps e EXPR

data LabelAsLit

data LabelAsFun

data LabelExpr :: * -> (* -> *) -> * -> * where
  LabelLit :: L.Label -> LabelExpr LabelAsLit e EXPR
  LabelApp :: L.Label -> e EXPR -> LabelExpr LabelAsFun e EXPR

data Match :: ((* -> *) -> * -> *) -> * -> (* -> *) -> * -> * where
  Match :: e EXPR -> [(Fix p l, e EXPR)] -> Match p l e EXPR

data RHS :: * -> (* -> *) -> * -> * where
  RHS :: i -> e EXPR -> RHS i e EXPR

$(derive [makeHFunctor, makeHFoldable, makeHTraversable] 
         [''Pres, ''UnknownPres, 
          ''Row, ''Type, ''UnknownType, ''RecVariantType,
          ''Scheme, ''IdScheme, ''SchemeInst,
          ''Pat, ''AppPat, ''RecordPat, ''MatchAllPat,
          ''Expr, ''FunDef, ''RecDef, ''TypeDef, ''RecordOps, ''LabelExpr, ''Match, ''RHS])

$(derive [smartConstructors, smartAConstructors] 
         [''Pres, {-''UnknownPres, -}
          {-''Row, -}''Type, {-''UnknownType, -}''RecVariantType,
          ''Scheme, ''IdScheme, ''SchemeInst,
          ''Pat, ''AppPat, ''RecordPat, {-''MatchAllPat,-}
          ''Expr, ''FunDef, ''RecDef, ''TypeDef, ''RecordOps, ''LabelExpr, ''Match, ''RHS])

$(derive [makeEqHF, makeOrdHF] 
         [''Pres, ''UnknownPres, 
          {-''Row, -}''Type, ''UnknownType, ''RecVariantType,
          {-''Scheme, -}''IdScheme, {-''SchemeInst,-}
          ''Pat, ''AppPat, {-''RecordPat, -}''MatchAllPat,
          ''Expr{-, ''FunDef-}{-, ''RecDef-}{-, ''TypeDef-}{-, ''RecordOps-}{-, ''LabelExpr-}{-, ''Match-}{-, ''RHS-}])

$(derive [makeShowHF] 
         [''Pres, ''UnknownPres, 
          {-''Row, -}''Type, ''UnknownType, ''RecVariantType,
          {-''Scheme, -}''IdScheme, {-''SchemeInst,-}
          ''Pat, ''AppPat, {-''RecordPat, -}''MatchAllPat,
          ''Expr{-, ''FunDef-}{-, ''RecDef-}{-, ''TypeDef-}{-, ''RecordOps-}{-, ''LabelExpr-}{-, ''Match-}, ''RHS])

{- based on automatically generated code-}

iUnknownPres :: (UnknownPres :<: f) => Cxt h f a PRES
iUnknownPres = inject UnknownPres

iIdRow x_a5iR = inject (IdRow x_a5iR)

iEmptyRow :: (Row :<: f) => Cxt h f a RowKind
iEmptyRow = inject EmptyRow

iExtendRow (x_a5iT, x_a5iU) x_a5iV
  = inject ((ExtendRow (x_a5iT, x_a5iU)) x_a5iV)

iUnknownType :: (UnknownType :<: f) => Cxt h f a TypeKind
iUnknownType = inject UnknownType

iMatchAllPat :: (MatchAllPat :<: f) => Cxt h f a ComplexPat
iMatchAllPat = inject MatchAllPat

iAUnknownPres _p_a5j2
  = Term ((injectA _p_a5j2) (inj UnknownPres))

iAIdRow _p_a5j7 x_a5j6
  = Term ((injectA _p_a5j7) (inj (IdRow x_a5j6)))

iAEmptyRow _p_a5j9
  = Term ((injectA _p_a5j9) (inj EmptyRow))

iAExtendRow _p_a5jd (x_a5ja, x_a5jb) x_a5jc
  = Term
      ((injectA _p_a5jd) (inj ((ExtendRow (x_a5ja, x_a5jb)) x_a5jc)))

iAUnknownType _p_a5jg
  = Term ((injectA _p_a5jg) (inj UnknownType))

iAMatchAllPat _p_a5jj
  = Term ((injectA _p_a5jj) (inj MatchAllPat))

compList :: [Ordering] -> Ordering
compList = fromMaybe EQ . find (/= EQ)

keqList :: (KEq f) => [f i] -> [f j] -> Bool
keqList [] [] = True
keqList (x : xs) (y : ys) = keq x y && keqList xs ys
keqList _ _ = False

keqTList :: (Eq a, KEq f) => [(a, f i)] -> [(a, f j)] -> Bool
keqTList [] [] = True
keqTList ((ax, x) : xs) ((ay, y) : ys) = 
  keq x y && (ax == ay) && keqTList xs ys
keqTList _ _ = False

kcompareList :: (KOrd f) => [f i] -> [f j] -> Ordering
kcompareList [] [] = EQ
kcompareList [] (_:_) = LT
kcompareList (_:_) [] = GT
kcompareList (x : xs) (y : ys) = case kcompare x y of
  EQ -> kcompareList xs ys
  other -> other

kcompareTList :: (Ord a, KOrd f) => [(a, f i)] -> [(a, f j)] -> Ordering
kcompareTList [] [] = EQ
kcompareTList [] (_:_) = LT
kcompareTList (_:_) [] = GT
kcompareTList ((ax, x) : xs) ((ay, y) : ys) = case compare ax ay of
  EQ -> case kcompare x y of
    EQ -> kcompareTList xs ys
    other -> other
  other -> other

instance EqHF Row where
  eqHF (IdRow x_a5Rw) (IdRow y_a5Rx) = and [(x_a5Rw == y_a5Rx)]
  eqHF EmptyRow EmptyRow = and [True]
  eqHF
    (ExtendRow (x_a5RA, x_a5RB) x_a5RC)
    (ExtendRow (y_a5RD, y_a5RE) y_a5RF)
    = and
        [(x_a5RA == y_a5RD), (x_a5RB `keq` y_a5RE), (x_a5RC `keq` y_a5RF)]
  eqHF _ _ = False

instance OrdHF Row where
  compareHF (IdRow x_a5RJ) (IdRow y_a5RK)
    = compList [(compare x_a5RJ) y_a5RK]
  compareHF IdRow {} EmptyRow = LT
  compareHF IdRow {} ExtendRow {} = LT
  compareHF EmptyRow IdRow {} = GT
  compareHF EmptyRow EmptyRow = EQ
  compareHF EmptyRow ExtendRow {} = LT
  compareHF ExtendRow {} IdRow {} = GT
  compareHF ExtendRow {} EmptyRow = GT
  compareHF
    (ExtendRow (x_a5RN, x_a5RO) x_a5RP)
    (ExtendRow (y_a5RQ, y_a5RR) y_a5RS)
    = compList [(compare x_a5RN) y_a5RQ, (kcompare x_a5RO) y_a5RR,
          (kcompare x_a5RP) y_a5RS]

instance (EqHF a_a2pH) => EqHF (Scheme a_a2pH) where
  eqHF (Scheme x_aahF x_aahG x_aahH) (Scheme y_aahI y_aahJ y_aahK)
    = and
        [(x_aahF == y_aahI), (x_aahG == y_aahJ), (x_aahH `keq` y_aahK)]

instance (HFunctor a_a2pH, OrdHF a_a2pH) => OrdHF (Scheme a_a2pH) where
  compareHF
    (Scheme x_aahN x_aahO x_aahP)
    (Scheme y_aahQ y_aahR y_aahS)
    = compList
        [(compare x_aahN) y_aahQ, (compare x_aahO) y_aahR,
          (kcompare x_aahP) y_aahS]

instance EqHF SchemeInst where
  eqHF
    (SchemeInst x_aahE x_aahF x_aahG)
    (SchemeInst y_aahH y_aahI y_aahJ)
    = and
        [(x_aahE `keq` y_aahH), (x_aahF `keqList` y_aahI),
          (x_aahG `keqList` y_aahJ)]

instance OrdHF SchemeInst where
  compareHF
    (SchemeInst x_aahL x_aahM x_aahN)
    (SchemeInst y_aahO y_aahP y_aahQ)
    = compList
        [(kcompare x_aahL) y_aahO, (kcompareList x_aahM) y_aahP,
          (kcompareList x_aahN) y_aahQ]

instance EqHF RecordPat where
  eqHF (RecordPat x_aakq) (RecordPat y_aakr)
    = and [(x_aakq `keqTList` y_aakr)]

instance OrdHF RecordPat where
  compareHF (RecordPat x_aakt) (RecordPat y_aaku)
    = compList
        [(kcompareTList x_aakt) y_aaku]

instance (EqHF a_a2vA, EqHF b_a2vB) => EqHF (FunDef a_a2vA b_a2vB) where
  eqHF
    (FunDef x_aaks x_aakt x_aaku x_aaky x_aakz)
    (FunDef y_aakA y_aakB y_aakC y_aakG y_aakH)
    = and
        [(x_aaks == y_aakA), (x_aakt == y_aakB), (x_aaku == y_aakC),
          (x_aaky `keq` y_aakG), (x_aakz `keq` y_aakH)]

instance (HFunctor a_a2vA, HFunctor b_a2vB, OrdHF a_a2vA, OrdHF b_a2vB) => OrdHF (FunDef a_a2vA b_a2vB) where
  compareHF
    (FunDef x_aakL x_aakM x_aakN x_aakR x_aakS)
    (FunDef y_aakT y_aakU y_aakV y_aakZ y_aal0)
    = compList
        [(compare x_aakL) y_aakT, (compare x_aakM) y_aakU, (compare x_aakN) y_aakV,
          (kcompare x_aakR) y_aakZ, (kcompare x_aakS) y_aal0]

instance (EqHF a_a2wr, EqHF b_a2ws) =>
          EqHF (RecDef a_a2wr b_a2ws) where
  eqHF
    (RecDef x_aaks x_aakt x_aaku x_aaky x_aakz)
    (RecDef y_aakA y_aakB y_aakC y_aakG y_aakH)
    = and
        [(x_aaks == y_aakA), (x_aakt == y_aakB), (x_aaku == y_aakC),
          (x_aaky `keq` y_aakG), (x_aakz `keq` y_aakH)]

instance (HFunctor a_a2wr, HFunctor b_a2ws, OrdHF a_a2wr, OrdHF b_a2ws) => OrdHF (RecDef a_a2wr b_a2ws) where
  compareHF
    (RecDef x_aakL x_aakM x_aakN x_aakR x_aakS)
    (RecDef y_aakT y_aakU y_aakV y_aakZ y_aal0)
    = compList
        [(compare x_aakL) y_aakT, (compare x_aakM) y_aakU, (compare x_aakN) y_aakV,
          (kcompare x_aakR) y_aakZ, (kcompare x_aakS) y_aal0]

instance (EqHF a_a2xc) => EqHF (TypeDef a_a2xc) where
  eqHF (TypeDef x_aakr x_aaks x_aakt) (TypeDef y_aaku y_aakv y_aakw)
    = and
        [(x_aakr == y_aaku), (x_aaks == y_aakv), (x_aakt `keq` y_aakw)]

instance (HFunctor a_a2xc, OrdHF a_a2xc) => OrdHF (TypeDef a_a2xc) where
  compareHF
    (TypeDef x_aakz x_aakA x_aakB)
    (TypeDef y_aakC y_aakD y_aakE)
    = compList
        [(compare x_aakz) y_aakC, (compare x_aakA) y_aakD,
          (kcompare x_aakB) y_aakE]

instance EqHF RecordOps where
  eqHF (RecordCons x_aaku) (RecordCons y_aakv)
    = and [(x_aaku `keqTList` y_aakv)]
  eqHF (FieldAccess x_aakw x_aakx) (FieldAccess y_aaky y_aakz)
    = and [(x_aakw `keq` y_aaky), (x_aakx == y_aakz)]
  eqHF (FieldRemove x_aakA x_aakB) (FieldRemove y_aakC y_aakD)
    = and [(x_aakA `keq` y_aakC), (x_aakB == y_aakD)]
  eqHF (RecordMod x_aakE x_aakF) (RecordMod y_aakG y_aakH)
    = and [(x_aakE `keq` y_aakG), (x_aakF `keqTList` y_aakH)]
  eqHF (RecordExt x_aakI x_aakJ) (RecordExt y_aakK y_aakL)
    = and [(x_aakI `keq` y_aakK), (x_aakJ `keqTList` y_aakL)]
  eqHF _ _ = False

instance OrdHF RecordOps where
  compareHF (RecordCons x_aakR) (RecordCons y_aakS)
    = compList
        [(kcompareTList x_aakR) y_aakS]
  compareHF RecordCons {} FieldAccess {} = LT
  compareHF RecordCons {} FieldRemove {} = LT
  compareHF RecordCons {} RecordMod {} = LT
  compareHF RecordCons {} RecordExt {} = LT
  compareHF FieldAccess {} RecordCons {} = GT
  compareHF (FieldAccess x_aakT x_aakU) (FieldAccess y_aakV y_aakW)
    = compList
        [(kcompare x_aakT) y_aakV, (compare x_aakU) y_aakW]
  compareHF FieldAccess {} FieldRemove {} = LT
  compareHF FieldAccess {} RecordMod {} = LT
  compareHF FieldAccess {} RecordExt {} = LT
  compareHF FieldRemove {} RecordCons {} = GT
  compareHF FieldRemove {} FieldAccess {} = GT
  compareHF (FieldRemove x_aakX x_aakY) (FieldRemove y_aakZ y_aal0)
    = compList
        [(kcompare x_aakX) y_aakZ, (compare x_aakY) y_aal0]
  compareHF FieldRemove {} RecordMod {} = LT
  compareHF FieldRemove {} RecordExt {} = LT
  compareHF RecordMod {} RecordCons {} = GT
  compareHF RecordMod {} FieldAccess {} = GT
  compareHF RecordMod {} FieldRemove {} = GT
  compareHF (RecordMod x_aal1 x_aal2) (RecordMod y_aal3 y_aal4)
    = compList
        [(kcompare x_aal1) y_aal3, (kcompareTList x_aal2) y_aal4]
  compareHF RecordMod {} RecordExt {} = LT
  compareHF RecordExt {} RecordCons {} = GT
  compareHF RecordExt {} FieldAccess {} = GT
  compareHF RecordExt {} FieldRemove {} = GT
  compareHF RecordExt {} RecordMod {} = GT
  compareHF (RecordExt x_aal5 x_aal6) (RecordExt y_aal7 y_aal8)
    = compList
        [(kcompare x_aal5) y_aal7, (kcompareTList x_aal6) y_aal8]

instance EqHF (LabelExpr a_a2zX) where
  eqHF (LabelLit x_aakr) (LabelLit y_aaks) = and [(x_aakr == y_aaks)]
  eqHF (LabelApp x_aakt x_aaku) (LabelApp y_aakv y_aakw)
    = and [(x_aakt == y_aakv), (x_aaku `keq` y_aakw)]
  eqHF _ _ = False

instance OrdHF (LabelExpr a_a2zX) where
  compareHF (LabelLit x_aakz) (LabelLit y_aakA)
    = compList
        [(compare x_aakz) y_aakA]
  compareHF LabelLit {} LabelApp {} = LT
  compareHF LabelApp {} LabelLit {} = GT
  compareHF (LabelApp x_aakB x_aakC) (LabelApp y_aakD y_aakE)
    = compList
        [(compare x_aakB) y_aakD, (kcompare x_aakC) y_aakE]

instance (EqHF a_a2AD) => EqHF (Match a_a2AD b_a2AE) where
  eqHF (Match x_aaks x_aakt) (Match y_aaku y_aakv)
    = and [(x_aaks `keq` y_aaku), (x_aakt `keqTList` y_aakv)]

instance (HFunctor a_a2AD, OrdHF a_a2AD) => OrdHF (Match a_a2AD b_a2AE) where
  compareHF (Match x_aakz x_aakA) (Match y_aakB y_aakC)
    = compList
        [(kcompare x_aakz) y_aakB, (kcompareTList x_aakA) y_aakC]

instance Eq a_aasm => EqHF (RHS a_aasm) where
  eqHF (RHS x_aelp x_aelq) (RHS y_aelr y_aels)
    = and [(x_aelp == y_aelr), (x_aelq `keq` y_aels)]

instance (Ord a_aasm) => OrdHF (RHS a_aasm) where
  compareHF (RHS x_aelv x_aelw) (RHS y_aelx y_aely)
    = compList
        [(compare x_aelv) y_aelx, (kcompare x_aelw) y_aely]

showConstr :: String -> [String] -> String
showConstr con [] = con
showConstr con args = "(" ++ con ++ " " ++ unwords args ++ ")"

instance ShowHF Row where
  showHF (IdRow x_a9OH)
    = (K $ (showConstr
              "IdRow")
              [show x_a9OH])
  showHF EmptyRow
    = (K $ (showConstr
              "EmptyRow")
              [])
  showHF (ExtendRow x_a9OI x_a9OJ)
    = (K $ (showConstr
              "ExtendRow")
              [show (second unK x_a9OI), unK x_a9OJ])

instance (HFunctor a_a2ks, ShowHF a_a2ks) => ShowHF (Scheme a_a2ks) where
  showHF (Scheme x_a9OM x_a9ON x_a9OO)
    = (K $ (showConstr
              "Scheme")
              [show x_a9OM, show x_a9ON, unK x_a9OO])

instance ShowHF RecordPat where
  showHF (RecordPat x_a9OQ)
    = (K $ (showConstr
              "RecordPat")
              [(\x -> "[" ++ x ++ "]") . intercalate ", " $ 
                map (\(a, b) -> "(" ++ show a ++ ", " ++ unK b ++ ")") x_a9OQ])

instance (HFunctor a_a2ql, HFunctor b_a2qm, ShowHF a_a2ql, ShowHF b_a2qm) =>
          ShowHF (FunDef a_a2ql b_a2qm) where
  showHF
    (FunDef x_a9OU x_a9OV x_a9OW x_a9P0 x_a9P1)
    = (K $ (showConstr
              "FunDef")
              [show x_a9OU, show x_a9OV, show x_a9OW, unK x_a9P0, unK x_a9P1])

instance (HFunctor a_a2rc, HFunctor b_a2rd, ShowHF a_a2rc, ShowHF b_a2rd) =>
          ShowHF (RecDef a_a2rc b_a2rd) where
  showHF
    (RecDef x_a9P5 x_a9P6 x_a9P7 x_a9Pb x_a9Pc)
    = (K $ (showConstr
              "RecDef")
              [show x_a9P5, show x_a9P6, show x_a9P7, unK x_a9Pb, unK x_a9Pc])

instance (HFunctor a_a2rX, ShowHF a_a2rX) => ShowHF (TypeDef a_a2rX) where
  showHF (TypeDef x_a9Pf x_a9Pg x_a9Ph)
    = (K $ (showConstr
              "TypeDef")
              [show x_a9Pf, show x_a9Pg, unK x_a9Ph])

instance ShowHF RecordOps where
  showHF (RecordCons x_a9Pn)
    = (K $ (showConstr
              "RecordCons")
              [(\x -> "[" ++ x ++ "]") . intercalate ", " $ 
                map (\(a, b) -> "(" ++ show a ++ ", " ++ unK b ++ ")") x_a9Pn])
  showHF (FieldAccess x_a9Po x_a9Pp)
    = (K $ (showConstr
              "FieldAccess")
              [unK x_a9Po, show x_a9Pp])
  showHF (FieldRemove x_a9Pq x_a9Pr)
    = (K $ (showConstr
              "FieldRemove")
              [unK x_a9Pq, show x_a9Pr])
  showHF (RecordMod x_a9Ps x_a9Pt)
    = (K $ (showConstr
              "RecordMod")
              [unK x_a9Ps, (\x -> "[" ++ x ++ "]") . intercalate ", " $ 
                map (\(a, b) -> "(" ++ show a ++ ", " ++ unK b ++ ")") x_a9Pt])
  showHF (RecordExt x_a9Pu x_a9Pv)
    = (K $ (showConstr
              "RecordExt")
              [unK x_a9Pu, (\x -> "[" ++ x ++ "]") . intercalate ", " $ 
                map (\(a, b) -> "(" ++ show a ++ ", " ++ unK b ++ ")") x_a9Pv])

instance (HFunctor a_a2vo, ShowHF a_a2vo) =>
          ShowHF (Match a_a2vo b_a2vp) where
  showHF (Match x_a9Pz x_a9PA)
    = (K $ (showConstr
              "Match")
              [unK x_a9Pz, (\x -> "[" ++ x ++ "]") . intercalate ", " $ 
                map (\(a, b) -> "(" ++ show a ++ ", " ++ unK b ++ ")") x_a9PA])

instance ShowHF SchemeInst where
  showHF (SchemeInst x_a9Uw x_a9Ux x_a9Uy)
    = (K $ (showConstr
              "SchemeInst")
              [unK x_a9Uw, show $ map unK x_a9Ux, show $ map unK x_a9Uy])

instance ShowHF (LabelExpr a_a2zX) where
  showHF (LabelLit x_aajp)
    = (K $ (showConstr
              "LabelLit")
              [show x_aajp])
  showHF (LabelApp x_aajq x_aajr)
    = (K $ (showConstr
              "LabelApp")
              [show x_aajq, unK x_aajr])