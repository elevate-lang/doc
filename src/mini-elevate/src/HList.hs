{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DataKinds, GADTs, PolyKinds, ScopedTypeVariables, ExplicitForAll, TypeApplications, AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeOperators, MultiParamTypeClasses, FunctionalDependencies #-}

module HList where

import GHC.TypeLits
import Data.Kind

class HListLike (d :: [Type] -> Type) where
  hNil :: d '[]
  hCons :: a -> d ts -> d (a ': ts)
  getHead :: d (a ': ts) -> a
  getTail :: d (a ': ts) -> d ts

data Status = Found | NotFound

class AccessFail (s :: Status) a

class NotOccurs a (ts :: [Type])

instance NotOccurs a '[]

instance (NotOccurs a ts) => NotOccurs a (b ': ts)

instance (AccessFail Found a) => NotOccurs a (a ': ts)

class (HListLike d) => Occurs a (ts :: [Type]) d where
  occurs :: d ts -> a

instance (AccessFail NotFound a, HListLike d) => Occurs a '[] d where
  occurs = error "should be a type error"

instance {-# OVERLAPS #-} (NotOccurs a ts, HListLike d) => Occurs a (a ': ts) d where
  occurs = getHead

instance {-# OVERLAPS #-} (Occurs a ts d, HListLike d) => Occurs a (b ': ts) d where
  occurs = occurs . getTail

class (HListLike d, Occurs a ts d) => Update a (ts :: [Type]) d where
  update :: (a -> a) -> d ts -> d ts

instance (AccessFail NotFound a, HListLike d) => Update a '[] d where
  update = error "should be a type error"

instance {-# OVERLAPS #-} (NotOccurs a ts, HListLike d) => Update a (a ': ts) d where
  update f ls = hCons (f (getHead ls)) (getTail ls)

instance {-# OVERLAPS #-} (Update a ts d, HListLike d) => Update a (b ': ts) d where
  update f ls = hCons (getHead ls) (update f (getTail ls))

infixr 5 :|

data HList :: [Type] -> Type where
  HNil :: HList '[]
  (:|) :: a -> HList ts -> HList (a ': ts)

instance HListLike HList where
  hNil = HNil
  hCons = (:|)
  getHead (a :| _) = a
  getTail (_ :| ls) = ls

data Field :: Symbol -> Type -> Type where
  Field :: forall s a. a -> Field s a

type s :- a = Field s a

select :: forall s a ts. (Occurs (Field s a) ts HList) => HList ts -> a
select ls = case occurs ls :: Field s a of Field a -> a

modify :: forall s a ts. (Update (Field s a) ts HList) => (a -> a) -> HList ts -> HList ts
modify f = update ((\(Field a) -> Field (f a)) :: Field s a -> Field s a)
