{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts #-}

module Util where

import Data.Comp.Multi

lCase :: forall g i f y. (f :<: g) => Term g i -> (f (Term g) i -> y) -> Maybe y
lCase x f = case project x :: Maybe (f (Term g) i) of
  Just x' -> Just (f x')
  Nothing -> Nothing

runCase :: [Maybe a] -> a
runCase ls = case [x | Just x <- ls] of
  (a : _) -> a
  _ -> error "Unexpected Case"

runCaseOn :: a -> [a -> Maybe b] -> b
runCaseOn a = runCase . map ($ a)

byDefault :: a -> b -> Maybe a
byDefault = const . Just