module Label where

import qualified Data.Set as Set
import Data.List

newtype Label = Label String deriving (Eq, Ord)

newtype Labels = Labels (Set.Set Label) deriving (Eq, Ord)

instance Show Label where
  show (Label l) = l

instance Show Labels where
  show (Labels ls) = intercalate ", " (map show (Set.toList ls))

label :: String -> Label
label = Label

labels :: Set.Set Label -> Labels
labels = Labels

null :: Labels -> Bool
null (Labels a) = Set.null a

fromList :: [Label] -> Labels
fromList ls = Labels (Set.fromList ls)

empty :: Labels
empty = Labels (Set.empty)

singleton :: Label -> Labels
singleton l = Labels (Set.singleton l)

isSubsetOf :: Labels -> Labels -> Bool
isSubsetOf (Labels a) (Labels b) = Set.isSubsetOf a b

disjoint :: Labels -> Labels -> Bool
disjoint (Labels a) (Labels b) = Set.disjoint a b

difference :: Labels -> Labels -> Labels
difference (Labels a) (Labels b) = Labels (Set.difference a b)

union :: Labels -> Labels -> Labels
union (Labels a) (Labels b) = Labels (Set.union a b)

intersection :: Labels -> Labels -> Labels
intersection (Labels a) (Labels b) = Labels (Set.intersection a b)