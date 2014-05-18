-- TO ADD: ss_fromIntSet :: Group -> IntSet -> Subset
-- TO ADD: conversion to & from [Int]
-- TO ADD: ∈ ???

module Groups.Types.Subset where
 import Control.Monad (guard)
 import Data.IntSet (IntSet)
 import qualified Data.IntSet as ISet
 import Groups.Types.Elem
 import Groups.Types.Group

 newtype Subset = Subset (Group, IntSet) deriving (Eq, Ord, Read, Show)
  -- The constructor needs to be kept private!

 ss_group :: Subset -> Group
 ss_group (Subset (gr, _)) = gr

 ss_toIntSet :: Subset -> IntSet
 ss_toIntSet (Subset (_, is)) = is

 ss_elems :: Subset -> [Elem]
 ss_elems (Subset (gr, is)) = [Elem (i, gr) | i <- ISet.toList is]

 ss_fromElems :: [Elem] -> Maybe Subset
 ss_fromElems [] = Nothing
 ss_fromElems xs@(Elem (_,g) : _) = fmap (\els -> Subset (g, ISet.fromList els)) $ fromels xs
  where fromels (Elem (y,h) : ys) = guard (g == h) >> fmap (y :) (fromels ys)
	fromels [] = Just []

 ss_trivial :: Group -> Subset
 ss_trivial g = Subset (g, ISet.singleton 0)

 ss_empty :: Group -> Subset
 ss_empty g = Subset (g, ISet.empty)

 ss_all :: Group -> Subset
 ss_all g = Subset (g, ISet.fromDistinctAscList $ g_elems g)

 ss_size :: Subset -> Int
 ss_size (Subset (_, is)) = ISet.size is

 (∪) :: Subset -> Subset -> Subset
 Subset (gr1, is1) ∪ Subset (gr2, is2)
  | gr1 == gr2 = Subset (gr1, ISet.union is1 is2)
  | otherwise = error "Subset.∪: group mismatch"

 (∩) :: Subset -> Subset -> Subset
 Subset (gr1, is1) ∩ Subset (gr2, is2)
  | gr1 == gr2 = Subset (gr1, ISet.intersection is1 is2)
  | otherwise = error "Subset.∩: group mismatch"

 (∖) :: Subset -> Subset -> Subset
 Subset (gr1, is1) ∖ Subset (gr2, is2)
  | gr1 == gr2 = Subset (gr1, ISet.difference is1 is2)
  | otherwise = error "Subset.∖: group mismatch"
