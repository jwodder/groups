module Groups.Types.Subset where
 import Data.IntSet (IntSet)
 import qualified Data.IntSet as ISet
 import Groups.Types.Element
 import Groups.Types.Group
 import qualified Groups.Internals

 newtype Subset = Subset (Group, IntSet) deriving (Eq, Ord, Read, Show)
  -- TODO: This constructor needs to be kept private!

 getGroup :: Subset -> Group
 getGroup (Subset (gr, _)) = gr

 toIntSet :: Subset -> IntSet
 toIntSet (Subset (_, is)) = is

 elements :: Subset -> [Element]
 elements (Subset (gr, is)) = [Element (i, gr) | i <- ISet.toList is]

 fromElems :: [Element] -> Maybe Subset
 fromElems [] = Nothing
 fromElems xs = do (ys, g) <- Groups.Internals.fromElems xs
		   return $ Subset (g, ISet.fromList ys)

 trivial :: Group -> Subset
 trivial g = Subset (g, ISet.singleton 0)

 empty :: Group -> Subset
 empty g = Subset (g, ISet.empty)

 total :: Group -> Subset
 total g = Subset (g, ISet.fromDistinctAscList $ g_elems g)

 size :: Subset -> Int
 size (Subset (_, is)) = ISet.size is

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
