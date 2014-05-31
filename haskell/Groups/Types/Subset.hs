module Groups.Types.Subset where
 import Data.Array (inRange, bounds)
 import Data.IntSet (IntSet)
 import qualified Data.IntSet as ISet
 import Groups.Types.Element (Element(..))
 import qualified Groups.Types.Element as E
 import Groups.Types.Group
 import qualified Groups.Internals

 newtype Subset = Subset (Group, IntSet) deriving (Eq, Ord, Read, Show)
  -- TODO: This constructor needs to be kept private!

 getGroup :: Subset -> Group
 getGroup (Subset (gr, _)) = gr

 toIntSet :: Subset -> IntSet
 toIntSet (Subset (_, is)) = is

 fromIntSet :: Group -> IntSet -> Subset
 fromIntSet g is = Subset (g, ISet.filter (inRange $ bounds $ gr_dat g) is)

 toInts :: Subset -> [Int]
 toInts (Subset (_, is)) = ISet.toList is

 fromInts :: Group -> [Int] -> Subset
 fromInts g is = Subset (g, ISet.fromList $ Prelude.filter (inRange $ bounds $ gr_dat g) is)

 elements :: Subset -> [Element]
 elements (Subset (gr, is)) = [Element (i, gr) | i <- ISet.toList is]

 fromElems :: [Element] -> Maybe Subset
 fromElems [] = Nothing
 fromElems xs = do (ys, g) <- Groups.Internals.fromElems xs
		   return $ Subset (g, ISet.fromList ys)

 empty :: Group -> Subset
 empty g = Subset (g, ISet.empty)

 trivial :: Group -> Subset
 trivial g = Subset (g, ISet.singleton 0)

 total :: Group -> Subset
 total g = Subset (g, ISet.fromDistinctAscList $ g_elems g)

 size :: Subset -> Int
 size (Subset (_, is)) = ISet.size is

 inSubset :: Int -> Subset -> Bool
 -- Consider renaming this.
 inSubset x (Subset (_, is)) = ISet.member x is

 (∈) :: Element -> Subset -> Bool
 -- Consider renaming this.
 x ∈ Subset (g, is) = E.getGroup x == g && ISet.member (E.elemID x) is

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
  | otherwise = Subset (gr1, is1)

 filter :: (Int -> Bool) -> Subset -> Subset
 filter p (Subset (g, is)) = Subset (g, ISet.filter p is)

 filter' :: (Element -> Bool) -> Subset -> Subset
 filter' p (Subset (g, is)) = Subset (g, ISet.filter (\x -> p $ Element (x,g)) is)

 (⊆) :: Subset -> Subset -> Bool
 Subset (gr1, is1) ⊆ Subset (gr2, is2) = gr1 == gr2 && ISet.isSubsetOf is1 is2

 (⊇) :: Subset -> Subset -> Bool
 (⊇) = flip (⊆)

 (⊂) :: Subset -> Subset -> Bool
 Subset (gr1, is1) ⊂ Subset (gr2, is2) = gr1 == gr2 && ISet.isProperSubsetOf is1 is2

 (⊃) :: Subset -> Subset -> Bool
 (⊃) = flip (⊂)
