module Groups.Ops where
 import Data.Array
 import Data.Maybe (isJust)
 import qualified Data.IntSet as ISet
 import Groups.Types
 import Groups.Types.Subset (Subset(..))
 import qualified Groups.Types.Subset as Sub
 import Groups.Internals

 gexp :: Element -> Int -> Element  -- Move this to Element.hs?
 gexp (Element (i, g)) n = n' == 0 ?: identity g
				   :? Element (expfa (g_oper g) n'' i', g)
  where (inv, ordr) = gr_dat g ! i
	n' = mod n ordr
	(n'', i') = ordr - n' < div ordr 2 ?: (ordr - n', inv) :? (n', i)

 gcycle :: Element -> [Element]
 gcycle (Element (x,g)) = toElems g $ g_cycle g x

 gclosure :: Subset -> Subset
 gclosure (Subset (g,is))
  | ISet.null is = Sub.trivial g
  | otherwise    = Subset (g, closure' g (ISet.toList is, is))

 -- |@centers g h x@ tests whether @x@ commutes with every element of @h@
 centers :: Subset -> Int -> Bool
 centers h x = all (\j -> x `op` j == j `op` x) $ Sub.toInts h
  where op = g_oper $ Sub.getGroup h

 centralizer :: Subset -> Subset
 centralizer h = Sub.filter (centers h) $ Sub.total $ Sub.getGroup h

 center :: Group -> Subset
 center g = Sub.filter (centers g') g' where g' = Sub.total g

 isAbelian :: Group -> Bool
 -- TODO: Try to reimplement this using information about @g@'s family rather
 -- than brute force.
 isAbelian g = all (centers $ Sub.total g) $ g_elems g
 --isAbelian g = all [centers (Sub.fromInts g xs) x | x:xs <- tails $ g_elems g]

 -- |@norms h x@ tests whether @h@ is invariant under conjugation by @x@ (i.e.,
 -- whether @x@ normalizes @h@).  If @x@ is not in @h@'s group, 'False' is
 -- returned.
 norms :: Subset -> Int -> Bool
 norms h x = g_in x g && all (`Sub.inSubset` h) [g_conjugate g x i
						 | i <- Sub.toInts h]
  where g = Sub.getGroup h

 normalizer :: Subset -> Subset
 normalizer h = Sub.filter (norms h) $ Sub.total $ Sub.getGroup h

 -- |Tests whether a given 'Subset' (assumed to be a subgroup) is normal within
 -- its containing 'Group'.  Whether or not the subset is actually a subgroup
 -- is not checked.
 isNormal :: Subset -> Bool
 isNormal h = all (norms h) $ g_elems $ Sub.getGroup h

 isSubset :: [Element] -> Bool
 isSubset = isJust . getGroup'

 isSubgroup :: Subset -> Bool
 isSubgroup (Subset (g,h)) = all ((`ISet.member` h) . uncurry (g_oper g))
  $ cartesian (ISet.toList h) (ISet.toList h)

 nilpotence :: Group -> Maybe Int
 nilpotence g | g_size g == 1 = Just 0
 nilpotence g = nil 1 l lc
  where (l:lc) = lowerCentral g
	nil i g' (g'':xs) | g' == g'' = Nothing
			  | Sub.size g'' == 1 = Just i
			  | otherwise = nil (i+1) g'' xs
	nil _ _ [] = undefined

 lowerCentral :: Group -> [Subset]
 lowerCentral g = iterate (commutators $ Sub.total g) $ Sub.total g

 commutators :: Subset -> Subset -> Subset
 commutators (Subset (g1, is1)) (Subset (g2, is2))
  | g1 == g2 = let gop = g_oper g1
		   xs = [g_invert g1 (gop y x) `gop` gop x y
			 | x <- ISet.toList is1, y <- ISet.toList is2]
	       in Subset (g1, closure' g1 (xs, ISet.fromList xs))
  | otherwise = error "commutators: group mismatch"

 conjugacies :: Group -> [Subset]
 conjugacies g = Sub.trivial g : conj (ISet.fromDistinctAscList [1..g_size g-1])
  where conj left | ISet.null left = []
		  | otherwise = Subset (g, cc) : conj (ISet.difference left cc)
		   where least = ISet.findMin left
			 cc = ISet.map (\x -> g_conjugate g x least) total
	total = ISet.fromDistinctAscList [0..g_size g-1]

 -- |Given two subsets of the same group that are already closed under the group
 -- operation (i.e., that are subgroups; this precondition is not checked),
 -- 'subgroupUnion' computes the closure of their union.
 subgroupUnion :: Subset -> Subset -> Subset
 subgroupUnion (Subset (g, is)) (Subset (g', js))
  | g == g' = Subset (g, closureS (ISet.toList js, ISet.union is js))
  | otherwise = error "subgroupUnion: group mismatch"
  where closureS ([],  seen) = seen
	closureS (new, seen) = closureS $ view (f new seen) seen
	f new seen = concat [[g_oper g a b, g_oper g b a]
			     | a <- ISet.toList seen, b <- new]

 conjugate :: Element -> Element -> Element
 conjugate y x = y · x · inverse y
