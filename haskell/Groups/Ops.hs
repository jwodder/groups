module Groups.Ops where
 import Data.Array
 import Data.Maybe (isJust)
 import qualified Data.IntSet as ISet
 import Groups.Types
 import Groups.Types.Subset (Subset(..))
 import qualified Groups.Types.Subset as Sub
 import Groups.Internals

 gexp :: Element -> Int -> Element
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
 norms h x = g_in x g && all (`Sub.inSubset` h) [g_oper g (g_oper g x i) x'
						 | i <- Sub.toInts h]
  where g = Sub.getGroup h
	x' = g_invert g x

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
 nilpotence g = nil 1 whole $ tail lowerCentral
  where whole = Sub.total g
	lowerCentral = iterate (commutators whole) whole
	nil i g' (g'':xs) | g' == g'' = Nothing
			  | Sub.size g'' == 1 = Just i
			  | otherwise = nil (i+1) g'' xs
	nil _ _ [] = undefined

 commutators :: Subset -> Subset -> Subset
 commutators (Subset (g1, is1)) (Subset (g2, is2))
  | g1 == g2 = let xs = [g_oper g1 (g_invert g1 (g_oper g1 y x)) (g_oper g1 x y)
			 | x <- ISet.toList is1, y <- ISet.toList is2]
	       in Subset (g1, closure' g1 (xs, ISet.fromList xs))
  | otherwise = error "commutators: group mismatch"

 conjugacies :: Group -> [Subset]
 conjugacies g = Sub.trivial g : conj (ISet.fromDistinctAscList [1..g_size g-1])
  where conj left | ISet.null left = []
		  | otherwise = Subset (g, cc) : conj (ISet.difference left cc)
		   where least = ISet.findMin left
			 (gop, ginv) = (g_oper g, g_invert g)
			 cc = ISet.map (\x -> x `gop` least `gop` ginv x) left
