module Groups.Ops where
 import Data.Set (Set)
 import qualified Data.Set as Set
 import Closure (closureS)
 import Groups.Types
 import Groups.Internals

 gexp :: Group a -> a -> Int -> a
 gexp g x n = n' == 0 ?: gid g :? expfa (goper g) n'' x'
  where ordr = gorder g x
	n' = mod n ordr
	(n'', x') = ordr-n' < div ordr 2 ?: (ordr-n', ginvert g x) :? (n', x)

 gcycle :: Eq a => Group a -> a -> [a]
 gcycle g x = gid g : takeWhile (/= gid g) (iterate (goper g x) x)

 gclosure :: Ord a => Group a -> Set a -> Set a
 gclosure g xs | Set.null xs = gtrivial g
	       | otherwise   = closure' g (Set.toList xs, xs)

 -- |@centers g h x@ tests whether @x@ commutes with every element of @h@
 centers :: Eq a => Group a -> Set a -> a -> Bool
 centers g h x = all (\j -> x `op` j == j `op` x) $ Set.toList h
  where op = goper g

 centralizer :: Eq a => Group a -> Set a -> Set a
 centralizer g h = Set.fromDistinctAscList $ filter (centers g h) $ gelems g

 center :: Eq a => Group a -> Set a
 center g = Set.fromDistinctAscList $ filter (centers g $ gtotal g) $ gelems g

 isAbelian :: Eq a => Group a -> Bool
 -- TODO: Try to reimplement this using information about @g@'s family rather
 -- than brute force.
 isAbelian g = abel $ tail $ gelems g
  where abel [] = True
	abel [_] = True
	abel (x:xs) = all (\y -> goper g x y == goper g y x) xs && abel xs

 -- |@norms g h x@ tests whether @h@ is invariant under conjugation by @x@
 -- (i.e., whether @x@ normalizes @h@).
 norms :: Ord a => Group a -> Set a -> a -> Bool
 {- -- If @x@ is not in @h@'s group, 'False' is returned. -}
 norms g h x = {- g_in x g && -} all ((`Set.member` h) . gconjugate g x)
				     (Set.toList h)

 normalizer :: Ord a => Group a -> Set a -> Set a
 normalizer g h = Set.fromDistinctAscList $ filter (norms g h) $ gelems g

 -- |Tests whether a given subset (assumed to be a subgroup) is normal within
 -- its containing group.  Whether or not the subset is actually a subgroup is
 -- not checked.
 isNormal :: Ord a => Group a -> Set a -> Bool
 isNormal g h = all (norms g h) $ gelems g

 isSubgroup :: Ord a => Group a -> Set a -> Bool
 isSubgroup g h = all ((`Set.member` h) . uncurry (goper g))
		   $ cartesian (Set.toList h) (Set.toList h)

 nilpotence :: Ord a => Group a -> Maybe Int
 nilpotence g | gsize g == 1 = Just 0
 nilpotence g = nil 1 l lc
  where (l:lc) = lowerCentral g
	nil i g' (g'':xs) | g' == g'' = Nothing
			  | Set.size g'' == 1 = Just i
			  | otherwise = nil (i+1) g'' xs
	nil _ _ [] = undefined

 lowerCentral :: Ord a => Group a -> [Set a]
 lowerCentral g = iterate (commutators g $ gtotal g) $ gtotal g

 commutators :: Ord a => Group a -> Set a -> Set a -> Set a
 commutators g as bs = closure' g (xs, Set.fromList xs)
  where gop = goper g
	xs = [ginvert g (gop y x) `gop` gop x y
	      | x <- Set.toList as, y <- Set.toList bs]

 conjugacies :: Ord a => Group a -> [Set a]
 conjugacies g = gtrivial g : conj (Set.fromDistinctAscList $ tail $ gelems g)
  where conj left | Set.null left = []
		  | otherwise     = cc : conj (Set.difference left cc)
		   where least = Set.findMin left
			 cc    = Set.map (\x -> gconjugate g x least) $ gtotal g

 -- |Given two subsets of the same group that are already closed under the group
 -- operation (i.e., that are subgroups; this precondition is not checked),
 -- 'subgroupUnion' computes the closure of their union.
 subgroupUnion :: Ord a => Group a -> Set a -> Set a -> Set a
 subgroupUnion g xs ys = closureS f (Set.toList ys, Set.union xs ys)
  where f new seen = concat [[goper g a b, goper g b a]
			     | a <- Set.toList seen, b <- new]

 gexponent :: Group a -> Int
 gexponent g = foldl lcm 1 $ map (gorder g) $ gelems g

-- TO ADD: isSubset
