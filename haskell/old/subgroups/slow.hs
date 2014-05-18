-- This is slower than subgroups01.hs:
subgroups02 :: Group -> Set IntSet
subgroups02 g = closure2' (\a b -> closure2'' (g_oper g) $ ISet.union a b)
 {- $ nub -} $ map (ISet.fromList . g_cycle g) $ g_elems g

-- This is much, much slower than subgroups01.hs:
subgroups03 :: Group -> Set IntSet
subgroups03 g = Set.fromList $ map (closure2'' (g_oper g) . ISet.unions)
 $ filter (not . null) $ subsequences $ nub $ map (ISet.fromList . g_cycle g)
 $ g_elems g
