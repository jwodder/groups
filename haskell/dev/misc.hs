 mkelem :: Group' a -> a -> Element
 mkelem g x = Element (g'index g x, mkgroup g)

 newtype Subgroup = Subgroup (Subset, Group)

 newtype Aut = Aut (Group, Array (Int, Int) Int, Group)
  -- Fields:
  -- * structure of the automorphism group
  -- * mapping from automorphism IDs to group IDs to automorphed group IDs
  -- * group of which this is the Aut

 (≤) :: [Element] -> Group -> Bool
 (⊴) :: [Element] -> Group -> Bool
 quotient :: Group -> [Element] -> Group
 mksubgroup :: [Element] -> Group
 (⨯) = direct

 -- Given a group $G$ and a subgroup $H$, returns a list of arbitrarily-chosen
 -- representatives of left cosets of $H$ (in ascending order) and a mapping
 -- from elements of $G$ to those representatives; not for export
 cosetify :: Group -> [Int] -> ([Int], Array Int Int)
 cosetify g h = cosify $ listArray (bounds $ gr_dat g) $ repeat Nothing
  where cosify c = case [x | (x, Nothing) <- assocs c] of
	 x:_ -> x &: cosify (c // [(y, Just x) | y <- map (g_oper g x) h])
	 []  -> ([], fmap fromJust c)

 lattice :: Array Int IntSet -> (Array (Int, Int) Bool, Array (Int, Int) Bool)
 lattice subgrs = (graphTbl, ixmap (bounds graphTbl) (\(g,h) -> (h,g)) graphTbl)
  where byOrdr = classify (ISet.size . (subgrs !)) $ indices subgrs
	-- For all ((h,g), tf) in graphTbl, tf is True iff g covers h under the
	-- subgroup relation.
	graphTbl = fst $ until (null . snd) (\(graph, (i,ss):xs) ->
	 (graph // concat [((s,g), True) : [((h,g), False) | h <- below graph s]
			   | s <- ss, (gn, gxs) <- xs, mod gn i == 0, g <- gxs,
			     ISet.isSubsetOf (subgrs ! s) (subgrs ! g)], xs))
	 (listArray ((0,0), (n,n)) $ repeat False, byOrdr)
	n = snd $ bounds subgrs
	below tbl g = [h | ((h, g'), True) <- assocs tbl, g' == g]

 data GroupType = Trivial
		| Boolean
		| Klein4
		| Cyclic      Int
		| MultiplicN  Int
		| HolCyclic   Int
		| CycSemiCyc  Int Int Int
		| Dicyclic    Int
		| Dihedral    Int
		| Symmetric   Int
		| Alternating Int
		| Direct      GroupType GroupType
		| Semidirect  GroupType GroupType ???
		  -- A new type should probably be used for the φ of semidirect
		  -- products so that GroupType can belong to Eq.
		 deriving (Eq, Ord, Read, Show)
