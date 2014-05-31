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
