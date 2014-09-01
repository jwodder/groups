 newtype Subgroup a = Subgroup (Group a) (Set a)

 newtype NormalSub a = NormalSub (Subgroup a)

 newtype Homomorph a b = HM (Group a, Group b, Array a b)

 newtype Aut a = Aut (Group ???, Array (???, a) a, Group a)
  -- Fields:
  -- * structure of the automorphism group
  -- * mapping from automorphism elements to group elements to automorphed
  --   group elements
  -- * group of which this is the Aut

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
