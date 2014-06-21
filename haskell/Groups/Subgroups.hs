module Groups.Subgroups (subgroups, subgroupGens) where
 import Control.Monad (guard)
 import Data.Maybe (mapMaybe)
 import Data.Map (Map)
 import qualified Data.Map as Map
 import Data.Set (Set)
 import qualified Data.Set as Set
 import Groups.Types
 import Groups.Types.Subset(Subset(..), (∉), (⊆), (∪), (∖))
 import qualified Groups.Types.Subset as Sub
 import Groups.Ops (subgroupUnion)

 -- |Returns a 'Set' of all subgroups of the given 'Group'
 subgroups :: Group -> Set Subset
 subgroups g = foldl (\subs c ->
   foldl (flip Set.insert) subs $ mapMaybe (addCycle c) $ Set.toList subs)
  (Map.keysSet cycles) (tail $ Map.toList cycles)
  where cycles = Map.fromListWith const [(Sub.fromInts g $ g_cycle g x, x)
					 | x <- g_elems g]
	addCycle (cyc, cg) subgr = do guard  $ Element (cg, g) ∉ subgr
				      return $ subgroupUnion subgr cyc

 subgroupGens :: Group -> Map Subset (Set Subset)
 -- returns a map from subgroups to sets of (minimal?) generating sets
 subgroupGens g = foldl (\subs c ->
  foldl (\m (s, g') -> Map.insertWith Set.union s g' m) subs
   $ mapMaybe (addCycle c) $ Map.toList subs) cycles (tail $ Map.toList cycles)
  where cycles = Map.fromListWith Set.union [(Sub.fromInts g $ g_cycle g x,
					      Set.singleton$Sub.fromInts g [x])
					     | x <- g_elems g]
	addCycle (cyc, gs) (subgr, gens) = do
	 [cg] <- return $ Sub.toInts $ Set.findMin gs
	 guard $ Element (cg, g) ∉ subgr
	 case Set.minView $ Set.filter ((== 1) . Sub.size) gens of
	  Just (g', _) -> guard $ not $ g' ⊆ cyc
	  Nothing      -> return ()
	 return (subgroupUnion subgr cyc,
		 Set.fromList [(a ∖ cyc) ∪ b
			       | a <- Set.toList gens, b <- Set.toList gs])
