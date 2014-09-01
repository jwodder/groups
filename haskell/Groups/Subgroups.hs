module Groups.Subgroups (subgroups, subgroupGens) where
 import Control.Monad (guard)
 import Data.Maybe (mapMaybe)
 import Data.Map (Map)
 import qualified Data.Map as Map
 import Data.Set (Set)
 import qualified Data.Set as Set
 import Groups.Type
 import Groups.Ops (gcycle, subgroupUnion)

 -- |Returns a 'Set' of all subgroups of the given 'Group'
 subgroups :: Ord a => Group a -> Set (Set a)  -- subgrs05b.hs
 subgroups g = foldl (\subs c ->
   foldl (flip Set.insert) subs $ mapMaybe (addCycle c) $ Set.toList subs)
  (Map.keysSet cycles) (tail $ Map.toList cycles)
  where cycles = Map.fromListWith const [(Set.fromList $ gcycle g x, x)
					 | x <- gelems g]
	addCycle (cyc, cg) subgr = do guard  $ Set.notMember cg subgr
				      return $ subgroupUnion g subgr cyc

 subgroupGens :: Ord a => Group a -> Map (Set a) (Set (Set a))
 -- returns a map from subgroups to sets of (minimal?) generating sets
 subgroupGens g = foldl (\subs c ->
  foldl (\m (s, g') -> Map.insertWith Set.union s g' m) subs
   $ mapMaybe (addCycle c) $ Map.toList subs) cycles (tail $ Map.toList cycles)
  where cycles = Map.fromListWith Set.union [(Set.fromList $ gcycle g x,
					      Set.singleton $ Set.singleton x)
					     | x <- gelems g]
	addCycle (cyc, gs) (subgr, gens) = do
	 [cg] <- return $ Set.toList $ Set.findMin gs
	 guard $ Set.notMember cg subgr
	 case Set.minView $ Set.filter ((== 1) . Set.size) gens of
	  Just (g', _) -> guard $ not $ Set.isSubsetOf g' cyc
	  Nothing      -> return ()
	 return (subgroupUnion g subgr cyc,
		 Set.fromList [Set.difference a cyc `Set.union` b
			       | a <- Set.toList gens, b <- Set.toList gs])
