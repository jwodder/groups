module Groups.Subgroups (subgroups, subgroupGens) where
 import Control.Monad (guard)
 import Data.Maybe (mapMaybe)
 import Data.IntSet (IntSet)
 import qualified Data.IntSet as ISet
 import Data.Map (Map)
 import qualified Data.Map as Map
 import Data.Set (Set)
 import qualified Data.Set as Set
 import Groups.Types
 import Groups.Types.Subset(Subset(..), fromIntSet)
 import Groups.Internals

 -- |Returns a 'Set' of all subgroups of the given 'Group'
 subgroups :: Group -> Set Subset  -- subgrs05b.hs
 subgroups g = Set.map (\s -> Subset (g,s)) $ foldl (\subs c ->
   foldl (flip Set.insert) subs $ mapMaybe (addCycle c) $ Set.toList subs)
  (Map.keysSet cycles) (tail $ Map.toList cycles)
  where cycles = Map.fromListWith const [(ISet.fromList $ g_cycle g x, x)
					 | x <- g_elems g]
	addCycle (cyc, cg) subgr = do guard  $ ISet.notMember cg subgr
				      return $ newClosure2 (g_oper g) subgr cyc

 subgroupGens :: Group -> Map Subset (Set Subset)
 -- returns a map from subgroups to sets of (minimal?) generating sets
 subgroupGens g = Map.mapKeysMonotonic (fromIntSet g) 
  $ fmap (Set.map (\s -> Subset (g,s)))
  $ foldl (\subs c -> foldl (\m (s, g') -> Map.insertWith Set.union s g' m) subs
   $ mapMaybe (addCycle c) $ Map.toList subs) cycles (tail $ Map.toList cycles)
  where cycles = Map.fromListWith Set.union [(ISet.fromList $ g_cycle g x,
					      Set.singleton $ ISet.singleton x)
					     | x <- g_elems g]
	addCycle (cyc, gs) (subgr, gens) = do
	 [cg] <- return $ ISet.toList $ Set.findMin gs
	 guard $ ISet.notMember cg subgr
	 case Set.minView $ Set.filter ((== 1) . ISet.size) gens of
	  Just (g', _) -> guard $ not $ ISet.isSubsetOf g' cyc
	  Nothing      -> return ()
	 return (newClosure2 (g_oper g) subgr cyc,
		 Set.fromList [ISet.difference a cyc `ISet.union` b
			       | a <- Set.toList gens, b <- Set.toList gs])

 newClosure2 :: (Int -> Int -> Int) -> IntSet -> IntSet -> IntSet
 -- Given two sets that are already closed under `f`, `newClosure2` computes
 -- the closure of their union
 newClosure2 f seen new = closureS (close2 f)
				   (ISet.toList new, ISet.union seen new)

 closureS :: ([Int] -> IntSet -> [Int]) -> ([Int], IntSet) -> IntSet
 closureS _ ([],  seen) = seen
 closureS f (new, seen) = closureS f $ view (f new seen) seen

 close2 :: (Int -> Int -> Int) -> [Int] -> IntSet -> [Int]
 close2 f new seen = concat [[f a b, f b a] | a <- ISet.toList seen, b <- new]
