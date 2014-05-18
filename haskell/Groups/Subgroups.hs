module Math.Groups.Subgroups (subgroups, subgroupGens) where
 import Control.Monad (guard)
 import Data.Maybe (mapMaybe)
 import Data.IntSet (IntSet)
 import qualified Data.IntSet as ISet
 import Data.Map (Map)
 import qualified Data.Map as Map
 import Data.Set (Set)
 import qualified Data.Set as Set
 import Math.Groups.Types
 import Math.Groups.Internals

 subgroups :: Group -> Set IntSet  -- subgrs05b.hs
 -- returns a set of subgroups
 subgroups g = foldl (\subs c ->
   foldl (flip Set.insert) subs $ mapMaybe (addCycle c) $ Set.toList subs)
  (Map.keysSet cycles) (tail $ Map.toList cycles)
  where cycles = Map.fromListWith const [(ISet.fromList $ g_cycle g x, x)
					 | x <- g_elems g]
	addCycle (cyc, cg) subgr = do guard  $ ISet.notMember cg subgr
				      return $ newClosure2 (g_oper g) subgr
							   (ISet.toList cyc)

 subgroupGens :: Group -> Map IntSet (Set IntSet)  -- subgrs04b.hs
 -- returns a map from subgroups to sets of (minimal?) generating sets
 subgroupGens g = foldl (\subs c ->
  foldl (\m (s, g') -> Map.insertWith Set.union s g' m) subs
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
	 return (newClosure2 (g_oper g) subgr (ISet.toList cyc),
		 Set.fromList [ISet.filter(`ISet.notMember` cyc)a `ISet.union` b
			       | a <- Set.toList gens, b <- Set.toList gs])

 newClosure2 :: (Int -> Int -> Int) -> IntSet -> [Int] -> IntSet
 newClosure2 f seen new = closureS (close2 f)
  (new, foldl (flip ISet.insert) seen new)

 closureS :: ([Int] -> IntSet -> [Int]) -> ([Int], IntSet) -> IntSet
 closureS _ ([],  seen) = seen
 closureS f (new, seen) = closureS f $ view (f new seen) seen

 close2 :: (Int -> Int -> Int) -> [Int] -> IntSet -> [Int]
 close2 f new seen = concat [[f a b, f b a] | a <- ISet.toList seen, b <- new]
