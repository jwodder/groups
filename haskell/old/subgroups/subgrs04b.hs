import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Groups
import Subutils

main :: IO ()
main = mapM_ (\(subgr, gens) -> do
  putStrLn $ bracket "{}" $ ISet.toList subgr
--mapM_ (putStrLn . (" = " ++) . bracket "⟨⟩" . Set.toList) $ Set.toList gens
 ) (Map.toList $ subgroups group)

subgroups :: Group -> Map IntSet (Set IntSet)
subgroups g = foldl (\subs c ->
 foldl (\m (s, g') -> Map.insertWith Set.union s g' m) subs
  $ mapMaybe (addCycle c) $ Map.toList subs) cycles (tail $ Map.toList cycles)
 where cycles = Map.fromListWith Set.union [(ISet.fromList $ g_cycle g x, Set.singleton $ ISet.singleton x) | x <- g_elems g]
       addCycle (cyc, gs) (subgr, gens) = do
	[cg] <- return $ ISet.toList $ Set.findMin gs
	guard $ ISet.notMember cg subgr
	case Set.minView $ Set.filter ((== 1) . ISet.size) gens of
	 Just (g', _) -> guard $ not $ ISet.isSubsetOf g' cyc
	 Nothing      -> return ()
	return (newClosure2 (g_oper g) subgr (ISet.toList cyc),
	       Set.fromList [ISet.filter (`ISet.notMember` cyc) a `ISet.union` b
			     | a <- Set.toList gens, b <- Set.toList gs])
