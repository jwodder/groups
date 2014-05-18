-- This is about the same speed as subgrs04.hs and subgrs05.hs (at least when
-- run on S_5), and it doesn't track generating sets.
import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import Data.Set (Set)
import qualified Data.Set as Set
import Math.Groups
import Subutils

main = mapM_ (putStrLn . bracket "{}" . ISet.toList)
 $ Set.toList $ subgroups group

subgroups :: Group -> Set IntSet
subgroups g = foldl (\subs c ->
  --foldl (flip Set.insert) subs $ map (addCycle c) $ Set.toList subs
  Set.union subs $ Set.map (addCycle c) subs
 ) cycles (tail $ Set.toList cycles)
 where cycles = Set.fromList $ map (ISet.fromList . g_cycle g) $ g_elems g
       addCycle cyc subgr = newClosure2 (g_oper g) subgr (ISet.toList cyc)
