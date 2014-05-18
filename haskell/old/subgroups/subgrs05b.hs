-- This is about the same speed as subgrs04.hs, but it doesn't track generating
-- sets.
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Math.Groups
import Subutils

main = mapM_ (putStrLn . bracket "{}" . ISet.toList)
 $ Set.toList $ subgroups group

subgroups :: Group -> Set IntSet
subgroups g = foldl (\subs c ->
  foldl (flip Set.insert) subs $ mapMaybe (addCycle c) $ Set.toList subs)
 (Map.keysSet cycles) (tail $ Map.toList cycles)
 where cycles = Map.fromListWith const [(ISet.fromList $ g_cycle g x, x)
					| x <- g_elems g]
       addCycle (cyc, cg) subgr = do
	guard  $ ISet.notMember cg subgr
	return $ newClosure2 (g_oper g) subgr (ISet.toList cyc)
