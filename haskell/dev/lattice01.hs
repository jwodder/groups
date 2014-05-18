import Data.Array
import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import qualified Data.Set as Set
import Math.Groups
import Math.Groups.Subgroups (subgroups)
import MoreData.Lists (classify)

--group = cycSemiCyc 8 4 (-1)
group = cycSemiCyc 8 4 3

main = do putStrLn "graph {"
	  mapM_ (\(order, subs) -> do
	   putStrLn $ " subgraph order" ++ show order ++ " {"
	   putStrLn "  rank = \"same\""
	   mapM_ (\i -> putStrLn $ "  s" ++ show i ++ " [shape = \"point\"" ++
	    (if isNormal' group $ subsDex ! i then "]"
	     else ", fillcolor = \"white\"]")) subs
	   putStrLn " }") byOrdr
	  mapM_ putStrLn [' ' : 's' : show i ++ " -- s" ++ show j
			  | ((i,j), True) <- assocs graph]
	  putStrLn "}"
 where subsSet = subgroups group
       subsDex = listArray (0, Set.size subsSet - 1) $ Set.toAscList subsSet
       (_, graph) = lattice subsDex
       byOrdr = classify (ISet.size . (subsDex !)) $ indices subsDex

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

isNormal' :: Group -> IntSet -> Bool
isNormal' g h = all norms $ g_elems g
 where (⋅) = g_oper g
       norms x = all (`ISet.member` h) $ map ((⋅ g_invert g x) . (x ⋅)) 
				       $ ISet.toList h
