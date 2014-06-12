import Data.Array
import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet
import qualified Data.Set as Set
import Groups
import Groups.Subgroups (subgroups)

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
			  | ((j,i), True) <- assocs graph]
	  putStrLn "}"
 where subsSet = subgroups group
       subsDex = listArray (0, Set.size subsSet - 1) $ Set.toAscList subsSet
       graph = lattice subsDex
       byOrdr = classify (ISet.size . (subsDex !)) $ indices subsDex

lattice :: Ix a => Array a IntSet -> Array (a,a) Bool
-- For all ((h,g), tf) in the return value, tf is True iff g covers h under the
-- subgroup relation (i.e., h<g and there is no k such that h<k<g).
lattice subgrs = fst $ until (null . snd) (\(graph, (i,ss):xs) ->
	(graph // concat [((s,g), True) : [((h,g), False) | h <- below graph s]
			  | s <- ss, (gn, gxs) <- xs, mod gn i == 0, g <- gxs,
			    ISet.isSubsetOf (subgrs ! s) (subgrs ! g)], xs))
	(listArray ((a,a), (b,b)) $ repeat False, byOrdr)
 where byOrdr = classify (ISet.size . (subgrs !)) $ indices subgrs
       (a,b) = bounds subgrs
       below tbl g = [h | ((h, g'), True) <- assocs tbl, g' == g]

isNormal' :: Group -> IntSet -> Bool
isNormal' g h = all norms $ g_elems g
 where (⋅) = g_oper g
       norms x = all (`ISet.member` h) $ map ((⋅ g_invert g x) . (x ⋅)) 
				       $ ISet.toList h

classify :: Ord b => (a -> b) -> [a] -> [(b, [a])]
classify f = foldr shove [] . map (\x -> (f x, x))
 where shove (b', a) [] = [(b', [a])]
       shove (b', a) ((b, as):xs) | b' == b = (b, a:as) : xs
				  | b' <  b = (b', [a]) : (b, as) : xs
				  | b' >  b = (b, as) : shove (b', a) xs
       shove _ _ = undefined  -- to stop the compilation warnings
-- classify f xs = Map.toList $ Map.fromListWith (flip (++)) [(f x, [x]) | x <- xs]
