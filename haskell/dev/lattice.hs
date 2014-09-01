-- TODO: It seems that, if two adjacent subgraphs have no lines between them
-- (e.g., the 8 and 6 subgraphs in the S_4 lattice), then `dot` will draw them
-- at the same height/rank.  Try to keep this from happening.

import Data.Array
import qualified Data.Set as Set
import Groups
import Groups.Subgroups (subgroups)
import Permutation (Permutation)

group :: Group Permutation
--group = cycSemiCyc 8 4 (-1)
--group = cycSemiCyc 8 4 3
group = symmetric 4

main :: IO ()
main = do putStrLn "graph {"
	  mapM_ (\(order, subs) -> do
	   putStrLn $ " subgraph order" ++ show order ++ " {"
	   putStrLn "  rank = \"same\""
	   mapM_ (\i -> putStrLn $ "  s" ++ show i ++ " [shape = \"point\"" ++
	    (if isNormal group (subsDex ! i) then "]"
	     else ", fillcolor = \"white\"]")) subs
	   putStrLn " }") byOrdr
	  mapM_ putStrLn [' ' : 's' : show i ++ " -- s" ++ show j
			  | ((j,i), True) <- assocs graph]
	  putStrLn "}"
 where subsSet = subgroups group
       subsDex = listArray (0, Set.size subsSet - 1) $ Set.toAscList subsSet
       graph = lattice subsDex
       byOrdr = classify (Set.size . (subsDex !)) $ indices subsDex

-- |Given a set of distinct subgroups of a single group, 'lattice' associates
-- each pair @(g,h)@ of subgroups with 'True' if & only if @h@ covers @g@ under
-- the subgroup relation &#x2014; i.e., iff @g@ is a proper subgroup of @h@ and
-- there is no other proper subgroup of @h@ that @g@ is also a subgroup of.
-- Each subgroup is associated with an 'Ix' value by the supplied input 'Array'
-- for ease of structuring the output data.
lattice :: (Ix a, Ord b) => Array a (Set.Set b) -> Array (a,a) Bool
lattice subgrs = fst $ until (null . snd) (\(graph, (i,ss):xs) ->
 (graph // concat [((s,g), True) : [((h,g), False) | h <- below graph s]
		   | s <- ss, (gn, gxs) <- xs, mod gn i == 0, g <- gxs,
		     Set.isSubsetOf (subgrs ! s) (subgrs ! g)], xs))
 (listArray ((a,a), (b,b)) $ repeat False, byOrdr)
 where byOrdr = classify (Set.size . (subgrs !)) $ indices subgrs
       (a,b) = bounds subgrs
       below tbl g = [h | ((h, g'), True) <- assocs tbl, g' == g]

classify :: Ord b => (a -> b) -> [a] -> [(b, [a])]
classify f = foldr shove [] . map (\x -> (f x, x))
 where shove (b', a) [] = [(b', [a])]
       shove (b', a) ((b, as):xs) | b' == b = (b, a:as) : xs
				  | b' <  b = (b', [a]) : (b, as) : xs
				  | b' >  b = (b, as) : shove (b', a) xs
       shove _ _ = undefined  -- to stop the compilation warnings
-- classify f xs = Map.toList $ Map.fromListWith (flip (++)) [(f x, [x]) | x <- xs]
