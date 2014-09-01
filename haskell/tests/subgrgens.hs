import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Groups (Group, symmetric)
import Groups.Subgroups (subgroupGens)
import Permutation (Permutation, showCycles)

gr :: Group Permutation
gr = symmetric 4

showElem :: Permutation -> String
showElem = showCycles

showElemSet :: Set.Set Permutation -> String
showElemSet xs = '{' : intercalate ", " (map showElem $ Set.toList xs) ++ "}"

main :: IO ()
main = mapM_ (\(subgr, gens) -> do putStrLn $ showElemSet subgr
				   mapM_ (\g -> putStrLn $ ' ' : showElemSet g)
				    $ Set.toList gens
				   putChar '\n') $ Map.toList $ subgroupGens gr
