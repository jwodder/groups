import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Groups (Group, symmetric)
import Groups.Subgroups (subgroupGens)
import Groups.Types.Subset (Subset)
import qualified Groups.Types.Subset as Sub
import Permutation (showCycles, fromLehmer)

gr :: Group
gr = symmetric 4

showElem :: Int -> String
showElem = showCycles . fromLehmer

showElemSet :: Subset -> String
showElemSet xs = '{' : intercalate ", " (map showElem $ Sub.toInts xs) ++ "}"

main :: IO ()
main = mapM_ (\(subgr, gens) -> do putStrLn $ showElemSet subgr
				   mapM_ (\g -> putStrLn $ ' ' : showElemSet g)
				    $ Set.toList gens
				   putChar '\n') $ Map.toList $ subgroupGens gr
