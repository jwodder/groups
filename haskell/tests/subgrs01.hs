import Data.Array (array, (!))
import Data.List (intercalate)
import Data.Set (toList)
import Groups
import Groups.Subgroups
import Groups.Types.Subset (toInts)
import Permutation (showCycles)

main = mapM_ (putStrLn . bracket "{}" . toInts) $ toList $ subgroups group

group :: Group
group = mkgroup group'

ciska :: Int -> String
ciska = (!) strs where strs = array (0, g'size group' - 1)
			[(g'index group' x, ciska' x) | x <- g'elems group']

group' = symmetric' 4
--group' = alternating' 5
ciska' = showCycles

bracket :: String -> [Int] -> String
bracket [o, c] vals = o : intercalate ", " (map ciska vals) ++ [c]
bracket _ _ = undefined
