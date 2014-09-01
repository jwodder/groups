import Data.Array (array, (!))
import Data.List (intercalate)
import Data.Set (toList)
import Groups
import Permutation (Permutation, showCycles)

main :: IO ()
main = mapM_ (putStrLn . bracket . toList) $ toList $ subgroups group

group :: Group Int
group = tabulate group'

ciska :: Int -> String
ciska = (!) strs where strs = array (0, gsize group' - 1)
			      [(gindex group' x, ciska' x) | x <- gelems group']

bracket :: [Int] -> String
bracket vals = '{' : intercalate ", " (map ciska vals) ++ "}"

group' :: Group Permutation
group' = symmetric 4
--group' = alternating 5

ciska' :: Permutation -> String
ciska' = showCycles
