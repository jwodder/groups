import Data.List (intercalate)
import System.Environment (getArgs)
import Groups.Families (permutation)
import qualified Permutation as P
import GrData

main :: IO ()
main = do argv@(_:_) <- getArgs
	  let perms = map (P.fromCycles . map (map read . splitElem ',')
					. splitElem '/') argv
	      name  = '⟨' : intercalate ", " (map P.showCycles perms) ++ "⟩"
	  putStr $ grdata' (name, permutation perms, [])

splitElem :: Eq a => a -> [a] -> [[a]]
-- split on all occurrences of an element
splitElem _ [] = [[]]
splitElem e xs = pre : if null post then [] else splitElem e (tail post)
 where (pre, post) = break (== e) xs
