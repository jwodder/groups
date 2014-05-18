import Data.Array
import Data.List (intercalate)
import System.Environment (getArgs)
import Closure (closure2A)
import Groups
import Groups.Internals (splitElem)
import qualified Permutation as P
import GrData

main :: IO ()
main = do argv@(_:_) <- getArgs
	  let perms = map (P.fromCycles . map (map read . splitElem ',')
					. splitElem '/') argv
	      n     = maximum $ map P.degree perms
	      els   = closure2A P.compose $ map (P.setDegree' n) perms
	      dex' = map head $ group $ sort els
	      dex = listArray (0, length dex' - 1) dex'
	      binsearch (low, high) _ | low > high = undefined
	      binsearch (low, high) x = case compare x (dex ! i) of
					 LT -> binsearch (low, i - 1) x
					 EQ -> i
					 GT -> binsearch (i + 1, high) x
					 where i = (low + high) `div` 2
	      name  = '⟨' : intercalate ", " (map P.showCycles perms) ++ "⟩"
	  putStr $ grdata (name, mkgroup $ Group' {
	   g'size   = rangeSize $ bounds dex,
	   g'elems  = els,
	   g'index  = binsearch (bounds dex),
	   g'oper   = P.compose,
	   g'invert = P.invert,
	   g'order  = P.order,
	   g'id     = P.identity' n
	  }, [])
