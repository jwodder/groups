import Data.Array
import Data.List (intercalate)
import System.Environment (getArgs)
import Closure (closure2A)
import Groups
import MoreData.Index (mkIndex, getIndex)
import MoreData.Lists (splitElem)
import qualified Permutation as P
import GrData

main :: IO ()
main = do argv@(_:_) <- getArgs
	  let perms = map (P.fromCycles . map (map read . splitElem ',')
					. splitElem '/') argv
	      n     = maximum $ map P.degree perms
	      els   = closure2A P.compose $ map (P.setDegree' n) perms
	      dex   = mkIndex els
	      name  = '⟨' : intercalate ", " (map P.showCycles perms) ++ "⟩"
	  putStr $ grdata (name, mkgroup $ Group' {
	   g'size   = rangeSize $ bounds dex,
	   g'elems  = els,
	   g'index  = getIndex dex,
	   g'oper   = P.compose,
	   g'invert = P.invert,
	   g'order  = P.order,
	   g'id     = P.identity' n
	  }, [])
