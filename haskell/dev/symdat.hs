import Data.Array
import Data.List (intercalate)
import System.Environment (getArgs)
import Algorithms.Closure (closure2A)
import Math.Groups
import MoreData.Index (mkIndex, getIndex)
import MoreData.Lists (splitElem)
import qualified MoreData.Symmetric as Sy
import GrData

main :: IO ()
main = do argv@(_:_) <- getArgs
	  let perms = map (Sy.fromCycles . map (map read . splitElem ',')
					 . splitElem '/') argv
	      n     = maximum $ map Sy.degree perms
	      els   = closure2A Sy.compose $ map (Sy.setDegree' n) perms
	      dex   = mkIndex els
	      name  = '⟨' : intercalate ", " (map Sy.showCycles perms) ++ "⟩"
	  putStr $ grdata (name, mkgroup $ Group' {
	   g'size   = rangeSize $ bounds dex,
	   g'elems  = els,
	   g'index  = getIndex dex,
	   g'oper   = Sy.compose,
	   g'invert = Sy.invert,
	   g'order  = Sy.order,
	   g'id     = Sy.identity' n
	  }, [])
