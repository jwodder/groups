import Data.Array
import Data.List (intercalate)
import Math.Groups

main = mapM_ (\x -> putStrLn $ intercalate "\t" $ map (ciska . elemID)
			     $ map (x ·) $ elements group) $ elements group

group = mkgroup group'

ciska :: Int -> String
ciska = (!) strs where strs = array (0, g'size group' - 1)
			[(g'index group' x, ciska' x) | x <- g'elems group']

group' = dihedral' 8

ciska' (False, 0) = "1"
ciska' (True, 0) = "s"
ciska' (s, 1) = (if s then "s" else "") ++ "r"
ciska' (s, r) = (if s then "s" else "") ++ "r^" ++ show r
