import Data.Array
import Data.List (intercalate)
import Groups

main = mapM_ (putStrLn . bracket "{}" . map elemID . gclosure . map (mkelem group'))
 [[(False, 4), (True, 0)],
  [(False, 4), (True, 1)],
  [(False, 4), (True, 2)],
  [(False, 4), (True, 3)]]

bracket :: String -> [Int] -> String
bracket [o, c] vals = o : intercalate ", " (map ciska vals) ++ [c]

group = mkgroup group'

ciska :: Int -> String
ciska = (!) strs where strs = array (0, g'size group' - 1)
			[(g'index group' x, ciska' x) | x <- g'elems group']

group' = dihedral' 8

ciska' (False, 0) = "1"
ciska' (True, 0) = "s"
ciska' (s, 1) = (if s then "s" else "") ++ "r"
ciska' (s, r) = (if s then "s" else "") ++ "r^" ++ show r

mkelem :: Group' a -> a -> Element
mkelem g x = Element (g'index g x, mkgroup g)
