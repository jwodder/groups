import Data.List (intercalate)
import Math.Groups.Type

group = semidirect quaternion (cyclic 2) (\x (i, j) -> if x == 1 then (mod (i + 2 * mod i 2 + 2*fromEnum j) 4, j) else (i, j))

main = mapM_ (\(subgr, gens) -> do
  putStrLn $ '{' : intercalate ", " (map ciska subgr) ++ "}"
  mapM_ (\a -> putStrLn $ " = ⟨" ++ intercalate ", " (map ciska a) ++ "⟩") gens)
 [(gcycle group x, [[x]]) | x <- gr_elems group]

ciska = showSemiAB group showQuatern (shexp "x")

showSemiAB :: (Eq a, Eq b) => Group (a, b) -> (a -> String) -> (b -> String)
 -> (a, b) -> String
-- This assumes that the supplied Showers return an empty string for
-- identities; figure out how best to deal with this.
showSemiAB g _ _ e | e == gr_id g = "1"
showSemiAB _ sha shb (a, b) = sha a ++ shb b

showQuatern :: (Int, Bool) -> String  -- for Q_8 only
showQuatern (0, False) = "1"
showQuatern (1, False) = "i"
showQuatern (2, False) = "-1"
showQuatern (3, False) = "-i"
showQuatern (0, True)  = "j"
showQuatern (1, True)  = "k"
showQuatern (2, True)  = "-j"
showQuatern (3, True)  = "-k"

shexp :: String -> Int -> String  -- helper function only
shexp _ 0 = ""
shexp var 1 = var
shexp var n = var ++ '^' : show n
