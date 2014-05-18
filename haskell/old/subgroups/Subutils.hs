module Subutils where
 import Data.Array
 import Data.List (intercalate)
 import Data.IntSet (IntSet)
 import qualified Data.IntSet as ISet
 import Math.Groups

 import MoreData.Symmetric (showCycles)

 group :: Group
 group = mkgroup group'

 ciska :: Int -> String
 ciska = (!) strs where strs = array (0, g'size group' - 1)
			 [(g'index group' x, ciska' x) | x <- g'elems group']

 --group' = symmetric' 4
 group' = alternating' 5
 ciska' = showCycles

{-
 group' = semidirect' quaternion' (cyclic' 2) (\x (i, j) -> if x == 1 then (mod (i + 2 * mod i 2 + 2*fromEnum j) 4, j) else (i, j))

 ciska' ((0, False), x) = if x == 0 then "1" else "x"
 ciska' ((i, j), x) | i >= 2 = '-' : ciska' ((i-2, j), x)
 ciska' (ij, 1) = ciska' (ij, 0) ++ "x"
 ciska' (ij, 0) = case ij of (0, False) -> ""
			     (1, False) -> "i"
			     (0, True)  -> "j"
			     (1, True)  -> "k"
-}

{-
 group' = genquaternion' 3

 ciska' = showGenQuatern 3

 showGenQuatern :: Int -> (Int, Bool) -> String
 showGenQuatern n | n < 2 = undefined
 showGenQuatern n = showDicyclic $ 2^pred n

 showDicyclic :: Int -> (Int, Bool) -> String
 showDicyclic _ (0, False) = "1"
 showDicyclic n (i, j) | i >= n = '-' : showDicyclic n (i-n, j)
 showDicyclic _ (i, j) = shexp "i" i ++ if j then "j" else ""

 shexp :: String -> Int -> String  -- helper function only
 shexp _ 0 = ""
 shexp var 1 = var
 shexp var n = var ++ '^' : show n
-}

{-
 group' = dihedral' 8

 ciska' (False, 0) = "1"
 ciska' (True, 0) = "s"
 ciska' (s, 1) = (if s then "s" else "") ++ "r"
 ciska' (s, r) = (if s then "s" else "") ++ "r^" ++ show r
-}

---------------------------------------

 bracket :: String -> [Int] -> String
 bracket [o, c] vals = o : intercalate ", " (map ciska vals) ++ [c]

 newClosure2 :: (Int -> Int -> Int) -> IntSet -> [Int] -> IntSet
 newClosure2 f seen new = closureS (close2 f)
  (new, foldl (flip ISet.insert) seen new)

 closureS :: ([Int] -> IntSet -> [Int]) -> ([Int], IntSet) -> IntSet
 closureS _ ([],  seen) = seen
 closureS f (new, seen) = closureS f (view (f new seen) seen)

 close2 :: (Int -> Int -> Int) -> [Int] -> IntSet -> [Int]
 close2 f new seen = concat [[f a b, f b a] | a <- ISet.toList seen, b <- new]

 view :: [Int] -> IntSet -> ([Int], IntSet)
 view [] seen = ([], seen)
 view (x:xs) seen | ISet.member x seen = view xs seen
 view (x:xs) seen = x &: view xs (ISet.insert x seen)
  where z &: (zs, y) = (z:zs, y)
