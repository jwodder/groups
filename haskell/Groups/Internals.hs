module Math.Groups.Internals where
 import Control.Monad (guard)
 import Data.Array
 import Data.Maybe (fromJust)
 import Data.IntSet (IntSet)
 import qualified Data.IntSet as ISet
 import Math.Groups.Types

 cycOrd :: Int -> Int -> Int
 cycOrd n x = n `div` gcd x n

 fromElems :: [Elem] -> Maybe ([Int], Group)
 fromElems [] = Nothing
 fromElems xs@(Elem (_,g) : _) = fromels xs
  where fromels (Elem (y,h) : ys) = do guard (g == h)
				       (zs, g') <- fromels ys
				       Just (y:zs, g')
	fromels [] = Just ([], g)

 toElems :: Group -> [Int] -> [Elem]
 toElems g xs = [Elem (i,g) | i <- xs]

 type ASubset = Array Int Bool

 mksubset :: [Elem] -> (ASubset, Group)
 mksubset [] = undefined
 mksubset xs = (accumArray (const id) False (bounds $ gr_dat g)
  $ map (flip (,) True) xs', g)
  where (xs', g) = fromJust $ fromElems xs

 -- |@norms g h x@ tests whether the subgroup @h@ of @g@ is invariant under
 -- conjugation by @x@ (i.e., whether @x@ normalizes @h@).
 norms :: Group -> ASubset -> Int -> Bool
 norms g h x = all (h !) [g_oper g (g_oper g x i) x' | (i, True) <- assocs h]
  where x' = g_invert g x

 -- |@centers g h x@ tests whether @x@ commutes with every element of @h@
 centers :: Group -> [Int] -> Int -> Bool
 centers g h x = all (\j -> g_oper g x j == g_oper g j x) h

 closure' :: Group -> ([Int], IntSet) -> IntSet
 closure' g (xs, is) = close (xs, is)
  where func ys = [g_oper g y x | y <- ys, x <- xs]
	close ([],  seen) = seen
	close (new, seen) = close $ view (func new) seen

 view :: [Int] -> IntSet -> ([Int], IntSet)
 view [] seen = ([], seen)
 view (x:xs) seen | ISet.member x seen = view xs seen
 view (x:xs) seen = x &: view xs (ISet.insert x seen)
  where z &: (zs, y) = (z:zs, y)
