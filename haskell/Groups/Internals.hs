{-# OPTIONS_HADDOCK hide #-}

module Groups.Internals where
 import Control.Monad (guard, liftM2)
 import Data.Bits (bit, testBit)
 import Data.IntSet (IntSet)
 import qualified Data.IntSet as ISet
 import Groups.Types

 cycOrd :: Int -> Int -> Int
 cycOrd n x = n `div` gcd x n

 fromElems :: [Element] -> Maybe ([Int], Group)
 -- TODO: Move this to Element.hs?
 fromElems [] = Nothing
 fromElems xs@(Element (_,g) : _) = fromels xs
  where fromels (Element (y,h) : ys) = do guard (g == h)
					  (zs, g') <- fromels ys
					  Just (y:zs, g')
	fromels [] = Just ([], g)

 toElems :: Group -> [Int] -> [Element]
 toElems g xs = [Element (i,g) | i <- xs]

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

 infixr 5 &:
 (&:) :: a -> ([a], b) -> ([a], b)
 x &: (xs, y) = (x:xs, y)

 data TernaryBranch a = a :? a deriving (Eq, Ord, Read, Show, Bounded)

 instance Functor TernaryBranch where fmap f (x :? y) = f x :? f y

 infixr 0 ?:, :?

 (?:) :: Bool -> TernaryBranch a -> a
 True  ?: (y :? _) = y
 False ?: (_ :? z) = z

 classify :: Ord b => (a -> b) -> [a] -> [(b, [a])]
 classify f = foldr shove [] . map (\x -> (f x, x))
  where shove (b', a) [] = [(b', [a])]
	shove (b', a) ((b, as):xs) | b' == b = (b, a:as) : xs
				   | b' <  b = (b', [a]) : (b, as) : xs
				   | b' >  b = (b, as) : shove (b', a) xs
	shove _ _ = undefined  -- to stop the compilation warnings
 -- classify f xs = Map.toList $ Map.fromListWith (flip (++)) [(f x, [x]) | x <- xs]

 cartesian :: [a] -> [b] -> [(a,b)]
 -- cartesian a b = [(x,y) | x <- a, y <- b]
 cartesian = liftM2 (,)

 -- |@cross list@ returns the list of all lists that are formed by taking one
 -- element from each sublist of @list@.
 cross :: [[a]] -> [[a]]
 cross [] = [[]]
 cross (x:xs) = [a:b | a <- x, b <- cross xs]

 extZip :: a -> b -> [a] -> [b] -> [(a,b)]
 extZip a b (x:xs) (y:ys) = (x,y) : extZip a b xs ys
 extZip a b (x:xs) [] = (x,b) : extZip a b xs []
 extZip a b [] (y:ys) = (a,y) : extZip a b [] ys
 extZip _ _ [] [] = []

 -- |@factor n@ returns a list of pairs of prime factors of @n@ and their
 -- multiplicities/exponents.  If @n@ is @1@, an empty list is returned.  If
 -- @n@ is @0@, @[(0, 1)]@ is returned.  If @n@ is negative, the result is
 -- equal to @(-1, 1) : factor (-n)@.
 --
 -- This function is not recommended for use on large integers.
 factor :: Integral a => a -> [(a, Int)]
 factor = fst . factorWith (2 : [3,5..])

 -- |@factorWith primes n@ decomposes @n@ into a product of powers of elements
 -- of @primes@ (which are assumed to all be coprime and positive) and a
 -- coprime quotient.  Specifically, it returns a pair containing:
 --
 -- * a list of pairs of factors of @n@ in @primes@ and their
 --   multiplicities/exponents, and
 --
 -- * the product of any remaining factors of @n@ that do not appear in
 --   @primes@ (or @1@ if there are none).
 --
 -- If @n@ is @1@, @([], 1)@ is always returned.  If @n@ is @0@, @([(0, 1)],
 -- 1)@ is always returned.  If @n@ is negative, the result equals @(-1, 1) :
 -- factorWith primes (-n)@.
 factorWith :: Integral a => [a] -> a -> ([(a, Int)], a)
 factorWith _ 0 = ([(0,1)], 1)
 factorWith primal n | n < 0 = (-1, 1) &: factor' primal (-n)
		     | n > 0 = factor' primal n
		     | otherwise = error "Provably redundant and unreachable"
  where factor' _  1    = ([], 1)
	factor' [] m    = ([], m)
	factor' (p:q) m | mod m p == 0 = (p, k) &: factor' q x
	 where (k,x) = until (\(_,y) -> mod y p /= 0)
			     (\(j,y) -> (j+1, div y p)) (0, m)
	factor' (_:q) m = factor' q m

 modInverse :: Integral a => a -> a -> Maybe a
 modInverse a n = invert (abs n) 0 (modulo a n) 1
  where invert _ _ 1 x = Just $ modulo x n
	invert _ _ 0 _ = Nothing
	invert u uc l lc = invert l lc (mod u l) (uc - lc * div u l)
	modulo a' n' = mod a' (abs n')

 modInverse' :: Integral a => a -> a -> a
 modInverse' a n = case modInverse a n of
  Just x  -> x
  Nothing -> error "Math.NumTheory.modInverse': no inverse exists"

 partitions :: Int -> [[Int]]
 partitions n = if n < 1 then undefined else gen n n
  where gen 0 _ = [[]]
	gen qty mx = [i:xs | i <- reverse [1..min qty mx], xs <- gen (qty-i) i]

 -- |@expfa f n x@ computes @foldl1 f (replicate n x)@ in log(/n/) time by
 -- assuming that @f@ is associative.
 expfa :: (a -> a -> a) -> Int -> a -> a
 expfa _ n _ | n < 1 = error "expfa: exponent must be positive"
 expfa f n x = agg
  where (mini, minx) = until (testBit n . fst) (\(i, y) -> (i+1, f y y)) (0, x)
	(_, agg, _)  = until (\(i, _, _) -> bit i > n)
			     (\(i, agg', y) -> (i+1,
			       if testBit n i then f agg' y else agg', f y y))
			     (mini+1, minx, f minx minx)
