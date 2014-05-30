module Permutation (
  -- * Permutation type
  Permutation,
  -- * Basic operations
  (!), compose, invert,
  -- * Construction
  identity, transpose,
  next, prev, firstOfDegree,
  fromLehmer,
  fromCycle, fromCycles,
  -- * Deconstruction
  toCycles, showCycles,
  -- * Properties
  order, isEven, isOdd, sign, degree, lehmer, disjoint
 ) where
 import Data.Array hiding ((!))
 import qualified Data.Array as A
 import Data.List (intercalate)
 import Data.Monoid

 newtype Permutation = Perm (Array Int Int) deriving (Eq)

 instance Ord Permutation where
  -- This comparison method produces the same ordering as the modified Lehmer
  -- codes.
  compare s@(Perm σ) t@(Perm τ) = compare (degree s, map (τ A.!) xedni)
					  (degree t, map (σ A.!) xedni)
   where xedni = [degree s, degree s-1 .. 1]

 instance Read Permutation where
  readsPrec p = readParen (p > 10) $ \r -> do ("fromCycles", s) <- lex r
					      (xs, t) <- reads s
					      return (fromCycles xs, t)

 instance Show Permutation where
  showsPrec p σ = showParen (p > 10) $ showString "fromCycles "
				     . shows (toCycles σ)

 instance Monoid Permutation where
  mempty  = identity
  mappend = compose

 infixl 9 !
 (!) :: Permutation -> Int -> Int
 Perm σ ! x = if inRange (bounds σ) x then σ A.! x else x

 compose :: Permutation -> Permutation -> Permutation
 compose s t = trim $ Perm $ array (1,n) [(i, s ! (t ! i)) | i <- [1..n]]
  where n = degree s `max` degree t

 invert :: Permutation -> Permutation
 -- TODO: Rename "inverse"?
 invert (Perm σ) = Perm $ array (bounds σ) [(b,a) | (a,b) <- assocs σ]
  -- Trimming is not needed here (assuming the input is trimmed).

 identity :: Permutation
 identity = Perm $ array (1,0) []

 transpose :: Int -> Int -> Permutation
 -- TODO: Rename "transposition"?
 transpose a b | a < 1 || b < 1 = error "Permutation.transpose: values must be positive"
 transpose a b | a == b = identity
 transpose a b = Perm $ array rng [(x, if x == a then b
				       else if x == b then a
				       else x) | x <- range rng]
  where rng = (1, max a b)

 fromCycle :: [Int] -> Permutation
 fromCycle []  = identity
 fromCycle [_] = identity
 fromCycle (a:xs) = trim $ Perm $ listArray (1,n) [1..n] // motions
  -- Trimming isn't actually necessary here, is it?
  where (n, motions) = cyke a (a:xs)
	-- TODO: This needs to check the input list for non-positive numbers.
	-- TODO: How should this deal with duplicate elements in the input?
	cyke m (x:y:zs) = (m', (x,y):qs) where (m', qs) = cyke (max m x) (y:zs)
	cyke m [b] = (max m b, [(b,a)])
	cyke _ [] = undefined

 fromCycles :: [[Int]] -> Permutation
 fromCycles = mconcat . map fromCycle

 toCycles :: Permutation -> [[Int]]
 toCycles (Perm σ) = cyke $ accumArray const False (bounds σ) []
  where cyke used = case [x | (x, False) <- assocs used] of
		     [] -> []
		     x:_ | σ A.! x == x -> cyke $ used // [(x, True)]
			 | otherwise    -> (x:c) : cyke sh
			 where (c, sh) = cykeAt (σ A.! x) $ used // [(x, True)]
			       cykeAt q sh' = if q == x then ([], sh')
					      else q &: (cykeAt (σ A.! q)
							 $ sh' // [(q, True)])
			       z &: (zs, w) = (z:zs, w)

 showCycles :: Permutation -> String
 showCycles σ = case toCycles σ of
  []  -> "1"
  cyc -> concatMap (('(' :) . (++ ")") . intercalate " " . map show) cyc

 order :: Permutation -> Int
 order = foldl lcm 1 . map length . toCycles

 isEven, isOdd :: Permutation -> Bool
 isEven = even . sum . map (pred . length) . toCycles
 isOdd  = not . isEven

 -- |Returns the sign of the given 'Permutation' &#x2014; 1 if it is even, -1
 -- if it is odd
 sign :: Num a => Permutation -> a
 sign s = if isEven s then 1 else -1

 degree :: Permutation -> Int
 degree (Perm σ) = snd $ bounds σ

 lehmer :: Permutation -> Int
 lehmer (Perm σ) = snd $ foldl (\(left, l) x ->
				let (left', i) = lehmer' left (σ A.! x) 0
				in (left', l*x + i)) (ds, 0) ds
  where ds = reverse $ indices σ
	lehmer' (y:ys) x' i | x' == y = (ys, i)
	lehmer' (y:ys) x' i = y &: lehmer' ys x' (i+1)
	lehmer' [] _ _ = undefined
	z &: (zs, w) = (z:zs, w)

 fromLehmer :: Int -> Permutation
 fromLehmer x = Perm $ array (1, deg) $ zip mapping [deg, deg-1 .. 1]
  where code = code' x 1
	code' y _ | y < 1 = []
	code' y f = mod y f : code' (div y f) (f+1)
	deg = length code
	mapping = foldl (\m (i,c) -> take c m ++ i:drop c m) [] $ zip [1..] code

 disjoint :: Permutation -> Permutation -> Bool
 disjoint (Perm σ) (Perm τ) = not $ any (\((i,a),b) -> i /= a && i /= b)
				  $ zip (assocs σ) (elems τ)


 -- |@firstOfDegree n@ returns the first 'Permutation' of degree @n@ in
 -- modified Lehmer code order.  If @n@ is 0 or 1 (or anything less than 0),
 -- this is the identity.  For higher degrees, this is @transpose(n, n-1)@.
 firstOfDegree :: Int -> Permutation
 firstOfDegree n | n < 2     = identity
		 | otherwise = transpose n (n-1)

 -- |Returns the next 'Permutation' in modified Lehmer code order
 next :: Permutation -> Permutation
 next s | degree s < 2 = transpose 1 2
 next (Perm σ) = case [i | i <- [2..n], σ A.! i > σ A.! (i-1)] of
		  []  -> firstOfDegree (n+1)
		  i:_ -> let (xs, y:ys) = span (σ A.! i <=) $ elems σ
			     (zs, _:ws) = splitAt (i - length xs - 2) ys
			 in trim $ Perm $ listArray (1,n)
				 $ reverse (xs ++ (σ A.! i) : zs) ++ y : ws
  where n = degree (Perm σ)

 -- |Returns the previous @Permutation@ in modified Lehmer code order.  If the
 -- identity (which has Lehmer code 0) is passed to this function, it is an
 -- error.
 prev :: Permutation -> Permutation
 prev s | degree s < 2 = error "Permutation.prev: cannot decrement identity"
 prev (Perm σ) = trim $ Perm $ listArray (1,n) $ reverse (xs ++ (σ A.! i) : zs)
						  ++ y : ws
  where n = degree (Perm σ)
	i = head [j | j <- [2..n], σ A.! j < σ A.! (j-1)]
	(xs, y:ys) = span (σ A.! i >=) $ elems σ
	(zs, _:ws) = splitAt (i - length xs - 2) ys

 trim :: Permutation -> Permutation  -- internal function
 trim (Perm σ) = Perm $ ixmap (1, deg) id σ
  where deg = last $ 0 : [i | (i,i') <- assocs σ, i /= i']
