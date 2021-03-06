module Permutation (
  -- * Permutation type
  Permutation,
  -- * Basic operations
  (!), permuteSeq, compose, inverse,
  -- * Construction
  identity, transposition,
  firstOfDegree, lastOfDegree, snBounds, s_n,
  fromLehmer,
  fromCycle, fromCycles,
  fromArray,
  readCycles,
  -- * Deconstruction
  toCycles, toArray, toArray', showCycles,
  -- * Properties
  order, isEven, isOdd, sign, degree, lehmer, disjoint
 ) where
 import Data.Array hiding ((!))
 import qualified Data.Array as A
 import Data.Char (isSpace)
 import Data.List (intercalate, (\\))
 import Data.Monoid

 -- |A permutation of finitely many positive integers.
 --
 -- The 'Ord' and 'Enum' instances are based on the ordering implied by the
 -- modified Lehmer codes used by the 'lehmer' and 'fromLehmer' functions.
 -- 'Ord' sorts 'Permutation's into the same order as their respective modified
 -- Lehmer codes, and 'Enum' enumerates 'Permutation' values in modified Lehmer
 -- code order.  'fromEnum' and 'toEnum' are equal to 'lehmer' and
 -- 'fromLehmer', respectively.  The smallest modified Lehmer code, 0, belongs
 -- to 'identity', the smallest 'Permutation'.  There is no upper bound.
 newtype Permutation = Perm (Array Int Int) deriving (Eq)

 instance Ord Permutation where
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

 instance Enum Permutation where
  fromEnum = lehmer
  toEnum   = fromLehmer

  -- The default implementations of `enumFrom` and `enumFromTo` listed in the
  -- standard don't seem efficient enough for Permutation.  `enumFromThen` and
  -- `enumFromThenTo`, on the other hand...
  enumFrom = iterate succ
  enumFromTo x z = takeWhile (<= z) $ iterate succ x

  succ s | degree s < 2 = transposition 1 2
  succ (Perm σ) = case [i | i <- [2..n], σ A.! i > σ A.! (i-1)] of
		   []  -> firstOfDegree (n+1)
		   i:_ -> let (xs, y:ys) = span (σ A.! i <=) $ elems σ
			      (zs, _:ws) = splitAt (i - length xs - 2) ys
			  in trim $ Perm $ listArray (1,n)
				  $ reverse (xs ++ (σ A.! i) : zs) ++ y : ws
   where n = degree (Perm σ)

  pred s | degree s < 2 = error "pred(Permutation): cannot decrement identity"
  pred (Perm σ) = trim $ Perm $ listArray (1,n) $ reverse (xs ++ (σ A.! i) : zs)
						   ++ y : ws
   where n = degree (Perm σ)
	 i = head [j | j <- [2..n], σ A.! j < σ A.! (j-1)]
	 (xs, y:ys) = span (σ A.! i >=) $ elems σ
	 (zs, _:ws) = splitAt (i - length xs - 2) ys

 instance Ix Permutation where
  range (a,b) = [a..b]

  index (a,b) x | inRange (a,b) x = lehmer x - lehmer a
		| otherwise = error "index(Permutation): value not in range"

  inRange (a,b) x = a <= x && x <= b

  rangeSize (a,b) | a <= b = lehmer b - lehmer a + 1
		  | otherwise = 0

 instance Monoid Permutation where
  mempty  = identity
  mappend = compose

 infixl 9 !
 (!) :: Permutation -> Int -> Int
 Perm σ ! x = if inRange (bounds σ) x then σ A.! x else x

 compose :: Permutation -> Permutation -> Permutation
 compose s t = trim $ Perm $ array (1,n) [(i, s ! (t ! i)) | i <- [1..n]]
  where n = degree s `max` degree t

 inverse :: Permutation -> Permutation
 inverse (Perm σ) = Perm $ array (bounds σ) [(b,a) | (a,b) <- assocs σ]
  -- Trimming is not needed here (assuming the input is trimmed).

 identity :: Permutation
 identity = Perm $ array (1,0) []

 transposition :: Int -> Int -> Permutation
 transposition a b | a < 1 || b < 1 = error "Permutation.transposition: values must be positive"
 transposition a b | a == b = identity
 transposition a b = Perm $ array rng [(x, if x == a then b
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
 toCycles (Perm σ) = cyke [a | (a,b) <- assocs σ, a /= b]
  where cyke [] = []
	cyke (x:xs) = (x:c) : cyke (xs \\ c)
	 where c = takeWhile (/= x) $ tail $ iterate (σ A.!) x

 showCycles :: Permutation -> String
 showCycles σ = case toCycles σ of
  []  -> "1"
  cyc -> concatMap (('(' :) . (++ ")") . intercalate " " . map show) cyc

 readCycles :: String -> Permutation
 readCycles s = case dropWhile isSpace s of
  '1':xs | all isSpace xs -> identity
  xs@('(':_)              -> fromCycles $ readCycles' xs
  _                       -> error "Permutation.readCycles: invalid argument"
  where readCycles' ('(':xs) = case break (== ')') xs of
				(ys, ')':zs) -> map read (words ys) : readCycles' (dropWhile isSpace zs)
				_ -> error "Permutation.readCycles: invalid argument"
	readCycles' xs | all isSpace xs = []
	readCycles' _ = error "Permutation.readCycles: invalid argument"

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
 fromLehmer x | x < 0 = error "Permutation.fromLehmer: argument must be nonnegative"
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
 -- this is the identity.  For higher degrees, this is @transposition n (n-1)@.
 firstOfDegree :: Int -> Permutation
 firstOfDegree n | n < 2     = identity
		 | otherwise = transposition n (n-1)

 -- |@lastOfDegree n@ returns the last 'Permutation' of degree @n@ in
 -- modified Lehmer code order.  If @n@ is 0 or 1 (or anything less than 0),
 -- this is the identity.  For higher degrees, this is @pred $ firstOfDegree
 -- (n+1)@.
 lastOfDegree :: Int -> Permutation
 lastOfDegree n | n < 2 = identity
		| otherwise = Perm $ array (1,n) [(i, n-i+1) | i <- [1..n]]

 -- |@s_n n@ returns a sorted list of all 'Permutation's in $S_n$, i.e., all
 -- 'Permutation's of degree at most @n@.
 s_n :: Int -> [Permutation]
 s_n n = takeWhile ((<= n) . degree) $ iterate succ identity
 -- s_n n = [identity .. lastOfDegree n]

 -- |@snBounds n@ returns the smallest and largest 'Permutation' in $S_n$,
 -- i.e., @(identity, lastOfDegree n)@.
 snBounds :: Int -> (Permutation, Permutation)
 snBounds n | n < 0 = error "Permutation.snBounds: invalid argument"
	    | otherwise = (identity, lastOfDegree n)

 toArray :: Permutation -> Array Int Int  -- Rename "toImage"?
 toArray (Perm σ) = σ

 toArray' :: Int -> Permutation -> Array Int Int  -- Rename "toImage'"?
 toArray' n p@(Perm σ) | n <= degree p = σ
		       | otherwise = listArray (1,n) $ elems σ++[degree p+1..n]

 fromArray :: Array Int Int -> Permutation  -- Rename "fromImage"?
 fromArray σ | rangeSize (a,b) == 0 = identity
	     | a < 1 = error "Permutation.fromArray: values must be positive"
	     | all (== 1) $ elems $ accumArray (+) (0 :: Int) (a,b) [(x,1) | x <- elems σ] = trim $ Perm $ array (1,b) $ [(i,i) | i <- [1..a-1]] ++ assocs σ
	     | otherwise = error "Permutation.fromArray: each index must be an element exactly once"
	     where (a,b) = bounds σ

 permuteSeq :: Permutation -> [a] -> [a]
 permuteSeq p xs | length xs < degree p = error "Permutation.permuteSeq: sequence must have at least `degree` elements"
		 | otherwise = map (\i -> xs !! (i-1)) $ elems $ toArray' (length xs) $ inverse p

 trim :: Permutation -> Permutation  -- internal function
 trim (Perm σ) = Perm $ ixmap (1, deg) id σ
  where deg = last $ 0 : [i | (i,i') <- assocs σ, i /= i']
