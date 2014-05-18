-- TODO: Make the Read and Show instances preserve degree information?

-- TODO: Make all Permutations returned from functions be trimmed to their
-- proper degree à la permutation.py

module Permutation (
  -- * Permutation type
  Permutation,
  -- * Basic operations
  (!), compose, invert,
  -- * Construction
  identity, identity', transpose, fromCycle, fromCycles,
  -- * Deconstruction
  toCycles, showCycles,
  -- * Properties
  order, isEven, isOdd, degree, degree', isIdentity, lehmer,
  -- * Other
  trim, setDegree, setDegree'
 ) where
 import Data.Array hiding ((!))
 import qualified Data.Array as A
 import Data.List (intercalate, mapAccumL)
 import Data.Monoid

 newtype Permutation = Perm (Array Int Int) deriving (Eq, Ord)

 instance Read Permutation where  -- This needs to preserve degree information.
  readsPrec p = readParen (p > 10) $ \r -> do ("fromCycles", s) <- lex r
					      (xs, t) <- reads s
					      return (fromCycles xs, t)

 instance Show Permutation where  -- This needs to preserve degree information.
  showsPrec p σ = showParen (p > 10) $ showString "fromCycles "
				     . shows (toCycles σ)

 instance Monoid Permutation where
  mempty  = identity
  mappend = compose

 infixl 9 !
 (!) :: Permutation -> Int -> Int
 Perm σ ! x = if inRange (bounds σ) x then σ A.! x else x

 compose :: Permutation -> Permutation -> Permutation
 compose s t = Perm $ array (1,n) [(i, s ! (t ! i)) | i <- [1..n]]
  where n = degree s `max` degree t

 invert :: Permutation -> Permutation
 invert (Perm σ) = Perm $ array (bounds σ) [(b,a) | (a,b) <- assocs σ]

 identity :: Permutation
 identity = Perm $ array (1, 0) []

 identity' :: Int -> Permutation
 identity' n = Perm $ listArray (1,n) [1..n]

 transpose :: Int -> Int -> Permutation
 transpose a b | a < 1 || b < 1 = error "Permutation.transpose: values must be positive"
 transpose a b | a == b = identity
 transpose a b = Perm $ array rng [(x, if x == a then b
				       else if x == b then a
				       else x) | x <- range rng]
  where rng = (1, max a b)

 fromCycle :: [Int] -> Permutation
 fromCycle []  = identity
 fromCycle [_] = identity
 fromCycle (a:xs) = Perm $ listArray (1,n) [1..n] // motions
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
			       x &: (xs, y) = (x:xs, y)

 showCycles :: Permutation -> String
 showCycles σ = case toCycles σ of
  []  -> "1"
  cyc -> concatMap (('(' :) . (++ ")") . intercalate " " . map show) cyc

 order :: Permutation -> Int
 order = foldl lcm 1 . map length . toCycles

 isEven, isOdd :: Permutation -> Bool
 isEven = even . sum . map (pred . length) . toCycles
 isOdd  = not . isEven

 degree :: Permutation -> Int
 degree (Perm σ) = snd $ bounds σ

 degree' :: Permutation -> Int
 degree' (Perm σ) = last $ 0 : [i | (i, i') <- assocs σ, i /= i']

 isIdentity :: Permutation -> Bool
 isIdentity (Perm σ) = all (uncurry (==)) $ assocs σ

 trim :: Permutation -> Permutation
 trim s@(Perm σ) = Perm $ ixmap (1, degree' s) id σ

 setDegree :: Int -> Permutation -> Maybe Permutation
 setDegree n s | n < degree' s = Nothing
 setDegree n (Perm σ) = Just $ Perm $ if n < d then ixmap (1,n) id σ
				      else listArray (1,n) $ elems σ ++ [d+1..n]
  where d = snd $ bounds σ

 setDegree' :: Int -> Permutation -> Permutation
 setDegree' n s = case setDegree n s of
  Just τ  -> τ
  Nothing -> error "Permutation.setDegree': incompatible degrees"

 lehmer :: Permutation -> Int
 lehmer (Perm σ) = sum $ zipWith (*) (reverse code) $ scanl (*) 1 [1..]
  where code = snd $ mapAccumL (\left x -> lehmer' left (σ A.! x) 0) ds ds
	ds = indices σ
	lehmer' (y:ys) x' i | x' == y = (ys, i)
	lehmer' (y:ys) x' i = y &: lehmer' ys x' (i+1)
	lehmer' [] _ _ = undefined
	x &: (xs, y) = (x:xs, y)
