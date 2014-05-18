-- TODO: Make the Read and Show instances preserve degree information?

-- TODO: Make all Symmetries returned from functions be trimmed to their proper
-- degree à la permutation.py

-- |Like 'MoreData.Permutation', but optimized(-ish) for permutations on sets
-- of the form @[1..n]@, especially when not sparse.

module MoreData.Symmetric (
  -- * Symmetry type
  Symmetry,
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
 import MoreData.Lists ((&:))

 newtype Symmetry = Symm (Array Int Int) deriving (Eq, Ord)

 instance Read Symmetry where  -- This needs to preserve degree information.
  readsPrec p = readParen (p > 10) $ \r -> do ("fromCycles", s) <- lex r
					      (xs, t) <- reads s
					      return (fromCycles xs, t)

 instance Show Symmetry where  -- This needs to preserve degree information.
  showsPrec p σ = showParen (p > 10) $ showString "fromCycles "
				     . shows (toCycles σ)

 instance Monoid Symmetry where
  mempty  = identity
  mappend = compose

 infixl 9 !
 (!) :: Symmetry -> Int -> Int
 Symm σ ! x = if inRange (bounds σ) x then σ A.! x else x

 compose :: Symmetry -> Symmetry -> Symmetry
 compose s t = Symm $ array (1,n) [(i, s ! (t ! i)) | i <- [1..n]]
  where n = degree s `max` degree t

 invert :: Symmetry -> Symmetry
 invert (Symm σ) = Symm $ array (bounds σ) [(b,a) | (a,b) <- assocs σ]

 identity :: Symmetry
 identity = Symm $ array (1, 0) []

 identity' :: Int -> Symmetry
 identity' n = Symm $ listArray (1,n) [1..n]

 transpose :: Int -> Int -> Symmetry
 transpose a b | a < 1 || b < 1 = error "Symmetric.transpose: values must be positive"
 transpose a b | a == b = identity
 transpose a b = Symm $ array rng [(x, if x == a then b
				       else if x == b then a
				       else x) | x <- range rng]
  where rng = (1, max a b)

 fromCycle :: [Int] -> Symmetry
 fromCycle []  = identity
 fromCycle [_] = identity
 fromCycle (a:xs) = Symm $ listArray (1,n) [1..n] // motions
  where (n, motions) = cyke a (a:xs)
	-- TODO: This needs to check the input list for non-positive numbers.
	-- TODO: How should this deal with duplicate elements in the input?
	cyke m (x:y:zs) = (m', (x,y):qs) where (m', qs) = cyke (max m x) (y:zs)
	cyke m [b] = (max m b, [(b,a)])
	cyke _ [] = undefined

 fromCycles :: [[Int]] -> Symmetry
 fromCycles = mconcat . map fromCycle

 toCycles :: Symmetry -> [[Int]]
 toCycles (Symm σ) = cyke $ accumArray const False (bounds σ) []
  where cyke used = case [x | (x, False) <- assocs used] of
		     [] -> []
		     x:_ | σ A.! x == x -> cyke $ used // [(x, True)]
			 | otherwise    -> (x:c) : cyke sh
			 where (c, sh) = cykeAt (σ A.! x) $ used // [(x, True)]
			       cykeAt q sh' = if q == x then ([], sh')
					      else q &: (cykeAt (σ A.! q)
							 $ sh' // [(q, True)])

 showCycles :: Symmetry -> String
 showCycles σ = case toCycles σ of
  []  -> "1"
  cyc -> concatMap (('(' :) . (++ ")") . intercalate " " . map show) cyc

 order :: Symmetry -> Int
 order = foldl lcm 1 . map length . toCycles

 isEven, isOdd :: Symmetry -> Bool
 isEven = even . sum . map (pred . length) . toCycles
 isOdd  = not . isEven

 degree :: Symmetry -> Int
 degree (Symm σ) = snd $ bounds σ

 degree' :: Symmetry -> Int
 degree' (Symm σ) = last $ 0 : [i | (i, i') <- assocs σ, i /= i']

 isIdentity :: Symmetry -> Bool
 isIdentity (Symm σ) = all (uncurry (==)) $ assocs σ

 trim :: Symmetry -> Symmetry
 trim s@(Symm σ) = Symm $ ixmap (1, degree' s) id σ

 setDegree :: Int -> Symmetry -> Maybe Symmetry
 setDegree n s | n < degree' s = Nothing
 setDegree n (Symm σ) = Just $ Symm $ if n < d then ixmap (1,n) id σ
				      else listArray (1,n) $ elems σ ++ [d+1..n]
  where d = snd $ bounds σ

 setDegree' :: Int -> Symmetry -> Symmetry
 setDegree' n s = case setDegree n s of
  Just τ  -> τ
  Nothing -> error "Symmetric.setDegree': incompatible degrees"

 lehmer :: Symmetry -> Int
 lehmer (Symm σ) = sum $ zipWith (*) (reverse code) $ scanl (*) 1 [1..]
  where code = snd $ mapAccumL (\left x -> lehmer' left (σ A.! x) 0) ds ds
	ds = indices σ
	lehmer' (y:ys) x' i | x' == y = (ys, i)
	lehmer' (y:ys) x' i = y &: lehmer' ys x' (i+1)
	lehmer' [] _ _ = undefined
