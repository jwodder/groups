module Closure (
  -- * Closure functions
  closure2, closure2', closure2'',
  closure2A, closure2A',
  closure1m, closure1m',
  -- * Utility functions
  closureL, closureS, close2, view
 ) where
 import Data.Set (Set, fromList, toList, member, insert)

 closure2 :: Ord a => (a -> a -> a) -> [a] -> [a]
 closure2 f start = closureL (close2 f) (start, fromList start)

 closure2' :: Ord a => (a -> a -> a) -> [a] -> Set a
 closure2' f start = closureS (close2 f) (start, fromList start)

 closure2'' :: Ord a => (a -> a -> a) -> Set a -> Set a
 closure2'' f start = closureS (close2 f) (toList start, start)

 closure2A :: Ord a => (a -> a -> a) -> [a] -> [a]
 -- For use when @f@ is associative
 closure2A f start = closure1m (\x -> map (`f` x) start) start

 closure2A' :: Ord a => (a -> a -> a) -> [a] -> Set a
 -- For use when @f@ is associative
 closure2A' f start = closure1m' (\x -> map (`f` x) start) start

 closure1m :: Ord a => (a -> [a]) -> [a] -> [a]
 closure1m f start = closureL (\xs _ -> concatMap f xs) (start, fromList start)

 closure1m' :: Ord a => (a -> [a]) -> [a] -> Set a
 closure1m' f start = closureS (\xs _ -> concatMap f xs) (start, fromList start)

 closureL :: Ord a => ([a] -> Set a -> [a]) -> ([a], Set a) -> [a]
 closureL _ ([],  _) = []
 closureL f (new, seen) = new ++ closureL f (view (f new seen) seen)

 closureS :: Ord a => ([a] -> Set a -> [a]) -> ([a], Set a) -> Set a
 closureS _ ([],  seen) = seen
 closureS f (new, seen) = closureS f (view (f new seen) seen)

 close2 :: Ord a => (a -> a -> a) -> [a] -> Set a -> [a]
 -- Reconsider this function's name; "bisetmap"? "doublemap"? "ambimap"?
 close2 f new seen = concat [[f a b, f b a] | a <- toList seen, b <- new]

 -- |Given a list @xs@ and a set @s@ of "seen" values, @view xs s@ returns a
 -- list of (non-duplicate) values in @xs@ but not in @s@ and an updated
 -- version of @s@ that includes the values in @xs@.
 view :: Ord a => [a] -> Set a -> ([a], Set a)
 view [] seen = ([], seen)
 view (x:xs) seen | member x seen = view xs seen
 view (x:xs) seen = x &: view xs (insert x seen)
  where z &: (zs, y) = (z:zs, y)
