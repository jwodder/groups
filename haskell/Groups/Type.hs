module Groups.Type where
 import Data.Array
 import qualified Data.Set as Set

 data Group a = Group {
  gsize   :: Int,
  gelems  :: [a],
  gindex  :: a -> Int,
  -- IMPORTANT INVARIANT: `gelems` must always be a sorted list in which the
  -- first element is the identity, and `gindex` must always return an
  -- element's index in this list.
  goper   :: a -> a -> a,
  ginvert :: a -> a,
  gorder  :: a -> Int,
  gid     :: a
  -- TODO: Add a `gcontains` or `(∈)` function
 }

 tabulate :: Group a -> Group Int
 tabulate g = Group {
  gsize   = gsize g,
  gelems  = indices invtbl,
  gindex  = id,
  goper   = curry (optbl !),
  ginvert = (invtbl !),
  gorder  = (ordtbl !),
  gid     = 0
 } where (optbl, invtbl, ordtbl) = tabulate' g

 tabulate' :: Group a -> (Array (Int, Int) Int, Array Int Int, Array Int Int)
 tabulate' g = (array ((0,0), (n-1, n-1)) [((wrap a,wrap b), wrap $ goper g a b)
					   | a <- list, b <- list],
		array (0, n-1) [(wrap a, wrap $ ginvert g a) | a <- list],
		array (0, n-1) [(wrap a, gorder g a) | a <- list])
  where n    = gsize  g
	list = gelems g
	wrap = gindex g

 gconjugate :: Group a -> a -> a -> a
 gconjugate g y x = goper g (goper g y x) (ginvert g y)

 gtrivial :: Group a -> Set.Set a
 gtrivial = Set.singleton . gid

 gtotal :: Group a -> Set.Set a
 gtotal = Set.fromDistinctAscList . gelems
