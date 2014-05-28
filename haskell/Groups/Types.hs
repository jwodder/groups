module Groups.Types (
  module Groups.Types.Element,
  module Groups.Types.Group,
  module Groups.Types.Subset,
  Group'(..), mkgroup, unmkgroup
 ) where
 import Data.Array
 import Groups.Types.Element
 import Groups.Types.Group
 import Groups.Types.Subset

 data Group' a = Group' {
  g'size   :: Int,
  g'elems  :: [a],
  g'index  :: a -> Int,
  -- IMPORTANT: The identity element MUST always have an index of 0!
  g'oper   :: a -> a -> a,
  g'invert :: a -> a,
  g'order  :: a -> Int,
  g'id     :: a
 }

 mkgroup :: Group' a -> Group
 mkgroup g' = Group (array ((0, 0), (n-1, n-1)) [((wrap a, wrap b),
						   wrap $ op a b)
						 | a <- list, b <- list],
		     array (0, n-1) [(wrap a, (wrap $ inv a, ordr a))
				     | a <- list])
  where n    = g'size   g'
	list = g'elems  g'
	wrap = g'index  g'
	op   = g'oper   g'
	inv  = g'invert g'
	ordr = g'order  g'

 unmkgroup :: Group -> Group' Int
 unmkgroup g = Group' {
  g'size   = g_size g,
  g'elems  = g_elems g,
  g'index  = id,
  g'oper   = g_oper g,
  g'invert = g_invert g,
  g'order  = g_order g,
  g'id     = 0
 }
