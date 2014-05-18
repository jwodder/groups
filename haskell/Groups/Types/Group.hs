module Math.Groups.Types.Group where
 import Data.Array

 newtype Group = Group (Array (Int, Int) Int, Array Int (Int, Int))
  -- Tuple items: group table; mapping from element numbers to inverses & orders
  deriving (Read, Show)
  -- The constructor must be kept private to prevent data mismatches.  To this
  -- end, gr_tbl and gr_dat shall be pure accessors rather than field labels.

 instance Eq  Group where Group (t1, _) == Group (t2, _) = t1 == t2
 instance Ord Group where compare (Group (t, _)) (Group (u, _)) = compare t u

 gr_tbl :: Group -> Array (Int, Int) Int
 gr_tbl (Group (t, _)) = t

 gr_dat :: Group -> Array Int (Int, Int)
 gr_dat (Group (_, d)) = d

 g_oper :: Group -> Int -> Int -> Int
 g_oper = curry . (!) . gr_tbl

 g_invert :: Group -> Int -> Int
 g_invert g = fst . (gr_dat g !)

 g_order :: Group -> Int -> Int
 g_order g = snd . (gr_dat g !)

 g_elems :: Group -> [Int]
 g_elems = indices . gr_dat

 g_size :: Group -> Int
 g_size = rangeSize . bounds . gr_dat

 g_cycle :: Group -> Int -> [Int]
 g_cycle g x = 0 : takeWhile (> 0) (iterate (g_oper g x) x)
