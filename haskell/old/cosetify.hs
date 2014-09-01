 -- |Given a 'Group' @g@ and a subset @h@, @cosetify g h@ returns a list of
 -- arbitrarily-chosen representatives of left cosets of @h@ (in ascending
 -- order) and a mapping from elements of @h@ to those representatives; not for
 -- export
 cosetify :: Group -> [Int] -> ([Int], Array Int Int)
 cosetify g h = cosify $ listArray (bounds $ gr_dat g) $ repeat Nothing
  where cosify c = case [x | (x, Nothing) <- assocs c] of
	 x:_ -> x &: cosify (c // [(y, Just x) | y <- map (g_oper g x) h])
	 []  -> ([], fmap fromJust c)
