 -- |@abelians n@ returns a list of all abelian groups of order @n@ together
 -- with their invariant factors.
 abelians :: Int -> [(Group, [Int])]
 abelians n | n < 1 = []
 abelians 1 = [(trivial, [])]
 abelians n = map (\xs -> (foldl1 (\g h -> tabulate $ direct g h)
			   $ map cyclic xs, xs))
	    $ map (foldl1 $ \a b -> map (uncurry (*)) $ extZip 1 1 a b) $ cross
	    $ map (\(p,k) -> map (map (p^)) $ partitions k) $ factor n
