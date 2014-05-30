import Permutation

main = do mapM_ (\s -> putStrLn $ show (lehmer s) ++ '\t' : showCycles s)
	   $ takeWhile (< firstOfDegree 6) $ iterate next identity
	  putStrLn "---"
	  mapM_ (\s -> putStrLn $ show (lehmer s) ++ '\t' : showCycles s)
	   $ map prev $ takeWhile (> identity) $ iterate prev $ firstOfDegree 6
