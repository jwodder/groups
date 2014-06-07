import Permutation

main = do mapM_ (\s -> putStrLn $ show (lehmer s) ++ '\t' : showCycles s)
	   $ takeWhile (< firstOfDegree 6) $ iterate succ identity
	  putStrLn "---"
	  mapM_ (\s -> putStrLn $ show (lehmer s) ++ '\t' : showCycles s)
	   $ map pred $ takeWhile (> identity) $ iterate pred $ firstOfDegree 6
