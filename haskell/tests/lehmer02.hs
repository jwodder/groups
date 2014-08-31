import Groups
import Permutation (Permutation, showCycles, fromLehmer)

group :: Group Permutation
group = symmetric 5

main :: IO ()
main = mapM_ (\x -> let lehmer = gindex group x
			remhel = fromLehmer lehmer
		    in if x /= remhel then putStrLn $ showCycles x ++ " -> "
						   ++ show lehmer  ++ " -> "
						   ++ showCycles remhel
		       else return ()) $ gelems group
