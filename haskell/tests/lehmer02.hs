import Groups
import Permutation (showCycles, fromLehmer)

n = 5
group = symmetric' n

main = mapM_ (\x -> let lehmer = g'index group x
			remhel = fromLehmer lehmer
		    in if x /= remhel then putStrLn $ showCycles x ++ " -> "
						   ++ show lehmer  ++ " -> "
						   ++ showCycles remhel
		       else return ()) $ g'elems group
