import Closure
import Permutation

main = mapM_ (putStrLn . showCycles) $ closure2A compose
 $ map (setDegree' 5 . transpose 1) [2..5]
