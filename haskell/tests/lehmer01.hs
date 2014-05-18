import Algorithms.Closure (closure2A)
import MoreData.Symmetric

n = 5

main = mapM_ (\s -> putStrLn $ show (lehmer s) ++ '\t' : showCycles s)
-- $ closure2A compose $ map (setDegree' n . transpose 1) [2..n]
 $ if n == 1 then [identity' 1]
   else closure2A compose [fromCycle [1..n], setDegree' n $ transpose 1 2]
