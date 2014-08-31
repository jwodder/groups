import Groups
import Permutation (showCycles, lehmer)

main :: IO ()
main = mapM_ (\s -> putStrLn $ show (lehmer s) ++ '\t' : showCycles s) $ gelems
 $ symmetric 5
