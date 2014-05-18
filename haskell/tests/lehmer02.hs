import Math.Groups5b
import MoreData.Symmetric (showCycles)

--group = symmetric' 4
group = alternating' 5

main = mapM_ (\s -> putStrLn $ show (g'index group s) ++ '\t' : showCycles s)
 $ g'elems group
