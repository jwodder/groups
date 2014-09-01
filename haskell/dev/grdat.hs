import System.IO (stdout, hFlush)
import Groups (Group)
import GrData
import GrList

main :: IO ()
main = mapM_ (\g -> putStrLn (grdata' g) >> hFlush stdout) groups

groups :: [(String, Group Int, [String])]
groups = groupList
--groups = o32nonA

{-
groups = [(intercalate "\\times" $ map (zahlen . show) xs, g, [])
	  | (g, xs) <- abelians 32]
 where zahlen [c] = "\\Z_" ++ [c]
       zahlen xs = "\\Z_{" ++ xs ++ "}"
-}

--groups = [("\\Dic_{" ++ show n ++ "}", tabulate $ dicyclic n, ["dicyclic"]) | n <- [1..20]]
