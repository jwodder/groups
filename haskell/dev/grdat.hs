import Math.Groups (Group)
import GrData
import GrList

main :: IO ()
main = mapM_ (putStrLn . grdata) groups

groups :: [(String, Group, [String])]
--groups = groupList
groups = o32nonA

{-
groups = [(intercalate "\\times" $ map (zahlen . show) xs, g, [])
	  | (g, xs) <- abelians 32]
 where zahlen [c] = "\\Z_" ++ [c]
       zahlen xs = "\\Z_{" ++ xs ++ "}"
-}

--groups = [("\\Dic_{" ++ show n ++ "}", dicyclic n, ["dicyclic"]) | n <- [1..20]]
