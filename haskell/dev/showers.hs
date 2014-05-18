 multish :: String -> String -> String  -- helper function only
 multish "1" xs = xs
 multish xs "1" = xs
 multish xs ys = xs ++ ys

 shexp :: String -> Int -> String  -- helper function only
 shexp _ 0 = "1"
 shexp var 1 = var
 shexp var n = var ++ '^' : show n

 showKlein :: (Bool, Bool) -> String
 showKlein (False, False) = "1"
 showKlein (False, True)  = "a"
 showKlein (True,  False) = "b"
 showKlein (True,  True)  = "c"

 showAB :: (a -> String) -> (b -> String) -> (a,b) -> String
 showAB sha shb (a,b) = multish (sha a) (shb b)

 showSemiBA :: Group' (a,b) -> (a -> String) -> (b -> String) -> (a,b) -> String
 showSemiBA g sha shb (a,b) = multish (shb b) (sha a')
  where b' = g'invert g (fst $ g'id g, b)
	a' = fst $ g'oper g b' (a, b)

 showQuatern :: (Int, Bool) -> String  -- for Q_8 only
 showQuatern (0, False) = "1"
 showQuatern (1, False) = "i"
 showQuatern (2, False) = "-1"
 showQuatern (3, False) = "-i"
 showQuatern (0, True)  = "j"
 showQuatern (1, True)  = "k"
 showQuatern (2, True)  = "-j"
 showQuatern (3, True)  = "-k"

 showGenQuatern :: Int -> (Int, Bool) -> String
 showGenQuatern n | n < 2 = undefined
 showGenQuatern n = showDicyclic $ 2^pred n

 showDicyclic :: Int -> (Int, Bool) -> String
 showDicyclic n (i,j) | i >= n = '-' : showDicyclic n (i-n, j)
 showDicyclic _ (i,j) = multish (shexp "i" i) (j ?: "j" :? "1")

 showDih :: (Bool, Int) -> String
 showDih (s,r) = multish (s ?: "s" :? "1") (shexp "r" r)

 -- custom shower for Q_8 ⋊ Z_2:
 showQ8sZ2 :: ((Int, Bool), Bool) -> String
 showQ8sZ2 ((0, False), x) = x ?: "x" :? "1"
 showQ8sZ2 ((i, j), x) | i >= 2 = '-' : showQ8sZ2 ((i-2, j), x)
 showQ8sZ2 (ij, True) = showQ8sZ2 (ij, False) ++ "x"
 showQ8sZ2 (ij, False) = case ij of (0, False) -> ""
				    (1, False) -> "i"
				    (0, True)  -> "j"
				    (1, True)  -> "k"
