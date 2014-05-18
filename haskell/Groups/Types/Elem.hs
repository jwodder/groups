module Groups.Types.Elem where
 import Groups.Types.Group

 newtype Elem = Elem (Int, Group) deriving (Eq, Ord, Read, Show)
  -- TODO: Keep this constructor private to prevent out-of-bounds errors?

 elemID :: Elem -> Int
 elemID (Elem (i, _)) = i

 elements :: Group -> [Elem]
 elements gr = [Elem (i, gr) | i <- g_elems gr]

 getGroup :: Elem -> Group
 getGroup (Elem (_, gr)) = gr

 getGroup' :: [Elem] -> Maybe Group
 getGroup' [] = Nothing
 getGroup' (x:xs) = grp xs
  where g = getGroup x
	grp (y:ys) | y ∈ g = grp ys
	grp [] = Just g
	grp _  = Nothing

 order :: Elem -> Int
 order (Elem (i, gr)) = g_order gr i

 inverse :: Elem -> Elem
 inverse (Elem (i, gr)) = Elem (g_invert gr i, gr)

 identity :: Group -> Elem
 identity gr = Elem (0, gr)

 isIdentity :: Elem -> Bool
 isIdentity (Elem (i, _)) = i == 0

 infixl 7 ·
 infix  4 ∈, ∋

 (∈) :: Elem -> Group -> Bool
 Elem (_, g1) ∈ g2 = g1 == g2

 (∋) :: Group -> Elem -> Bool
 (∋) = flip (∈)

 (·) :: Elem -> Elem -> Elem
 Elem (a, g1) · Elem (b, g2) | g1 == g2  = Elem (g_oper g1 a b, g1)
			     | otherwise = undefined
