module Groups.Types.Element where
 import Groups.Types.Group

 newtype Element = Element (Int, Group) deriving (Eq, Ord, Read, Show)
  -- TODO: Keep this constructor private to prevent out-of-bounds errors

 elemID :: Element -> Int
 elemID (Element (i, _)) = i

 elements :: Group -> [Element]
 elements gr = [Element (i, gr) | i <- g_elems gr]

 getGroup :: Element -> Group
 getGroup (Element (_, gr)) = gr

 getGroup' :: [Element] -> Maybe Group
 getGroup' [] = Nothing
 getGroup' (x:xs) = grp xs
  where g = getGroup x
	grp (y:ys) | y ∈ g = grp ys
	grp [] = Just g
	grp _  = Nothing

 order :: Element -> Int
 order (Element (i, gr)) = g_order gr i

 inverse :: Element -> Element
 inverse (Element (i, gr)) = Element (g_invert gr i, gr)

 identity :: Group -> Element
 identity gr = Element (0, gr)

 isIdentity :: Element -> Bool
 isIdentity (Element (i, _)) = i == 0

 infixl 7 ·
 infix  4 ∈, ∋

 (∈) :: Element -> Group -> Bool
 Element (_, g1) ∈ g2 = g1 == g2

 (∋) :: Group -> Element -> Bool
 (∋) = flip (∈)

 (·) :: Element -> Element -> Element
 Element (a, g1) · Element (b, g2) | g1 == g2  = Element (g_oper g1 a b, g1)
				   | otherwise = undefined
