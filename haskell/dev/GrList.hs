module GrList (groupList, o32nonA) where
 import Groups
 import Groups.Internals  -- isSublist and Ternary

 type GrEntry = (String, Group, [String])

 groupList :: [GrEntry]
 -- TODO: Store this as a list of pairs of abelian & nonabelian groups of each
 -- order :: [([GrEntry], [GrEntry])].  Alternatively, have one list in this
 -- format and make `groupList` just a flattening of the other list.
 groupList = [
   ("1", trivial, ["cyclic", "symmetric", "alternating"]),
   cyc 2,
   cyc 3,
   cyc 4, v4,
   cyc 5,
   cyc 6,
      sym 3,
   cyc 7,
   cyc 8, cyc 4 ⨯ cyc 2, elemAbel 2 3,
      quat 2,
      dih 4,
   cyc 9, elemAbel 3 2,
   cyc 10,
      dih 5,
   cyc 11,
   cyc 12, cyc 6 ⨯ cyc 2,
      alt 4,
      dih 6,
      csc 3 4 (-1),
   cyc 13,
   cyc 14,
      dih 7,
   cyc 15,
   cyc 16, cyc 8 ⨯ cyc 2, cyc 4 ⨯ cyc 4, cyc 4 ⨯ v4, elemAbel 2 4,
      quat 3,
      qd16,
      m16,
      quat 2 ⨯ cyc 2,
      q8sz2,
      dih 4 ⨯ cyc 2,
      csc 4 4 (-1),
      v4sz4,
      dih 8,
   cyc 17,
   cyc 18, cyc 6 ⨯ cyc 3,
      dih 9,
      sym 3 ⨯ cyc 3,
      gdih $ elemAbel 3 2,
   cyc 19,
   cyc 20, cyc 10 ⨯ cyc 2,
      ("F_{20}", cycSemiCyc 5 4 3, []),
      dic 5,
      dih 10,
   cyc 21,
      csc 7 3 2,
   cyc 22,
      dih 11,
   cyc 23,

   cyc 24, cyc 12 ⨯ cyc 2, cyc 6 ⨯ v4,
      dic 6,
      quat 2 ⨯ cyc 3,
      csc 3 4 (-1) ⨯ cyc 2,
      dih 4 ⨯ cyc 3,
      sym 3 ⨯ cyc 4,
      alt 4 ⨯ cyc 2,
      sym 4,
      dih 12,
      dih 6 ⨯ cyc 2,
      -- Q_8\rtimes\Z_3 ???
      -- ???
      -- ???

   cyc 25, elemAbel 5 2,
   cyc 26,
      dih 13,

   cyc 27, cyc 9 ⨯ cyc 3, elemAbel 3 3,
      -- ???
      -- ???

   cyc 28, cyc 14 ⨯ cyc 2,
      dih 14,
      dic 7,
   cyc 29,

   cyc 30,
      dih 15,
      -- ???
      -- ???

   cyc 31,
   cyc 32, cyc 16 ⨯ cyc 2, cyc 8 ⨯ cyc 4, cyc 8 ⨯ v4, cyc 4 ⨯ cyc 4 ⨯ cyc 2,
      cyc 4 ⨯ elemAbel 2 3, elemAbel 2 5
  ] ++ o32nonA

 v4 :: GrEntry
 v4 = ("V_4", klein4, ["dihedral"])

 qd16 :: GrEntry
 qd16 = ("QD_{16}", cycSemiCyc 8 2 3, [])

 m16 :: GrEntry
 m16 = ("M", cycSemiCyc 8 2 5, [])

 q8sz2 :: GrEntry
 q8sz2 = ("Q_8\\rtimes\\Z_2", mkgroup $ semidirect' quaternion' boolean' (\x (i,j) -> x ?: (mod (i + 2*mod i 2 + 2*fromEnum j) 4, j) :? (i,j)), [])

 v4sz4 :: GrEntry
 v4sz4 = ("V_4\\rtimes\\Z_4", mkgroup $ semidirect' klein4' (cyclic' 4) (\x (a,b) -> odd x ?: (b,a) :? (a,b)), [])

 o32nonA :: [GrEntry]
 o32nonA = [
   quat 4,
   dih 16,
   csc 16 2 7,
   quat 3 ⨯ cyc 2,
   qd16 ⨯ cyc 2,
   dih 8 ⨯ cyc 2,
   csc 8 4 (-1),
   csc 8 4 3,
   ("\\Hol(\\Z_8)", holCyclic 8, []),  -- ≅ \Z_8\rtimes V_4
   m16 ⨯ cyc 2,
   quat 2 ⨯ v4,
   q8sz2 ⨯ cyc 2,
   dih 4 ⨯ v4,
   csc 4 4 (-1) ⨯ cyc 2,
   v4sz4 ⨯ cyc 2,
   quat 2 ⨯ cyc 4,
   dih 4 ⨯ cyc 4,
   csc 8 4 5,
   csc 16 2 9,
   gdih $ cyc 4 ⨯ cyc 4
   -- Q_8\rtimes\Z_4
   -- Q_8\rtimes V_4
  ]

 cyc :: Int -> GrEntry
 cyc n = (sub "\\Z" n, cyclic n, ["cyclic"])

 dih :: Int -> GrEntry
 dih n = (sub "\\Dih" n, dihedral n, ["dihedral"])

 gdih :: GrEntry -> GrEntry
 gdih (n, g, _) = ("\\Dih(" ++ n ++ ")", mkgroup $ semidirect' g' boolean' (?: g'invert g' :? id), [])
  where g' = unmkgroup g

 elemAbel :: Int -> Int -> GrEntry
 elemAbel p n = (sub "E" $ p^n, foldl1 direct $ replicate n $ cyclic p, [])

 sym :: Int -> GrEntry
 sym n = (sub "S" n, symmetric n, ["symmetric"])

 alt :: Int -> GrEntry
 alt n = (sub "A" n, alternating n, ["alternating"])

 dic :: Int -> GrEntry
 dic n = (sub "\\Dic" n, dicyclic n, ["dicyclic"])

 quat :: Int -> GrEntry
 quat n = (sub "Q" $ 2^(n+1), genquaternion n, ["dicyclic", "quaternion"])

 csc :: Int -> Int -> Int -> GrEntry
 csc a b c = (sub "\\Z" a ++ op ++ sub "\\Z" b, cycSemiCyc a b c, [])
  where op = c == -1 ?: "\\rtimes" :? sub "\\rtimes" c

 (⨯) :: GrEntry -> GrEntry -> GrEntry
 (n1, g1, _) ⨯ (n2, g2, _) = (binop n1 "\\times" n2, direct g1 g2, [])

 sub :: String -> Int -> String
 sub s n = s ++ '_' : sub' (show n)
  where sub' [c] = [c]
	sub' xs = '{' : xs ++ "}"

 binop :: String -> String -> String -> String
 binop x "\\times" y = (isSublist "rtimes" x ?: '(' : x ++ ")" :? x)
		    ++ "\\times"
		    ++ (head y' `elem` "\\(){}" ?: "" :? "{}") ++ y'
  where y' = isSublist "rtimes" y ?: '(' : y ++ ")" :? y
 binop x op y = (isSublist "times" x ?: '(' : x ++ ")" :? x) ++ op
	     ++ (head y' `elem` "\\(){}" ?: "" :? "{}") ++ y'
  where y' = isSublist "times" y ?: '(' : y ++ ")" :? y
