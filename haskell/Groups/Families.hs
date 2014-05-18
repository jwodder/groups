module Math.Groups.Families where
 import Data.Array
 import Data.List (sort)
 import Algorithms.Closure (closure2A)
 import Math.Groups.Types
 import Math.Groups.Internals
 import Math.Combinatorics (partitions)
 import Math.NumTheory (modInverse', factor)
 import MoreData.Lists (cartesian, cross, extZip)
 import qualified MoreData.Permutation as P
 import Ternary

 cyclic :: Int -> Group
 cyclic = mkgroup . cyclic'

 cyclic' :: Int -> Group' Int
 cyclic' n | n < 1 = undefined
 cyclic' n = Group' {
  g'size = n,
  g'elems = [0..n-1],
  g'index = id,
  g'oper = (\x y -> mod (x+y) n),
  g'invert = (`mod` n) . negate,
  g'order = cycOrd n,
  g'id = 0
 }

 direct :: Group -> Group -> Group
 direct g h = mkgroup $ direct' (unmkgroup g) (unmkgroup h)

 direct' :: Group' a -> Group' b -> Group' (a, b)
 direct' g h = Group' {
  g'size = g'size g * g'size h,
  g'elems = cartesian (g'elems g) (g'elems h),
  g'index = (\(a, b) -> g'index g a * g'size h + g'index h b),
  g'oper = (\(a1, b1) (a2, b2) -> (g'oper g a1 a2, g'oper h b1 b2)),
  g'invert = (\(a, b) -> (g'invert g a, g'invert h b)),
  g'order = (\(a, b) -> g'order g a `lcm` g'order h b),
  g'id = (g'id g, g'id h)
 }

 dihedral :: Int -> Group
 dihedral = mkgroup . dihedral'

 dihedral' :: Int -> Group' (Bool, Int)
 dihedral' n | n < 1 = undefined
 dihedral' n = Group' {
  g'size = 2*n,
  g'elems = cartesian [False,True] [0..n-1],
  g'index = (\(s,r) -> (s ?: n :? 0) + r),
  g'oper = (\(s1, r1) (s2, r2) -> (s1 /= s2, mod (r2 + (s2 ?: negate :? id) r1) n)),
  g'invert = (\(s,r) -> s ?: (s,r) :? (False, mod (-r) n)),
  g'order = (\(s,r) -> s ?: 2 :? cycOrd n r),
  g'id = (False, 0)
 }

 dicyclic :: Int -> Group
 dicyclic = mkgroup . dicyclic'

 dicyclic' :: Int -> Group' (Int, Bool)
 dicyclic' n | n < 1 = undefined
 dicyclic' n = Group' {
  g'size = 4*n,
  g'elems = cartesian [0..2*n-1] [False, True],
  g'index = (\(i,j) -> (j ?: 2*n :? 0) + i),
  g'oper = (\(i1, j1) (i2, j2) -> (mod (i1 + (j1 ?: negate :? id) i2 + (j1 && j2 ?: n :? 0)) (2*n), j1 /= j2)),
  g'invert = (\(i,j) -> (mod (j ?: i+n :? (-i)) (2*n), j)),
  g'order = (\(i,j) -> j ?: 4 :? cycOrd (2*n) i),
  g'id = (0, False)
 }

 -- |@genquaternion n@ returns $Q_{2^{n+1}}$
 genquaternion :: Int -> Group
 genquaternion = mkgroup . genquaternion'

 genquaternion' :: Int -> Group' (Int, Bool)
 genquaternion' n | n < 2 = undefined
 genquaternion' n = dicyclic' $ 2 ^ pred n

 trivial :: Group
 trivial = Group (array ((0,0), (0,0)) [((0,0), 0)], array (0,0) [(0, (0,1))])

 trivial' :: Group' ()
 trivial' = Group' {
  g'size   = 1,
  g'elems  = [()],
  g'index  = const 0,
  g'oper   = const,
  g'invert = id,
  g'order  = const 1,
  g'id     = ()
 }

 boolean :: Group
 boolean = mkgroup boolean'

 boolean' :: Group' Bool
 boolean' = Group' {
  g'size = 2,
  g'elems = [False, True],
  g'index = fromEnum,
  g'oper = (/=),
  g'invert = id,
  g'order = succ . fromEnum,
  g'id = False
 }

 klein4 :: Group
 klein4 = mkgroup klein4'

 klein4' :: Group' (Bool, Bool)
 klein4' = direct' boolean' boolean'

 quaternion :: Group
 quaternion = genquaternion 2

 quaternion' :: Group' (Int, Bool)
 quaternion' = genquaternion' 2

 symmetric :: Int -> Group
 symmetric = mkgroup . symmetric'

 symmetric' :: Int -> Group' P.Permutation
 symmetric' n | n < 1 = undefined
 symmetric' n = Group' {
  g'size = product [1..n],
  -- TODO: Compare the performances of the two methods for generating $S_n$
  --g'elems = closure2A P.compose $ map (P.setDegree' n . P.transpose 1) [2..n],
  g'elems = if n == 1 then [P.identity' 1]
	    else closure2A P.compose [P.fromCycle [1..n],
				      P.setDegree' n $ P.transpose 1 2],
  g'index = P.lehmer,
  g'oper = P.compose,
  g'invert = P.invert,
  g'order = P.order,
  g'id = P.identity' n
 }

 alternating :: Int -> Group
 alternating = mkgroup . alternating'

 alternating' :: Int -> Group' P.Permutation
 alternating' n | n < 1 = undefined
 alternating' n = Group' {
  g'size = n == 1 ?: 1 :? div facN 2,
  g'elems = els,
  g'index = (!) dex . P.lehmer,
  g'oper = P.compose,
  g'invert = P.invert,
  g'order = P.order,
  g'id = P.identity' n
 } where facN = product [1..n]
	 -- TODO: Prove that the below is correct!
	 els = closure2A P.compose [P.setDegree' n $ P.fromCycle [1,a,b]
				     | a <- [2..n], b <- [a+1..n]]
	 dex = array (0, facN-1) $ zip (sort $ map P.lehmer els) [0..]

 multiplicN :: Int -> Group  -- Rename to "autCyclic"?
 multiplicN = mkgroup . multiplicN'

 multiplicN' :: Int -> Group' Int
 multiplicN' n | n < 1 = undefined
 multiplicN' n = Group' {
  g'size = length coprimes,
  g'elems = coprimes,
  g'index = (!) dex,
  g'oper = (\x y -> mod (x*y) n),
  g'invert = (`modInverse'` n),
  g'order = (\x -> succ $ length $ takeWhile (/= 1) $ iterate (\y -> mod (x*y) n) x),
  g'id = 1
 } where coprimes = filter ((== 1) . gcd n) [1..n]
	 dex = array (1,n) $ zip coprimes [0..]

 holCyclic :: Int -> Group
 holCyclic = mkgroup . holCyclic'

 holCyclic' :: Int -> Group' (Int, Int)
 holCyclic' n = semidirect' (cyclic' n) (multiplicN' n) (\x y -> mod (x*y) n)

 semidirect' :: Group' a -> Group' b -> (b -> a -> a) -> Group' (a, b)
 -- It is the user's responsibility to ensure that the third argument is a
 -- homomorphism from the second group to the automorphism group of the first.
 semidirect' g h φ = Group' {
  g'size = g'size g * g'size h,
  g'elems = cartesian (g'elems g) (g'elems h),
  g'index = (\(x,y) -> g'index g x * g'size h + g'index h y),
  g'oper = gop,
  g'invert = (\(x,y) -> let y' = g'invert h y in (φ y' $ g'invert g x, y')),
  g'id = (g'id g, g'id h),
  -- TODO: Prove that the below is correct!
  g'order = (\xy -> let (xs, q:_) = break semiID $ iterate (gop xy) xy
		    in (length xs + 1) * semiOrd q)
 } where gop (gx, hx) (gy, hy) = (g'oper g gx $ φ hx gy, g'oper h hx hy)
	 semiID (a,b) = g'index g a == 0 || g'index h b == 0
	 semiOrd (a,b) = if g'index g a == 0 then g'order h b else g'order g a

 cycSemiCyc :: Int -> Int -> Int -> Group
 cycSemiCyc n m i = mkgroup $ cycSemiCyc' n m i

 cycSemiCyc' :: Int -> Int -> Int -> Group' (Int, Int)
 cycSemiCyc' n m i | mod (i^m) n/=1 = error "cycSemiCyc': invalid homomorphism"
 cycSemiCyc' n m i = semidirect' (cyclic' n) (cyclic' m) (\y x -> mod (x*i^y) n)

 abelians :: Int -> [(Group, [Int])]
 abelians n | n < 1 = []
 abelians n = map (\xs -> (foldl1 direct $ map cyclic xs, xs))
	    $ map (foldl1 $ \a b -> map (uncurry (*)) $ extZip 1 1 a b) $ cross
	    $ map (\(p,k) -> map (map (p^)) $ partitions k) $ factor n
