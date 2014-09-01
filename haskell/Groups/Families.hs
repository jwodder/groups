module Groups.Families where
 import Data.Array
 import Data.List (sort)
 import Closure (closure2A)
 import Groups.Type
 import Groups.Internals
 import qualified Permutation as P

 cyclic :: Int -> Group Int
 cyclic n | n < 1 = undefined
 cyclic n = Group {
  gsize = n,
  gelems = [0..n-1],
  gindex = id,
  goper = (\x y -> mod (x+y) n),
  ginvert = (`mod` n) . negate,
  gorder = cycOrd n,
  gid = 0
 }

 direct :: Group a -> Group b -> Group (a,b)
 direct g h = Group {
  gsize = gsize g * gsize h,
  gelems = cartesian (gelems g) (gelems h),
  gindex = (\(a,b) -> gindex g a * gsize h + gindex h b),
  goper = (\(a1, b1) (a2, b2) -> (goper g a1 a2, goper h b1 b2)),
  ginvert = (\(a,b) -> (ginvert g a, ginvert h b)),
  gorder = (\(a,b) -> gorder g a `lcm` gorder h b),
  gid = (gid g, gid h)
 }

 dihedral :: Int -> Group (Bool, Int)
 dihedral n | n < 1 = undefined
 dihedral n = Group {
  gsize = 2*n,
  gelems = cartesian [False,True] [0..n-1],
  gindex = (\(s,r) -> (s ?: n :? 0) + r),
  goper = (\(s1, r1) (s2, r2) -> (s1 /= s2, mod (r2 + (s2 ?: negate :? id) r1) n)),
  ginvert = (\(s,r) -> s ?: (s,r) :? (False, mod (-r) n)),
  gorder = (\(s,r) -> s ?: 2 :? cycOrd n r),
  gid = (False, 0)
 }

 dicyclic :: Int -> Group (Int, Bool)
 dicyclic n | n < 2 = undefined
 dicyclic n = Group {
  gsize = 4*n,
  gelems = cartesian [0..2*n-1] [False, True],
  gindex = (\(i,j) -> i*2 + fromEnum j),
  goper = (\(i1, j1) (i2, j2) -> (mod (i1 + (j1 ?: negate :? id) i2 + (j1 && j2 ?: n :? 0)) (2*n), j1 /= j2)),
  ginvert = (\(i,j) -> (mod (j ?: i+n :? (-i)) (2*n), j)),
  gorder = (\(i,j) -> j ?: 4 :? cycOrd (2*n) i),
  gid = (0, False)
 }

 -- |@genquaternion n@ returns $Q_{2^{n+1}}$
 genquaternion :: Int -> Group (Int, Bool)
 genquaternion n | n < 2 = undefined
 genquaternion n = dicyclic $ 2 ^ pred n

 trivial :: Group ()
 trivial = Group {
  gsize   = 1,
  gelems  = [()],
  gindex  = const 0,
  goper   = const,
  ginvert = id,
  gorder  = const 1,
  gid     = ()
 }

 boolean :: Group Bool
 boolean = Group {
  gsize = 2,
  gelems = [False, True],
  gindex = fromEnum,
  goper = (/=),
  ginvert = id,
  gorder = succ . fromEnum,
  gid = False
 }

 klein4 :: Group (Bool, Bool)
 klein4 = direct boolean boolean

 quaternion :: Group (Int, Bool)
 quaternion = genquaternion 2

 symmetric :: Int -> Group P.Permutation
 symmetric n | n < 1 = undefined
 symmetric n = Group {
  gsize = product [1..n],
  gelems = P.s_n n,
  gindex = P.lehmer,
  goper = P.compose,
  ginvert = P.inverse,
  gorder = P.order,
  gid = P.identity
 }

 alternating :: Int -> Group P.Permutation
 alternating n | n < 1 = undefined
 alternating n = Group {
  gsize = n == 1 ?: 1 :? div facN 2,
  gelems = els,
  gindex = (!) dex . P.lehmer,
  goper = P.compose,
  ginvert = P.inverse,
  gorder = P.order,
  gid = P.identity
 } where facN = product [1..n]
	 els = filter P.isEven $ P.s_n n
	 dex = array (0, facN-1) $ zip (map P.lehmer els) [0..]

 permutation :: [P.Permutation] -> Group P.Permutation
 permutation [] = symmetric 1
 permutation perms = Group {
  gsize   = qty,
  gelems  = els,
  gindex  = (!) dex . P.lehmer,
  goper   = P.compose,
  ginvert = P.inverse,
  gorder  = P.order,
  gid     = P.identity
 } where els = closure2A P.compose perms
	 -- Counting the quantity and largest Lehmer code while zipping should
	 -- be faster than using `zip`, `length`, and `last` separately.
	 (zipped, (qty, maxCode)) = zippy 0 $ sort $ map P.lehmer els
	 zippy i [c] = ([(c,i)], (i+1, c))
	 zippy i (c:xs) = (c,i) &: zippy (i+1) xs
	 zippy _ [] = error "Impossible runtime error in permutation"
	 dex = array (0, maxCode) zipped

 autCyclic :: Int -> Group Int
 autCyclic n | n < 1 = undefined
 autCyclic n = Group {
  gsize = length coprimes,
  gelems = coprimes,
  gindex = (!) dex,
  goper = (\x y -> mod (x*y) n),
  ginvert = (`modInverse` n),
  gorder = (\x -> succ $ length $ takeWhile (/= 1) $ iterate (\y -> mod (x*y) n) x),
  gid = 1
 } where coprimes = filter ((== 1) . gcd n) [1..n]
	 dex = array (1,n) $ zip coprimes [0..]

 holCyclic :: Int -> Group (Int, Int)
 holCyclic n = semidirect (cyclic n) (autCyclic n) (\x y -> mod (x*y) n)

 semidirect :: (Eq a, Eq b) => Group a -> Group b -> (b->a->a) -> Group (a,b)
 -- It is the user's responsibility to ensure that the third argument is a
 -- homomorphism from the second group to the automorphism group of the first.
 semidirect g h φ = Group {
  gsize = gsize g * gsize h,
  gelems = cartesian (gelems g) (gelems h),
  gindex = (\(x,y) -> gindex g x * gsize h + gindex h y),
  goper = gop,
  ginvert = (\(x,y) -> let y' = ginvert h y in (φ y' $ ginvert g x, y')),
  gid = (gid g, gid h),
  -- TODO: Prove that the below is correct!
  gorder = (\xy -> let (xs, q:_) = break semiID $ iterate (gop xy) xy
		   in (length xs + 1) * semiOrd q)
 } where gop (gx, hx) (gy, hy) = (goper g gx $ φ hx gy, goper h hx hy)
	 semiID (a,b)  = a == gid g || b == gid h
	 semiOrd (a,b) = if a == gid g then gorder h b else gorder g a

 cycSemiCyc :: Int -> Int -> Int -> Group (Int, Int)
 cycSemiCyc n m i | mod (i^m) n/=1 = error "cycSemiCyc: invalid homomorphism"
 cycSemiCyc n m i = semidirect (cyclic n) (cyclic m) (\y x -> mod (x*i^y) n)

 -- |@abelians n@ returns a list of all abelian groups of order @n@ together
 -- with their invariant factors.
 abelians :: Int -> [(Group Int, [Int])]
 abelians n | n < 1 = []
 abelians 1 = [(tabulate trivial, [])]
 abelians n = map (\xs -> (foldl1 (\g h -> tabulate $ direct g h)
			   $ map cyclic xs, xs))
	    $ map (foldl1 $ \a b -> map (uncurry (*)) $ extZip 1 1 a b) $ cross
	    $ map (\(p,k) -> map (map (p^)) $ partitions k) $ factor n
