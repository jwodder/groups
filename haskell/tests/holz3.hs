import Data.Array
import Data.List (intercalate)
import Groups
import Groups.Internals (modInverse, cartesian)

group = holCyclic 3

main = mapM_ (\x -> putStrLn $ intercalate "\t" $ map (show . elemID)
			     $ map (x ·) $ elements group) $ elements group

--multiplicN :: Int -> Group  -- Rename to "autCyclic"?
--multiplicN = mkgroup . multiplicN'

multiplicN' :: Int -> Group' Int
multiplicN' n | n < 1 = undefined
multiplicN' n = Group' {
 g'size = length coprimes,
 g'elems = coprimes,
 g'index = dex,
 g'oper = (\x y -> mod (x*y) n),
 g'invert = (`modInverse` n),
 g'order = ordDex,
 g'id = 1
} where (coprimes, dex, ordDex) = coprimedex n

holCyclic :: Int -> Group
holCyclic = mkgroup . holCyclic'

holCyclic' :: Int -> Group' (Int, Int)
holCyclic' n = semidirect' (cyclic' n) (multiplicN' n) (*)

coprimedex :: Int -> ([Int], Int -> Int, Int -> Int)
-- indexing the elements of \Z_n^\times
coprimedex n = (coprimes, (!) dex, (!) ordDex)
 where coprimes = filter ((== 1) . gcd n) [1..n-1]
       dex = array (0, n-1) $ zip coprimes [0..]
       ordDex = array (0, n-1) $ (1,1) : [(a, ords a 1 a) | a <- tail coprimes]
       ords a i bi = if bi < a then i * ordDex ! bi
		     else ords a (i+1) (mod (bi*a) n)

semidirect' :: Group' a -> Group' b -> (b -> a -> a) -> Group' (a, b)
-- It is the user's responsibility to ensure that the third argument is a
-- homomorphism from @Group a@ to the automorphism group of @Group b@.
semidirect' g h φ = Group' {
 g'size = g'size g * g'size h,
 g'elems = cartesian (g'elems g) (g'elems h),
 g'index = (\(a, b) -> g'index g a * g'size h + g'index h b),
 g'oper = (\(gx, hx) (gy, hy) -> (g'oper g gx $ φ hx gy, g'oper h hx hy)),
 g'invert = (\(x, y) -> let y' = g'invert h y in (φ y' $ g'invert g x, y')),
 g'id = (g'id g, g'id h),
 g'order = undefined
}
