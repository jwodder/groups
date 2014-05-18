module GrData (grdata) where
import Data.Array
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.IntSet as ISet
import qualified Data.Map as Map
import qualified Data.Set as Set
import Groups
import Groups.Subgroups
import Groups.Internals  -- factor and Ternary

grdata :: (String, Group, [String]) -> String
grdata (name, g, props) | g_size g == 1 = unlines [
  name,
  intercalate " " $ "1 abelian nilpotent solvable" : props,
  "1",
  "rank=0",
  "exp=1",
  "nilpotence=0"
 ]
grdata (name, g, props) = unlines [
  name,
  intercalate " " $ [
   show n,
   abel ?: "abelian" :? "nonabelian",
   nilpot ?: "nilpotent" :? "nonnilpotent",
   nilpot || and [subQtys ! i /= 0 | i <- divisors, gcd i (div n i) == 1]
      ?: "solvable" :? "nonsolvable",  -- insoluble?
   or [q /= 0 | (m,q) <- assocs normQtys, m /= 1, m /= n]
      ?: "nonsimple" :? "simple"
  ] ++ props,
  "1=///" ++ show (ccQtys ! 1) ++ ' ' : intercalate " " [
   show i ++ '=' : show (ords     ! i)
	  ++ '/' : show (subQtys  ! i)
	  ++ '/' : show (normQtys ! i)
	  ++ '/' : show (ccQtys   ! i)
   | i <- divisors
  ],
  "rank=" ++ show (Set.findMin $ Set.map ISet.size $ subs Map.! self),
  "exp="  ++ show (foldl lcm 1 [k | (k,a) <- assocs ords, a /= 0]),
  "nilpotence=" ++ (abel ?: "1" :? nilpot ?: show (fromJust $ nilpotence g) :? "nil")
 ] where n        = g_size g
	 self     = ISet.fromDistinctAscList $ g_elems g
	 divisors = filter ((== 0) . mod n) [2..n]
	 subs     = subgroupGens g
	 ords     = freqArray (1,n) $ map snd $ elems $ gr_dat g
	 subQtys  = freqArray (1,n) $ map ISet.size $ Map.keys subs
	 normQtys = freqArray (1,n) $ map ISet.size $ filter (isNormal' g)
						    $ Map.keys subs
	 ccQtys   = freqArray (1,n) $ map ss_size $ conjugacies g
	 abel     = isAbelian g
	 nilpot   = abel || and [subQtys ! (p^a) == normQtys ! (p^a)
				 | (p,a) <- factor n]

freqArray :: Ix a => (a,a) -> [a] -> Array a Int
freqArray b = accumArray (+) 0 b . map (\x -> (x,1))

isNormal' :: Group -> ISet.IntSet -> Bool
isNormal' g h = all norms $ g_elems g
 where (⋅) = g_oper g
       norms x = all (`ISet.member` h) $ map ((⋅ g_invert g x) . (x ⋅)) 
				       $ ISet.toList h
