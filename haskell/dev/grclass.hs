{-# LANGUAGE MultiParamTypeClasses #-}

class GroupClass g e where
 identity :: g -> e
 op       :: g -> e -> e -> e
 inverse  :: g -> e -> e
 elements :: g -> [e]
 (∈)      :: e -> g -> Bool
 order    :: g -> e -> Int
 size     :: g -> Int

instance GroupClass Group Int where
 identity _ = 0
 op         = g_oper
 inverse    = g_invert
 elements   = g_elems
 (∈)        = g_in
 order      = g_order
 size       = g_size

instance GroupClass (Group a) a where
 identity = g'id
 op       = g'oper
 inverse  = g'invert
 elements = g'elems
 e ∈ g    = e `elem` elements g
 order    = g'order
 size     = g'size
