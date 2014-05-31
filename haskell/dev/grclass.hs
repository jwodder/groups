{-# LANGUAGE MultiParamTypeClasses #-}

import qualified Groups.Types.Element as E

class GroupClass g e where
 identity :: g -> e
 op       :: g -> e -> e -> e
 inverse  :: g -> e -> e
 order    :: g -> e -> Int
 size     :: g -> Int  -- This is invalid, isn't it?
 elements :: g -> [e]
 (∈)      :: e -> g -> Bool

instance GroupClass Group Int where
 identity _ = 0
 op         = g_oper
 inverse    = g_invert
 order      = g_order
 size       = g_size
 elements   = g_elems
 (∈)        = g_in

instance GroupClass (Group a) a where
 identity = g'id
 op       = g'oper
 inverse  = g'invert
 order    = g'order
 size     = g'size
 elements = g'elems
 e ∈ g    = e `elem` elements g

instance GroupClass Group E.Element where
 identity = E.identity
 op       = const (E.·)
 inverse  = const E.inverse
 order    = const E.order
 size     = g_size
 elements = E.elements
 (∈)      = (E.∈)
