module Huffman.Statistics
( entropy
) where

import Huffman.Internal

lg2 :: (Floating a) => a -> a
lg2 = logBase 2

entropy :: (Floating a) => CodeBook -> (a, a)
entropy = (\(eO, eH, wTot) -> (lg2 wTot - eO/wTot, eH/wTot)) . foldr fn (0,0,0)
    where fn (_,w,l,_) (eO, eH, wTot) = (eO+wi*lg2 wi, eH+li*wi, wTot+wi)
            where wi = fromIntegral w
                  li = fromIntegral l
