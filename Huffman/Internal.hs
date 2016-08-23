module Huffman.Internal where

import Data.Function(on)
import Data.Ord(comparing)
import qualified Data.Set as Set

type Symbol s = s
type Message s = [Symbol s]
type Weight = Int
type CodeLength = Int
type HuffmanCode = [Bool]

newtype SWmapping s = SWmapping (Symbol s, Weight)
instance (Eq s) => Eq (SWmapping s) where
    (==) = (==) `on` symbolSW
instance (Ord s) => Ord (SWmapping s) where
    compare = comparing symbolSW

symbolSW :: SWmapping s -> Symbol s
symbolSW (SWmapping (s, _)) = s

weightSW :: SWmapping s -> Weight
weightSW (SWmapping (_, w)) = w

type FrequencyTable s = Set.Set (SWmapping s)

type CodeBook s = [(Symbol s, Weight, CodeLength, HuffmanCode)]

data HuffmanTree s = Leaf (SWmapping s) | Node Weight
                                               (HuffmanTree s)
                                               (HuffmanTree s)
instance Eq (HuffmanTree s) where
    (==) = (==) `on` weightHT
instance Ord (HuffmanTree s) where
    compare = comparing weightHT

weightHT :: HuffmanTree s -> Weight
weightHT (Leaf sw) = weightSW sw
weightHT (Node w _ _) = w

mergeHTs :: HuffmanTree s -> HuffmanTree s -> HuffmanTree s
mergeHTs ht1 ht2 = Node (w1+w2) ht1 ht2
    where w1 = weightHT ht1
          w2 = weightHT ht2
