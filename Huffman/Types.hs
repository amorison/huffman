module Huffman.Types where

import Data.Function(on)
import Data.Set(Set)

type Symbol s = s
type Weight w = w
type HuffmanCode = [Bool]

newtype SWmapping s w = SWmapping (Symbol s, Weight w) deriving (Show)
instance (Eq s) => Eq (SWmapping s w) where
    (==) = (==) `on` symbolSW
instance (Ord s) => Ord (SWmapping s w) where
    compare = compare `on` symbolSW

symbolSW :: SWmapping s w -> Symbol s
symbolSW (SWmapping (s, _)) = s

weightSW :: SWmapping s w -> Weight w
weightSW (SWmapping (_, w)) = w

type FrequencyTable s w = Set (SWmapping s w)

type HuffmanCodeBook s = [(Symbol s, Int, HuffmanCode)]

data HuffmanTree s w = Leaf (SWmapping s w) | Node (Weight w)
                                                   (HuffmanTree s w)
                                                   (HuffmanTree s w)
    deriving (Show)
instance (Eq w) => Eq (HuffmanTree s w) where
    (==) = (==) `on` weightHT
instance (Ord w) => Ord (HuffmanTree s w) where
    compare = compare `on` weightHT

weightHT :: HuffmanTree s w -> Weight w
weightHT (Leaf sw) = weightSW sw
weightHT (Node w _ _) = w

mergeHTs :: (Num w) => HuffmanTree s w -> HuffmanTree s w -> HuffmanTree s w
mergeHTs ht1 ht2 = Node (w1+w2) ht1 ht2
    where w1 = weightHT ht1
          w2 = weightHT ht2
