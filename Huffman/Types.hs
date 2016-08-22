module Huffman.Types where

import Data.Function(on)

type Symbol s = s
type Weight w = w
type HuffmanCode = [Bool]

type SWmapping s w = (Symbol s, Weight w)
type FrequencyTable s w = [SWmapping s w]

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
weightHT (Leaf sw) = snd sw
weightHT (Node w _ _) = w

mergeHTs :: (Num w) => HuffmanTree s w -> HuffmanTree s w -> HuffmanTree s w
mergeHTs ht1 ht2 = Node (w1+w2) ht1 ht2
    where w1 = weightHT ht1
          w2 = weightHT ht2
