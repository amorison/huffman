module Huffman
where

import Data.Function(on)
import Data.List(insert, sort, group)

type Symbol s = s
type Weight w = w
type HuffmanCode = String -- e.g. "0111"

type SWmapping s w = (Symbol s, Weight w)
type FrequencyTable s w = [SWmapping s w]
type SHmapping s = (Symbol s, HuffmanCode)
type HuffmanTable s = [SHmapping s]

-- Huffman Tree and associated functions
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

-- expects a list of HT sorted by weight
reduceHTs :: (Num w, Ord w) => [HuffmanTree s w] -> HuffmanTree s w
reduceHTs [] = undefined
reduceHTs [ht1] = ht1
reduceHTs (ht1:ht2:hts) = reduceHTs $ insert (mergeHTs ht1 ht2) hts


-- Core functions
buildFreqTable :: (Ord s) => [Symbol s] -> FrequencyTable s Int
buildFreqTable = map (\xs -> (head xs, length xs)) . group . sort

buildTree :: (Num w, Ord w) => FrequencyTable s w -> HuffmanTree s w
buildTree = reduceHTs . sort . map Leaf

-- follow the nodes of a tree up to its leaves, adding
-- bits to the obtained code
codeFromTree :: HuffmanTree s w -> HuffmanTable s
codeFromTree = cftHelper ""
    where cftHelper rcode (Leaf (s, _)) = [(s, reverse rcode)]
          cftHelper rcode (Node _ ht1 ht2) = codes1 ++ codes2
              where codes1 = cftHelper ('0':rcode) ht1
                    codes2 = cftHelper ('1':rcode) ht2

huffmanEncode :: (Ord s) => [Symbol s] -> HuffmanTable s
huffmanEncode = codeFromTree . buildTree . buildFreqTable
