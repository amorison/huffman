module Huffman
where

import Data.Function(on)
import Data.List(insert, sort, group)

type Symbol = Char
type Weight = Int
type HuffmanCode = String -- e.g. "0111"

type SWmapping = (Symbol, Weight)
type FrequencyTable = [SWmapping]
type SHmapping = (Symbol, HuffmanCode)
type HuffmanTable = [SHmapping]

-- Huffman Tree and associated functions
data HuffmanTree = Leaf SWmapping | Node Weight HuffmanTree HuffmanTree deriving (Show)
instance Eq HuffmanTree where
    (==) = (==) `on` weightHT
instance Ord HuffmanTree where
    compare = compare `on` weightHT

weightHT :: HuffmanTree -> Weight
weightHT (Leaf sw) = snd sw
weightHT (Node w _ _) = w

mergeHTs :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergeHTs ht1 ht2 = Node (w1+w2) ht1 ht2
    where w1 = weightHT ht1
          w2 = weightHT ht2

-- expects a list of HT sorted by weight
reduceHTs :: [HuffmanTree] -> HuffmanTree
reduceHTs [] = undefined
reduceHTs [ht1] = ht1
reduceHTs (ht1:ht2:hts) = reduceHTs $ insert (mergeHTs ht1 ht2) hts


-- Core functions
buildFreqTable :: String -> FrequencyTable
buildFreqTable = map (\xs -> (head xs, length xs)) . group . sort

buildTree :: FrequencyTable -> HuffmanTree
buildTree = reduceHTs . sort . map Leaf

-- follow the nodes of a tree up to its leaves, adding
-- bits to the obtained code
codeFromTree :: HuffmanTree -> HuffmanTable
codeFromTree = cftHelper ""
    where cftHelper rcode (Leaf (s, _)) = [(s, reverse rcode)]
          cftHelper rcode (Node _ ht1 ht2) = codes1 ++ codes2
              where codes1 = cftHelper ('0':rcode) ht1
                    codes2 = cftHelper ('1':rcode) ht2

huffmanEncode :: String -> HuffmanTable
huffmanEncode = codeFromTree . buildTree . buildFreqTable
