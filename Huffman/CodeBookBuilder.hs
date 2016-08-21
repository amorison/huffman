module Huffman.CodeBookBuilder
( huffmanEncode
) where

import Huffman.Types
import Data.List(insert, sort, group)

-- expects a list of HT sorted by weight
reduceHTs :: (Num w, Ord w) => [HuffmanTree s w] -> HuffmanTree s w
reduceHTs [] = undefined
reduceHTs [ht1] = ht1
reduceHTs (ht1:ht2:hts) = reduceHTs $ insert (mergeHTs ht1 ht2) hts

buildFreqTable :: (Ord s) => [Symbol s] -> FrequencyTable s Int
buildFreqTable = map (\xs -> (head xs, length xs)) . group . sort

buildTree :: (Num w, Ord w) => FrequencyTable s w -> HuffmanTree s w
buildTree = reduceHTs . sort . map Leaf

-- follow the nodes of a tree up to its leaves, adding
-- bits to the obtained code
codeFromTree :: HuffmanTree s w -> HuffmanCodeBook s
codeFromTree = cftHelper ""
    where cftHelper rcode (Leaf (s, _)) = [(s, reverse rcode)]
          cftHelper rcode (Node _ ht1 ht2) = codes1 ++ codes2
              where codes1 = cftHelper ('0':rcode) ht1
                    codes2 = cftHelper ('1':rcode) ht2

huffmanEncode :: (Ord s) => [Symbol s] -> HuffmanCodeBook s
huffmanEncode = codeFromTree . buildTree . buildFreqTable
