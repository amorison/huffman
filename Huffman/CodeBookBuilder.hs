module Huffman.CodeBookBuilder
( huffmanEncode
) where

import Huffman.Types
import Data.List(insert, sort, sortOn, uncons, group, foldl')
import Data.Ord

decrement :: HuffmanCode -> HuffmanCode
decrement [] = []
decrement (lsb:hb) = if lsb
                     then False:hb
                     else True:(decrement hb)

-- expects a list of HT sorted by weight
reduceHTs :: (Num w, Ord w) => [HuffmanTree s w] -> HuffmanTree s w
reduceHTs [] = undefined
reduceHTs [ht1] = ht1
reduceHTs (ht1:ht2:hts) = reduceHTs $ insert (mergeHTs ht1 ht2) hts

buildFreqTable :: (Ord s) => [Symbol s] -> FrequencyTable s Int
buildFreqTable = map (\xs -> (head xs, length xs)) . group . sort

buildTree :: (Num w, Ord w) => FrequencyTable s w -> HuffmanTree s w
buildTree = reduceHTs . sort . map Leaf

depthLeaves :: HuffmanTree s w -> [(Int, Symbol s)]
depthLeaves = dHelper 0
    where dHelper d (Leaf (s, _)) = [(d, s)]
          dHelper d (Node _ ht1 ht2) = depths1 ++ depths2
              where depths1 = dHelper (d+1) ht1
                    depths2 = dHelper (d+1) ht2

canonicalBook :: (Ord s) => [(Int, Symbol s)] -> HuffmanCodeBook s
canonicalBook = foldl' fldFun [] . sortOn Down
    where fldFun [] (d,s) = [(s,d,replicate d True)]
          fldFun xs@((_,dx,cx):xt) (d,s) = (s,d,c):xs
              where c = decrement $ drop (dx-d) cx

codeFromTree :: (Ord s) => HuffmanTree s w -> HuffmanCodeBook s
codeFromTree = canonicalBook . depthLeaves

huffmanEncode :: (Ord s) => [Symbol s] -> HuffmanCodeBook s
huffmanEncode = codeFromTree . buildTree . buildFreqTable
