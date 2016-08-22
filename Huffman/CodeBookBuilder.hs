module Huffman.CodeBookBuilder
( huffmanEncode
) where

import Huffman.Types
import Data.List(insert, sort, sortOn, uncons, group, foldl')

increment :: HuffmanCode -> HuffmanCode
increment [] = [True]
increment code = if not lsb
                 then True:hb
                 else False:(increment hb)
                     where Just (lsb, hb) = uncons code

-- expects a list of HT sorted by weight
reduceHTs :: (Num w, Ord w) => [HuffmanTree s w] -> HuffmanTree s w
reduceHTs [] = undefined
reduceHTs [ht1] = ht1
reduceHTs (ht1:ht2:hts) = reduceHTs $ insert (mergeHTs ht1 ht2) hts

buildFreqTable :: (Ord s) => [Symbol s] -> FrequencyTable s Int
buildFreqTable = map (\xs -> (head xs, length xs)) . group . sort

buildTree :: (Num w, Ord w) => FrequencyTable s w -> HuffmanTree s w
buildTree = reduceHTs . sort . map Leaf

depthLeaves :: (Integral d) => HuffmanTree s w -> [(Symbol s, d)]
depthLeaves = dHelper 0
    where dHelper d (Leaf (s, _)) = [(s, d)]
          dHelper d (Node _ ht1 ht2) = depths1 ++ depths2
              where depths1 = dHelper (d+1) ht1
                    depths2 = dHelper (d+1) ht2

canonicalBook :: (Ord s) => [(Symbol s, Int)] -> HuffmanCodeBook s
canonicalBook = reverse . foldl' fldFun [] . sortOn snd . sortOn fst
    where fldFun [] (s,d) = [(s,d,replicate d False)]
          fldFun xs@((_,dx,cx):xt) (s,d) = (s,d,c):xs
              where c = replicate (d-dx) False ++ increment cx

codeFromTree :: (Ord s) => HuffmanTree s w -> HuffmanCodeBook s
codeFromTree = canonicalBook . depthLeaves

huffmanEncode :: (Ord s) => [Symbol s] -> HuffmanCodeBook s
huffmanEncode = codeFromTree . buildTree . buildFreqTable
