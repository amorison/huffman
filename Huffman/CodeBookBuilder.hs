module Huffman.CodeBookBuilder
( huffmanEncode
) where

import Huffman.Types
import Data.List(insert, sort, sortOn, foldl')
import Data.Maybe(fromJust)
import Data.Ord
import qualified Data.Set as Set

decrement :: HuffmanCode -> HuffmanCode
decrement [] = []
decrement (lsb:hb) = if lsb
                     then False:hb
                     else True:(decrement hb)

insertSymbol :: (Ord s) => FrequencyTable s Int -> Symbol s -> FrequencyTable s Int
insertSymbol fTbl sbl = if Just swm == prSbl
                        then Set.insert (SWmapping (sbl, prW+1)) fTbl
                        else Set.insert swm fTbl
    where swm = SWmapping (sbl, 1)
          prSbl = Set.lookupLE (SWmapping (sbl,1)) fTbl
          prW = weightSW $ fromJust prSbl

buildFreqTable :: (Ord s) => [Symbol s] -> FrequencyTable s Int
buildFreqTable = foldl' insertSymbol Set.empty

-- expects a list of HT sorted by weight
reduceHTs :: (Num w, Ord w) => [HuffmanTree s w] -> HuffmanTree s w
reduceHTs [] = undefined
reduceHTs [ht1] = ht1
reduceHTs (ht1:ht2:hts) = reduceHTs $ insert (mergeHTs ht1 ht2) hts

buildTree :: (Num w, Ord w) => FrequencyTable s w -> HuffmanTree s w
buildTree = reduceHTs . sort . map Leaf . Set.toList

depthLeaves :: HuffmanTree s w -> [(Int, Symbol s)]
depthLeaves = dHelper 0
    where dHelper d (Leaf (SWmapping (s, _))) = [(d, s)]
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
