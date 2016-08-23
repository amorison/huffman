module Huffman.CodeBook
( Symbol
, Message
, CodeBook
, huffmanEncode
) where

import Huffman.Internal
import Data.List(insert, sort, sortOn, foldl')
import Data.Maybe(fromJust)
import Data.Ord
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL


-- construction of the code book is a three steps process:
--   - construction of the FrequencyTable of the Symbols
--     in the message
--   - construction of the Huffman tree from this table
--   - determination of the canonical code that should
--     be attributed to each Symbol based on this tree
huffmanEncode :: Message -> CodeBook
huffmanEncode = codeFromTree . buildTree . buildFreqTable


-- construction of FrequencyTable
--   each Symbol is read after the other and its frequency
--   is updated
buildFreqTable :: Message -> FrequencyTable
buildFreqTable = BL.foldl' insertSymbol Set.empty

insertSymbol :: FrequencyTable -> Symbol -> FrequencyTable
insertSymbol fTbl sbl = if Just swm == prSbl
                        then Set.insert (SWmapping (sbl, prW+1)) fTbl
                        else Set.insert swm fTbl
    where swm = SWmapping (sbl, 1)
          prSbl = Set.lookupLE (SWmapping (sbl,1)) fTbl
          prW = weightSW $ fromJust prSbl


-- construction of the Huffman tree
--   the content of the FrequencyTable is changed
--   into a bunch of Huffman tree
--   The two trees with the lowest weight are merged
--   until only one final tree remain
buildTree :: FrequencyTable -> HuffmanTree
buildTree = reduceHTs . sort . map Leaf . Set.toList

-- expects a list of HT sorted by weight
reduceHTs :: [HuffmanTree] -> HuffmanTree
reduceHTs [] = undefined
reduceHTs [ht1] = ht1
reduceHTs (ht1:ht2:hts) = reduceHTs $ insert (mergeHTs ht1 ht2) hts


-- construction of the CodeBook
--   the depth of each Symbol in the HuffmanTree
--   is the length of its code
--   the canonical Huffman code is then attributed
--   to all the Symbol starting with the one with
--   the longest code
decrement :: HuffmanCode -> HuffmanCode
decrement [] = []
decrement (lsb:hb) = if lsb
                     then False:hb
                     else True:(decrement hb)

depthLeaves :: HuffmanTree -> [(CodeLength, Symbol, Weight)]
depthLeaves = dHelper 0
    where dHelper d (Leaf (SWmapping (s, w))) = [(d, s, w)]
          dHelper d (Node _ ht1 ht2) = depths1 ++ depths2
              where depths1 = dHelper (d+1) ht1
                    depths2 = dHelper (d+1) ht2

canonicalBook :: [(CodeLength, Symbol, Weight)] -> CodeBook
canonicalBook = foldl' fldFun [] . sortOn Down
    where fldFun [] (d,s,w) = [(s,w,d,replicate d True)]
          fldFun xs@((_,_,dx,cx):xt) (d,s,w) = (s,w,d,c):xs
              where c = decrement $ drop (dx-d) cx

codeFromTree :: HuffmanTree -> CodeBook
codeFromTree = canonicalBook . depthLeaves
