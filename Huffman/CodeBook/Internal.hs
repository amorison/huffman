module Huffman.CodeBook.Internal where

import Data.Function(on)
import Data.List(insert, sort, sortOn, foldl')
import Data.Maybe(fromJust)
import Data.Ord
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

type HuffmanCodeBook s = [(Symbol s, CodeLength, HuffmanCode)]

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

decrement :: HuffmanCode -> HuffmanCode
decrement [] = []
decrement (lsb:hb) = if lsb
                     then False:hb
                     else True:(decrement hb)

insertSymbol :: (Ord s) => FrequencyTable s -> Symbol s -> FrequencyTable s
insertSymbol fTbl sbl = if Just swm == prSbl
                        then Set.insert (SWmapping (sbl, prW+1)) fTbl
                        else Set.insert swm fTbl
    where swm = SWmapping (sbl, 1)
          prSbl = Set.lookupLE (SWmapping (sbl,1)) fTbl
          prW = weightSW $ fromJust prSbl

-- expects a list of HT sorted by weight
reduceHTs :: [HuffmanTree s] -> HuffmanTree s
reduceHTs [] = undefined
reduceHTs [ht1] = ht1
reduceHTs (ht1:ht2:hts) = reduceHTs $ insert (mergeHTs ht1 ht2) hts

buildTree :: FrequencyTable s -> HuffmanTree s
buildTree = reduceHTs . sort . map Leaf . Set.toList

depthLeaves :: HuffmanTree s -> [(CodeLength, Symbol s)]
depthLeaves = dHelper 0
    where dHelper d (Leaf (SWmapping (s, _))) = [(d, s)]
          dHelper d (Node _ ht1 ht2) = depths1 ++ depths2
              where depths1 = dHelper (d+1) ht1
                    depths2 = dHelper (d+1) ht2

canonicalBook :: (Ord s) => [(CodeLength, Symbol s)] -> HuffmanCodeBook s
canonicalBook = foldl' fldFun [] . sortOn Down
    where fldFun [] (d,s) = [(s,d,replicate d True)]
          fldFun xs@((_,dx,cx):xt) (d,s) = (s,d,c):xs
              where c = decrement $ drop (dx-d) cx

codeFromTree :: (Ord s) => HuffmanTree s -> HuffmanCodeBook s
codeFromTree = canonicalBook . depthLeaves
