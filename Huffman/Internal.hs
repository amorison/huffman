module Huffman.Internal where

import Data.Function(on)
import Data.Ord(comparing)
import Data.Word
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL

type Symbol = Word8
type Message = BL.ByteString
type Weight = Int
type CodeLength = Int
type HuffmanCode = [Bool]

newtype SWmapping = SWmapping (Symbol, Weight)
instance Eq SWmapping where
    (==) = (==) `on` symbolSW
instance Ord SWmapping where
    compare = comparing symbolSW

symbolSW :: SWmapping -> Symbol
symbolSW (SWmapping (s, _)) = s

weightSW :: SWmapping -> Weight
weightSW (SWmapping (_, w)) = w

type FrequencyTable = Set.Set SWmapping

type CodeBook = [(Symbol, Weight, CodeLength, HuffmanCode)]

data HuffmanTree = Leaf SWmapping | Node Weight
                                         HuffmanTree
                                         HuffmanTree
instance Eq HuffmanTree where
    (==) = (==) `on` weightHT
instance Ord HuffmanTree where
    compare = comparing weightHT

weightHT :: HuffmanTree -> Weight
weightHT (Leaf sw) = weightSW sw
weightHT (Node w _ _) = w

mergeHTs :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergeHTs ht1 ht2 = Node (w1+w2) ht1 ht2
    where w1 = weightHT ht1
          w2 = weightHT ht2
