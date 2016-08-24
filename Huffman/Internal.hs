module Huffman.Internal where

import Data.Function(on)
import Data.Ord(comparing)
import Data.Word
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map

type Symbol = Word8
type Message = BL.ByteString
type Weight = Int
type CodeLength = Int
type HuffmanCode = [Bool]

type FrequencyTable = Map.Map Symbol Weight

type CodeBook = [(Symbol, Weight, CodeLength, HuffmanCode)]

data HuffmanTree = Leaf (Symbol, Weight) | Node Weight
                                                HuffmanTree
                                                HuffmanTree
instance Eq HuffmanTree where
    (==) = (==) `on` weightHT
instance Ord HuffmanTree where
    compare = comparing weightHT

weightHT :: HuffmanTree -> Weight
weightHT (Leaf (_, w)) = w
weightHT (Node w _ _) = w

mergeHTs :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergeHTs ht1 ht2 = Node (w1+w2) ht1 ht2
    where w1 = weightHT ht1
          w2 = weightHT ht2
