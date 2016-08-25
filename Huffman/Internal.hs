module Huffman.Internal where

import Data.Function(on)
import Data.Ord(comparing)
import Data.Word
import qualified Data.ByteString.Lazy as BL

type Symbol = Word8
type Message = BL.ByteString
type Weight = Int
type CodeLength = Int
type HuffmanCode = [Bool]

type FrequencyTable = [(Symbol, Weight)]

type CodeBookEntry = (Symbol, Weight, CodeLength, HuffmanCode)
type CodeBook = [CodeBookEntry]

getSymbol :: CodeBookEntry -> Symbol
getSymbol (s, _, _, _) = s

getSymbolWeight :: CodeBookEntry -> Weight
getSymbolWeight (_, w, _, _) = w

getCodeLength :: CodeBookEntry -> CodeLength
getCodeLength (_, _, l, _) = l

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
