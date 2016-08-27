module Huffman.Internal where

import Data.Function(on)
import Data.Ord(comparing)
import Data.Word
import qualified Data.ByteString.Lazy as BL
import qualified Data.Array.IArray as IArr
import qualified Data.Array.Unboxed as AU
import qualified Data.Tree as T

type Symbol = Word8
type Message = BL.ByteString
type Weight = Int
type CodeLength = Word8
type HuffmanCode = Integer

type FrequencyTable = [(Symbol, Weight)]

type CodeBookEntry = (Symbol, Weight, CodeLength, HuffmanCode)
type CodeBook = [CodeBookEntry]

type CodeTree = T.Tree Symbol
type CodeArray = IArr.Array Symbol HuffmanCode
type LengthArray = AU.UArray Symbol CodeLength

getSymbol :: CodeBookEntry -> Symbol
getSymbol (s, _, _, _) = s

getSymbolWeight :: CodeBookEntry -> Weight
getSymbolWeight (_, w, _, _) = w

getCodeLength :: CodeBookEntry -> CodeLength
getCodeLength (_, _, l, _) = l

getCode :: CodeBookEntry -> HuffmanCode
getCode (_, _, _, c) = c

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

-- some conversion stuff
toWord8 = fromIntegral :: Int   -> Word8
toInt   = fromIntegral :: Word8 -> Int
toItgr  = fromIntegral :: Word8 -> Integer

-- replace element in a list
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = xh ++ (x:xt)
    where (xh, (_:xt)) = splitAt i xs

-- reverse bit order of the significant portion
-- of a given HuffmanCode
revBitsCode :: CodeLength -> HuffmanCode -> HuffmanCode
revBitsCode _ 0 = 0
revBitsCode 0 c = c
revBitsCode n c = h + 2*(revBitsCode (n-1) l)
    where (h, l) = c `quotRem` (2^(toItgr n-1))
