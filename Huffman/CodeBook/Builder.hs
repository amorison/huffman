module Huffman.CodeBook.Builder
( Symbol
, Message
, FrequencyTable
, HuffmanCodeBook
, buildFreqTable
, buildCodeBook
, huffmanEncode
) where

import Huffman.CodeBook.Internal
import Data.List(foldl')
import Data.Set(empty)

buildFreqTable :: (Ord s) => Message s -> FrequencyTable s
buildFreqTable = foldl' insertSymbol empty

buildCodeBook :: (Ord s) => FrequencyTable s -> HuffmanCodeBook s
buildCodeBook = codeFromTree . buildTree

huffmanEncode :: (Ord s) => Message s -> HuffmanCodeBook s
huffmanEncode = buildCodeBook . buildFreqTable
