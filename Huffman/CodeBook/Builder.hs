module Huffman.CodeBook.Builder
( Symbol
, Message
, CodeBook
, huffmanEncode
) where

import Huffman.CodeBook.Internal
import Data.List(foldl')
import Data.Set(empty)

huffmanEncode :: (Ord s) => Message s -> CodeBook s
huffmanEncode = codeFromTree . buildTree . foldl' insertSymbol empty
