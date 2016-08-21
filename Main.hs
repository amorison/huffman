module Main(main) where

import Huffman.CodeBookBuilder
import Data.List(sortOn)

main = interact $ unlines . map show . sortOn fst . huffmanEncode
