module Main(main) where

import Huffman.CodeBookBuilder

main = interact $ unlines . map show . huffmanEncode
