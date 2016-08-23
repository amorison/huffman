module Main(main) where

import Huffman.CodeBook.Builder(huffmanEncode)

main = interact $ unlines . map show . huffmanEncode
