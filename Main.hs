module Main(main) where

import Huffman.CodeBook(CodeBook, huffmanEncode)
import Huffman.Statistics(entropy)
import qualified Data.ByteString.Lazy as BL

printCodeBook :: CodeBook -> String
printCodeBook = unlines . map show

main = do
    message <- BL.getContents
    let codeBook = huffmanEncode message
        (opt, enc, encTot) = entropy codeBook
    putStr $ printCodeBook codeBook
    putStrLn $ "\nInformation content (bits/symbol):"
    putStrLn $ "Entropy:     " ++ show opt
    putStrLn $ "Encoding:    " ++ show enc
    putStrLn $ "With header: " ++ show encTot
