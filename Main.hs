module Main(main) where

import Huffman.CodeBook.Builder(CodeBook, huffmanEncode)
import Huffman.CodeBook.Statistics(entropy)

printCodeBook :: (Ord s, Show s) => CodeBook s -> String
printCodeBook = unlines . map show

main = do
    message <- getContents
    let codeBook = huffmanEncode message
        (opt, enc) = entropy codeBook
    putStr $ printCodeBook codeBook
    putStrLn $ "\nInformation content (bits/symbol):"
    putStrLn $ "Entropy: " ++ show opt
    putStrLn $ "Encoding: " ++ show enc
