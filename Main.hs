module Main(main) where

import Huffman.CodeBook(buildCodeBook)
import Huffman.Statistics(entropy)
import qualified Data.ByteString.Lazy as BL

main = do
    message <- BL.getContents
    let (opt, enc, encTot) = entropy $ buildCodeBook message
    putStrLn $ "Information content (bits/symbol):"
    putStrLn $ "Entropy:     " ++ show opt
    putStrLn $ "Encoding:    " ++ show enc
    putStrLn $ "With header: " ++ show encTot
