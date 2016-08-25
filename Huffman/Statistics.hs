module Huffman.Statistics
( entropy
, nBitsCodeLength
, totalEncodedLength
) where

import Huffman.Internal

lg2 :: (Floating a) => a -> a
lg2 = logBase 2

-- compute (optimal bits/symbol, obtained bits/symbol)
--   the obtained b/s is computed without taking the cost of
--   encoding the tree into account
entropy :: (Floating a) => CodeBook -> (a, a)
entropy cb = (entropyShannon cb, lTot/wTot)
    where lTot = fromIntegral $ totalEncodedLength cb
          wTot = fromIntegral $ totalMsgLength cb

-- compute Shannon entropy
entropyShannon :: (Floating a) => CodeBook -> a
entropyShannon cb = sum $ map ((\p -> (-p)*lg2 p) . getP) cb
    where getP = (/wTot) . fromIntegral . getSymbolWeight
          wTot = fromIntegral $ totalMsgLength cb

-- necessary number of bits to encode CodeLength
nBitsCodeLength :: CodeBook -> Int
nBitsCodeLength = ceiling . lg2 . fromIntegral . maximum . map getCodeLength

-- total length in bytes of initial message
totalMsgLength :: CodeBook -> Weight
totalMsgLength = sum . map getSymbolWeight

-- total length in bits of encoded message
totalEncodedLength :: CodeBook -> CodeLength
totalEncodedLength = foldr go 0
    where go (_, w, l, _) lTot = lTot + w*l
