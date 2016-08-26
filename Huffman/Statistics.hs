module Huffman.Statistics
( entropy
, nBitsCodeLength
, totalEncodedLength
, sizeAS, sizePS
) where

import Huffman.Internal

lg2 :: (Floating a) => a -> a
lg2 = logBase 2

-- compute (optimal bits/symbol, bits/symbol msg, bits/symbol msg+hdr)
entropy :: (Floating a) => CodeBook -> (a, a, a)
entropy cb = (entropyShannon cb, lTot/wTot, (lTot+sHdr)/wTot)
    where lTot = fromIntegral $ totalEncodedLength cb
          wTot = fromIntegral $ totalMsgLength cb
          sHdr = fromIntegral $ sizeHdr cb

-- compute Shannon entropy
entropyShannon :: (Floating a) => CodeBook -> a
entropyShannon cb = sum $ map ((\p -> (-p)*lg2 p) . getP) cb
    where getP = (/wTot) . fromIntegral . getSymbolWeight
          wTot = fromIntegral $ totalMsgLength cb

-- necessary number of bits to encode CodeLength
nBitsCodeLength :: CodeBook -> Int
nBitsCodeLength = floor . (+1) . lg2 . fromIntegral .
                  maximum . map getCodeLength

-- total length in bytes of initial message
totalMsgLength :: CodeBook -> Weight
totalMsgLength = sum . map getSymbolWeight

-- total length in bits of encoded message
totalEncodedLength :: CodeBook -> CodeLength
totalEncodedLength = foldr go 0
    where go (_, w, l, _) lTot = lTot + w*l

-- bits required to encode Huffman tree
sizeHdr :: CodeBook -> Int
sizeHdr cb = min (sizeAS cb) (sizePS cb)

-- if all symbols code length are encoded
sizeAS :: CodeBook -> Int
sizeAS = (+16) . (*256) . nBitsCodeLength

-- if present symbols and associated code
-- length are encoded
sizePS :: CodeBook -> Int
sizePS cb = 16 + nSbl * (8 + bitsCode) + padding
    where nSbl = length cb
          bitsCode = nBitsCodeLength cb
          padding = (8 - bitsCode*nSbl `rem` 8) `rem` 8
