module Huffman.Formatter
( encode
) where

import Huffman.Internal
import Huffman.Statistics
import Data.List(foldl', sortOn)
import Data.Word
import qualified Data.ByteString.Lazy as BL

-- some conversion stuff
toWord8 = fromIntegral :: Int   -> Word8
toInt   = fromIntegral :: Word8 -> Int


-- encode Message according to given CodeBook
encode :: Message -> CodeBook -> Message
encode msg cb = encodeHeader cb


-- create header of encoded file
--   header contains:
--     - magic number (number of bits use to encode size of Codes
--                   + padding between header and encoded Message)
--     - encoded tree with the best method
encodeHeader :: CodeBook -> Message
encodeHeader cb = if sizeAS cb <= sizePS cb
                    then encHdrAllSymbols cb
                    else encHdrPresentSymbols cb

-- header contains the length of code for all symbols, even the non present
encHdrAllSymbols :: CodeBook -> Message
encHdrAllSymbols cb = BL.pack $ magic:0:(compact bitsCode olc)
    where magic = toWord8 (bitsCode-1)  -- padding=0
          bitsCode = nBitsCodeLength cb
          olc = orderedLengthCode cb

-- header contains the set of present symbols and their length
encHdrPresentSymbols :: CodeBook -> Message
encHdrPresentSymbols cb = BL.pack $ magic:nSbl:sbl ++ compact bitsCode lc
    where magic = toWord8 (bitsCode-1) + padding*2^4
          padding = (8 - toWord8 bitsCode*nSbl `rem` 8) `rem` 8
          nSbl = toWord8 $ length cb
          bitsCode = nBitsCodeLength cb
          sbl = map getSymbol cb
          lc = map getCodeLength cb

-- convert list of Int into list of bytes, using
-- given number of bits (<8) per Int
compact :: Int -> [Int] -> [Word8]
compact npack = reverse . final . foldl' go (0, 0, [])
    where final (b,cw8,w8s) = if b==0 then w8s else cw8:w8s
          go (bpos, curWd8, out) n =
            if bpos+npack < 8 -- =8 >> perfect match
            then (bpos + npack,
                  curWd8 + toWord8 (n*2^bpos),
                  out)
            else ((bpos + npack) `rem` 8,
                  toWord8 h,
                  (toWord8 (l*(2^bpos)) + curWd8):out)
                where (h,l) = n `quotRem` (2^(8-bpos))

-- return list of CodeLength for all possible Symbol
-- even the non-present in the message (with a null
-- code length)
orderedLengthCode :: CodeBook -> [CodeLength]
orderedLengthCode = reverse . go 0 [] . sortOn getSymbol
    where go curIdx ordList [] = replicate (256-curIdx) 0 ++ ordList
          go curIdx ordList ((s,_,l,_):cbt) =
            go (toInt s+1) (l:(replicate (toInt s-curIdx) 0 ++ ordList)) cbt
