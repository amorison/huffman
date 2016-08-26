module Huffman.Formatter
( encode
) where

import Huffman.Internal
import Huffman.Formatter.Internal
import Huffman.CodeBook
import Huffman.Statistics
import qualified Data.ByteString.Lazy as BL


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
