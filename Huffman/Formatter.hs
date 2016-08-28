module Huffman.Formatter
( compress
) where

import Huffman.Internal
import Huffman.Formatter.Internal
import Huffman.CodeBook
import Huffman.Statistics
import Data.List(foldl')
import Data.Word
import qualified Data.ByteString.Lazy as BL
import qualified Data.Array.IArray as IArr


-- compress Message according to given CodeBook
compress :: Message -> CodeBook -> Message
compress msg cb = encodeHeader cb `BL.append` encode cb msg

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
    where magic = bitsCode-1  -- padding=0
          bitsCode = toWord8 $ nBitsCodeLength cb
          olc = orderedLengthCode cb

-- header contains the set of present symbols and their length
encHdrPresentSymbols :: CodeBook -> Message
encHdrPresentSymbols cb = BL.pack $ magic:nSbl:sbl ++ compact bitsCode lc
    where magic = bitsCode-1 + padding*2^4
          padding = (8 - bitsCode*nSbl `rem` 8) `rem` 8
          nSbl = toWord8 $ length cb
          bitsCode = toWord8 $ nBitsCodeLength cb
          sbl = map getSymbol cb
          lc = map getCodeLength cb

-- encode Message according to given CodeBook
encode :: CodeBook -> Message -> Message
encode cb = BL.cons padding . BL.pack . (\(_,_,xs) -> xs) .
            foldl' go (padding,0,[]) . BL.unpack
    where padding = toWord8 (8 - totalEncodedLength cb `rem` 8) `rem` 8
          (ca, la) = buildArrays cb
          go :: (CodeLength,Word8,[Word8]) -> Symbol -> (CodeLength,Word8,[Word8])
          go (b,w,ws) ns = if nb+b < 8
                           then (nb+b, w+(fromIntegral nw)*2^b, ws)
                           else (rl, q2, ws ++ (w+(fromIntegral rw)*2^b):spread (8*ql) qw)
            where nw = ca IArr.! ns
                  nb = la IArr.! ns
                  (qw, rw) = nw `quotRem` (2^(8-toItgr b))
                  (ql, rl) = (nb+b-8) `quotRem` 8
                  q2 = fromIntegral $ nw `quot` (2^(nb-rl))
