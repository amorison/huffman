module Huffman.Formatter.Internal where

import Huffman.Internal
import Data.List(foldl', sortOn)
import Data.Word

-- convert list of bytes into list of bytes, but
-- shrinking each byte on given number of bits (<8)
compact :: Word8 -> [Word8] -> [Word8]
compact npack = reverse . final . foldl' go (0, 0, [])
    where final (b,cw8,w8s) = if b==0 then w8s else cw8:w8s
          go (bpos, curWd8, out) n =
            if bpos+npack < 8 -- =8 >> perfect match
            then (bpos + npack,
                  curWd8 + n*2^bpos,
                  out)
            else ((bpos + npack) `rem` 8, h,
                  (l*(2^bpos) + curWd8):out)
                where (h,l) = n `quotRem` (2^(8-bpos))
