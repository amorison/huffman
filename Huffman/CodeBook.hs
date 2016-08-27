module Huffman.CodeBook
( buildCodeBook
, orderedLengthCode
, buildArrays
, buildCodeTree
) where

import Huffman.Internal
import Control.Monad.ST
import Data.Array.ST
import Data.Foldable
import Data.List(insert, sort, sortOn, foldl')
import Data.Ord
import qualified Data.ByteString.Lazy as BL
import qualified Data.Array.IArray as IArr
import qualified Data.Tree as T

-- construction of the code book is a three steps process:
--   - construction of the FrequencyTable of the Symbols
--     in the message
--   - construction of the Huffman tree from this table
--   - determination of the canonical code that should
--     be attributed to each Symbol based on this tree
buildCodeBook :: Message -> CodeBook
buildCodeBook = codeFromTree . buildTree . buildFreqTable


-- construction of FrequencyTable
--   each Symbol is read after the other and its frequency
--   is updated
buildFreqTable :: Message -> FrequencyTable
buildFreqTable msg = filter ((/=0).snd) $ zip [0..] $ runST $ incSymbol msg

incSymbol :: Message -> ST s ([Weight])
incSymbol msg = do
    tbl <- newArray (0,255) 0 :: ST s (STUArray s Symbol Weight)
    forM_ (BL.unpack msg) $ \sbl -> do
        n <- readArray tbl sbl
        writeArray tbl sbl (n+1)
    getElems tbl


-- construction of the Huffman tree
--   the content of the FrequencyTable is changed
--   into a bunch of Huffman tree (after removal
--   of absent Symbols)
--   The two trees with the lowest weight are merged
--   until only one final tree remain
buildTree :: FrequencyTable -> HuffmanTree
buildTree = reduceHTs . sort . map Leaf

-- expects a list of HT sorted by weight
reduceHTs :: [HuffmanTree] -> HuffmanTree
reduceHTs [] = undefined
reduceHTs [ht1] = ht1
reduceHTs (ht1:ht2:hts) = reduceHTs $ insert (mergeHTs ht1 ht2) hts


-- construction of the CodeBook
--   the depth of each Symbol in the HuffmanTree
--   is the length of its code
--   the canonical Huffman code is then attributed
--   to all the Symbol starting with the one with
--   the longest code
depthLeaves :: HuffmanTree -> [(CodeLength, Symbol, Weight)]
depthLeaves = go 0
    where go d (Leaf (s, w)) = [(d, s, w)]
          go d (Node _ ht1 ht2) = depths1 ++ depths2
              where depths1 = go (d+1) ht1
                    depths2 = go (d+1) ht2

-- construct codes in ascending order based on
-- descending list of code lengths and symbols
canonicalCodes :: [(CodeLength, Symbol)] -> [(Symbol, CodeLength, HuffmanCode)]
canonicalCodes = foldl' go []
    where go [] (l,s) = [(s, l, 2^l - 1)]
          go xs@((_,lx,cx):xt) (l,s) = (s,l,c):xs
              where c = cx `quot` 2^(max 0 (lx-l)) - 1

canonicalBook :: [(CodeLength, Symbol, Weight)] -> CodeBook
canonicalBook lsw = zipWith (\(s,l,c) w -> (s,w,l,c)) slc ws
    where descList = sortOn Down lsw
          slc = canonicalCodes $ map (\(l,s,_) -> (l,s)) descList
          ws = reverse $ map (\(_,_,w) -> w) descList

codeFromTree :: HuffmanTree -> CodeBook
codeFromTree = canonicalBook . depthLeaves


-- CodeBook manipulations

-- return list of f(CodeBookEntry) for all Symbols (0-255),
-- putting a default value for absent Symbols
orderedListOf :: (CodeBookEntry -> a) -> a -> CodeBook -> [a]
orderedListOf f dflt = reverse . go 0 [] . sortOn getSymbol
    where go curIdx ordList [] = replicate (256-curIdx) dflt ++ ordList
          go curIdx ordList (ce:cbt) =
            go (s+1) ((f ce):(replicate (s-curIdx) dflt ++ ordList)) cbt
              where s = toInt $ getSymbol ce

-- return list of CodeLength for all possible Symbols
orderedLengthCode :: CodeBook -> [CodeLength]
orderedLengthCode = orderedListOf getCodeLength 0

-- return Array containing HuffmanCode indexed by Symbol
--   bits of codes are reversed to have the most significant
--   bit first in the encoded file
buildArrays :: CodeBook -> (CodeArray, LengthArray)
buildArrays cb = (IArr.listArray (0, 255) $ zipWith revBitsCode olc ocd,
                  IArr.listArray (0, 255) olc)
    where ocd = orderedListOf getCode 0 cb
          olc = orderedLengthCode cb

-- return Tree containing Symbols at its Leaves and
-- number of bits necessary to access to the next leaves at nodes
buildCodeTree :: [(CodeLength, Symbol)] -> CodeTree
buildCodeTree = foldl' go leaf . canonicalCodes . sortOn Down . filter ((/=0).fst)
    where leaf = T.Node 0 []
          go _ (s,0,_) = T.Node s []
          go (T.Node 0 []) (s,l,_) = T.Node l $ (T.Node s []):(replicate (2^(toInt l)-1) leaf)
          go (T.Node n ts) (s,l,c) = T.Node n $ replaceAt i (go (ts!!i) (s,l-n,r)) ts
            where (q, r) = c `quotRem` (2^(toItgr l - toItgr n))
                  i = fromIntegral q
