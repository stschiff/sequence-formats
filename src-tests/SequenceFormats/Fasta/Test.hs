{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.Fasta.Test (testFastaRead) where

import qualified Data.ByteString.Char8 as BS
import qualified Pipes.Prelude as P
import SequenceFormats.Fasta (loadFastaChrom)
import SequenceFormats.Utils (Chrom(..))
import System.IO (withFile, IOMode(..))
import Test.Tasty.HUnit (Assertion, assertEqual)

testFastaRead :: Assertion
testFastaRead = withFile "testDat/example.fasta" ReadMode $ \h -> do
    fastaProd <- loadFastaChrom h (Chrom "chr3")
    fastaString <- BS.unpack . BS.concat <$> P.toListM fastaProd
    assertEqual "fastaReadTest_assertFastaEntries" testString fastaString
  where
    testString = "ACGACGACGACGGGGTTTAAAAAGGGTTTCCTCTCTCTCTGGG"