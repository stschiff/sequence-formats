{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.FastaSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import qualified Pipes.Prelude as P
import SequenceFormats.Fasta (loadFastaChrom)
import SequenceFormats.Utils (Chrom(..))
import System.IO (withFile, IOMode(..))
import Test.Hspec

spec :: Spec
spec = do
    testLoadFastaChrom

testLoadFastaChrom :: Spec
testLoadFastaChrom = around (withFile "testDat/example.fasta" ReadMode) $
    describe "loadFastaChrom" $
        it "should read the correct fasta string from file" $ \h -> do
            fastaProd <- loadFastaChrom h (Chrom "chr3")
            fastaString <- BS.unpack . BS.concat <$> P.toListM fastaProd
            fastaString `shouldBe` testString
  where
    testString = "ACGACGACGACGGGGTTTAAAAAGGGTTTCCTCTCTCTCTGGG"