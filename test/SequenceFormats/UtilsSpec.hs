{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.UtilsSpec (spec) where

import           SequenceFormats.Utils (Chrom (..), SeqFormatException (..),
                                        decompressMultiMember)

import           Control.Exception     (evaluate)
import qualified Data.ByteString.Char8 as B
import           Pipes                 (each, yield)
import           Pipes.GZip            (compress, defaultCompression)
import qualified Pipes.Prelude         as P
import           Test.Hspec

spec :: Spec
spec = do
    testChrom
    testDecompressMultiMember

testChrom :: Spec
testChrom = describe "Chrom" $ do
    specify "2 should be smaller than 10" $
        Chrom "2" < Chrom "10" `shouldBe` True
    specify "chr2 should be smaller than 10" $
        Chrom "chr2" < Chrom "10" `shouldBe` True
    specify "2 should be smaller than chr10" $
        Chrom "2" < Chrom "chr10" `shouldBe` True
    specify "chr2 should be smaller than chr10" $
        Chrom "chr2" < Chrom "chr10" `shouldBe` True
    specify "chr22 should be smaller than chrX" $
        Chrom "chr22" < Chrom "chrX" `shouldBe` True
    specify "chrX should be smaller than chrY" $
        Chrom "chrX" < Chrom "chrY" `shouldBe` True
    specify "chrY should be smaller than chrMT" $
        Chrom "chrY" < Chrom "chrMT" `shouldBe` True
    specify "22 should be smaller than chrMT" $
        Chrom "22" < Chrom "chrMT" `shouldBe` True
    specify "X should be smaller than chrMT" $
        Chrom "X" < Chrom "chrMT" `shouldBe` True
    specify "chrSSS should throw" $
        evaluate (Chrom "chrSSS" < Chrom "chrMT") `shouldThrow` (==SeqFormatException "cannot parse chromosome SSS")



testDecompressMultiMember :: Spec
testDecompressMultiMember = describe "decompressMultiMember" $ do
    let gz bs = P.fold (<>) B.empty id (compress defaultCompression (yield bs))
        expected = "first line\nsecond line\nthird line\n"
    members <- runIO $ mapM gz ["first line\n", "second line\n", "third line\n"]
    it "decompresses all members if chunks align with member boundaries" $ do
        out <- P.fold (<>) B.empty id (decompressMultiMember (each members))
        out `shouldBe` expected
    it "decompresses all members if chunks do not align with member boundaries" $ do
        let (a, b) = B.splitAt 25 (B.concat members)
        out <- P.fold (<>) B.empty id (decompressMultiMember (each [a, b]))
        out `shouldBe` expected
