{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.UtilsSpec (spec) where

import SequenceFormats.Utils (Chrom(..))

import Test.Hspec

spec :: Spec
spec = testChrom

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
