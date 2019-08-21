{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.Utils.Test (testChromOrder) where

import SequenceFormats.Utils (Chrom(..))
import Test.Tasty.HUnit (Assertion, assertBool)

testChromOrder :: Assertion
testChromOrder = do
    assertBool "testChromSmaller" (Chrom "2" < Chrom "10")
    assertBool "testChromSmaller2" (Chrom "chr2" < Chrom "10")
    assertBool "testChromSmaller3" (Chrom "2" < Chrom "chr10")
    assertBool "testChromSmaller3" (Chrom "chr2" < Chrom "chr10")
