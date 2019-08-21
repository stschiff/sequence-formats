{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.RareAlleleHistogram.Test (testReadHistogram, testWriteHistogram) where

import SequenceFormats.RareAlleleHistogram (RareAlleleHistogram(..), readHistogram, writeHistogramFile)

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Test.Tasty.HUnit (Assertion, assertEqual)

testReadHistogram :: Assertion
testReadHistogram = do
    hist <- readHistogram "testDat/example.histogram.txt"
    assertEqual "readHistogramTest" hist testHistogramDat

testWriteHistogram :: Assertion
testWriteHistogram = do
    let fn = "/tmp/histogramWriteTest.txt"
    writeHistogramFile fn testHistogramDat
    hist <- liftIO  $ readHistogram fn
    liftIO $ assertEqual "writeHistogramTest" hist testHistogramDat

testHistogramDat :: RareAlleleHistogram
testHistogramDat = RareAlleleHistogram names nVec 1 10 [] [] 1146826657 counts jnEstimates
  where
    names = ["EUR", "SEA", "SIB", "CHK", "SAM"]
    nVec = [66, 42, 44, 8, 28]
    counts = Map.fromList [
        ([1,0,0,0,0], 773148),
        ([0,1,0,0,0], 527207),
        ([0,0,1,0,0], 368640),
        ([0,0,0,0,1], 213918),
        ([2,0,0,0,0], 158795),
        ([0,2,0,0,0], 73241)]
    jnEstimates = Just $ Map.fromList [
        ([1,0,0,0,0], (6.7e-4, 3.6e-6)),
        ([0,1,0,0,0], (4.5e-4, 2.5e-6)),
        ([0,0,1,0,0], (3.2e-4, 2.4e-6)),
        ([0,0,0,0,1], (1.8e-4, 3.6e-6)),
        ([2,0,0,0,0], (1.3e-4, 1.4e-6)),
        ([0,2,0,0,0], (6.3e-5, 7.3e-7))]
