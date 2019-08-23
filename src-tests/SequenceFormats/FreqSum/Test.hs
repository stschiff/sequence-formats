{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.FreqSum.Test (fsReadTest, fsWriteTest) where

import SequenceFormats.FreqSum (readFreqSumFile, printFreqSumFile, FreqSumEntry(..), 
    FreqSumHeader(..))
import SequenceFormats.Utils (Chrom(..))

import Control.Foldl (purely, list)
import Control.Monad.IO.Class (liftIO)
import Pipes (each, runEffect, (>->))
import qualified Pipes.Prelude as P
import Pipes.Safe (runSafeT)
import Test.Tasty.HUnit (Assertion, assertEqual)

fsReadTest :: Assertion
fsReadTest = runSafeT $ do
    let fsFile = "testDat/example.freqsum"
    (fsHeader, fsProd) <- readFreqSumFile fsFile
    liftIO $ assertEqual "fsReadTest_assertIndEntries" testDatFsHeader fsHeader
    fsEntries <- purely P.fold list fsProd
    liftIO $ assertEqual "fsReadTest_assertFsEntries" testDatFsEntries fsEntries

fsWriteTest :: Assertion
fsWriteTest = do
    let fn = "/tmp/freqSumWriteTest.txt"
        testDatFsProd = each testDatFsEntries
    runSafeT . runEffect $ testDatFsProd >-> printFreqSumFile fn testDatFsHeader
    runSafeT $ do
        (fsHeader, fsProd) <- readFreqSumFile fn
        liftIO $ assertEqual "fsWriteTest_assertIndEntries" testDatFsHeader fsHeader
        fsEntries <- purely P.fold list fsProd
        liftIO $ assertEqual "fsWriteTest_assertFsEntries" testDatFsEntries fsEntries

testDatFsHeader :: FreqSumHeader 
testDatFsHeader = FreqSumHeader names numbers
  where
    names = ["SAMPLE0", "SAMPLE1", "SAMPLE2", "SAMPLE3", "SAMPLE4"]
    numbers = [2, 2, 2, 1, 1]

testDatFsEntries :: [FreqSumEntry]
testDatFsEntries = [
    FreqSumEntry (Chrom "11") 0      'A' 'C' [Just 1, Just 1,  Just 1, Just 1,  Just 1],
    FreqSumEntry (Chrom "11") 100000 'A' 'G' [Just 2, Just 1,  Just 0, Just 0,  Just 0],
    FreqSumEntry (Chrom "11") 200000 'A' 'T' [Just 0, Just 1,  Just 1, Just 1,  Just 1],
    FreqSumEntry (Chrom "11") 300000 'C' 'A' [Just 2, Nothing, Just 1, Just 0,  Just 0],
    FreqSumEntry (Chrom "11") 400000 'G' 'A' [Just 0, Just 1,  Just 1, Just 1,  Just 1],
    FreqSumEntry (Chrom "11") 500000 'T' 'A' [Just 2, Just 2,  Just 1, Nothing, Just 1],
    FreqSumEntry (Chrom "11") 600000 'G' 'T' [Just 0, Just 0,  Just 1, Nothing, Nothing]]

