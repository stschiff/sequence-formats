{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.FreqSum.Test (fsReadTest, fsWriteTest) where

import Control.Foldl (purely, list)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Managed.Safe (runManaged)
import Control.Monad.Trans.Class (lift)
import Data.Vector (fromList)
import Filesystem.Path.CurrentOS (encodeString)
import Pipes (each, runEffect, (>->))
import qualified Pipes.Prelude as P
import Pipes.Safe (runSafeT)
import SequenceFormats.FreqSum (readFreqSumFile, printFreqSumFile, FreqSumEntry(..), 
    FreqSumHeader(..))
import SequenceFormats.Utils (Chrom(..))
import Turtle (mktempfile)
import Test.Tasty.HUnit (Assertion, assertEqual)

fsReadTest :: Assertion
fsReadTest = do
    let fsFile = "testDat/example.freqsum"
    (fsHeader, fsProd) <- runSafeT $ readFreqSumFile fsFile
    assertEqual "fsReadTest_assertIndEntries" testDatFsHeader fsHeader
    fsEntries <- runSafeT $ purely P.fold list fsProd
    assertEqual "fsReadTest_assertFsEntries" testDatFsEntries fsEntries

fsWriteTest :: Assertion
fsWriteTest = runManaged $ do
    tmpFs <- encodeString <$> mktempfile "testDat" "fsWriteTest"
    let testDatFsProd = each testDatFsEntries
    liftIO . runSafeT . runEffect $
        testDatFsProd >-> printFreqSumFile tmpFs testDatFsHeader
    (fsHeader, fsProd) <- liftIO . runSafeT $ readFreqSumFile tmpFs
    liftIO $ assertEqual "fsWriteTest_assertIndEntries" testDatFsHeader fsHeader
    fsEntries <- liftIO . runSafeT $ purely P.fold list fsProd
    liftIO $ assertEqual "fsWriteTest_assertFsEntries" testDatFsEntries fsEntries

testDatFsHeader :: FreqSumHeader 
testDatFsHeader = FreqSumHeader names numbers
  where
    names = ["SAMPLE0", "SAMPLE1", "SAMPLE2", "SAMPLE3", "SSS"] --"SAMPLE4"]
    numbers = [2, 2, 2, 1, 4]--1]

testDatFsEntries :: [FreqSumEntry]
testDatFsEntries = [
    FreqSumEntry (Chrom "11") 0      'A' 'C' [Just 1, Just 1,  Just 1, Just 1,  Just 0],--1],
    FreqSumEntry (Chrom "11") 100000 'A' 'G' [Just 2, Just 1,  Just 0, Just 0,  Just 0],
    FreqSumEntry (Chrom "11") 200000 'A' 'T' [Just 0, Just 1,  Just 1, Just 1,  Just 1],
    FreqSumEntry (Chrom "11") 300000 'C' 'A' [Just 2, Nothing, Just 1, Just 0,  Just 0],
    FreqSumEntry (Chrom "11") 400000 'G' 'A' [Just 0, Just 1,  Just 1, Just 1,  Just 1],
    FreqSumEntry (Chrom "11") 500000 'T' 'A' [Just 2, Just 2,  Just 1, Nothing, Just 1],
    FreqSumEntry (Chrom "11") 600000 'G' 'T' [Just 0, Just 0,  Just 1, Nothing, Nothing]]

