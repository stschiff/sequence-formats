module SequenceFormats.Pileup.Test (testReadPileupFile) where

import SequenceFormats.Pileup (readPileupFromFile, PileupRow(..))
import SequenceFormats.Utils (Chrom(..))

import Control.Foldl (purely, list)
import qualified Pipes.Prelude as P
import Pipes.Safe (runSafeT)
import Test.Tasty.HUnit (Assertion, assertEqual)


testReadPileupFile :: Assertion
testReadPileupFile = do
    let pProd = readPileupFromFile "testDat/example.pileup"
    pEntries <- runSafeT $ purely P.fold list pProd
    assertEqual "pReadTest_assertPileupEntries" testDatPentries pEntries

testDatPentries :: [PileupRow]
testDatPentries = [
    PileupRow (Chrom "1") 1000 'A' ["ACCT", "AAA", "ACCAACC"],
    PileupRow (Chrom "1") 2000 'C' ["GGCA", "ACT", "ACCAACC"],
    PileupRow (Chrom "2") 1000 'G' ["AGCT", "AAA", "ACCAACC"]]
