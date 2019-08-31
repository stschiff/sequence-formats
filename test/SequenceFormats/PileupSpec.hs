module SequenceFormats.PileupSpec (spec) where

import SequenceFormats.Pileup (readPileupFromFile, PileupRow(..))
import SequenceFormats.Utils (Chrom(..))

import Control.Foldl (purely, list)
import qualified Pipes.Prelude as P
import Pipes.Safe (runSafeT)
import Test.Hspec

spec :: Spec
spec = testReadPileupFromFile

testReadPileupFromFile :: Spec
testReadPileupFromFile = describe "readPileupFromFile" $
    it "should read correct data from file" $ do
        let pProd = readPileupFromFile "testDat/example.pileup"
        runSafeT (purely P.fold list pProd) `shouldReturn` mockDatPentries

mockDatPentries :: [PileupRow]
mockDatPentries = [
    PileupRow (Chrom "1") 1000 'A' ["ACCT", "AAA", "ACCAACC"],
    PileupRow (Chrom "1") 2000 'C' ["GGCA", "ACT", "ACCAACC"],
    PileupRow (Chrom "2") 1000 'G' ["AGCT", "AAA", "ACCAACC"]]
