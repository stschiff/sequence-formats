{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.PlinkSpec (spec) where

import SequenceFormats.Eigenstrat (EigenstratSnpEntry(..))
import SequenceFormats.Plink (readBimFile)
import SequenceFormats.Utils (Chrom(..))

import Control.Foldl (purely, list)
import qualified Pipes.Prelude as P
import Pipes.Safe (runSafeT)
import Test.Hspec

spec :: Spec
spec = do
    testReadBimFile

mockDatEigenstratSnp :: [EigenstratSnpEntry]
mockDatEigenstratSnp = [
    EigenstratSnpEntry (Chrom "11") 0      0.000000 "rs0000" 'A' 'C',
    EigenstratSnpEntry (Chrom "11") 100000 0.001000 "rs1111" 'A' 'G',
    EigenstratSnpEntry (Chrom "11") 200000 0.002000 "rs2222" 'A' 'T',
    EigenstratSnpEntry (Chrom "11") 300000 0.003000 "rs3333" 'C' 'A',
    EigenstratSnpEntry (Chrom "11") 400000 0.004000 "rs4444" 'G' 'A',
    EigenstratSnpEntry (Chrom "11") 500000 0.005000 "rs5555" 'T' 'A',
    EigenstratSnpEntry (Chrom "11") 600000 0.006000 "rs6666" 'G' 'T']

testReadBimFile :: Spec
testReadBimFile = describe "readBimFile" $
    it "should read a BIM file correctly" $ do
        let esSnpProd = readBimFile "testDat/example.bim"
        (runSafeT $ purely P.fold list esSnpProd) `shouldReturn` mockDatEigenstratSnp
