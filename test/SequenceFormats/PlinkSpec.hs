{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.PlinkSpec (spec) where

import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine, Sex (..))
import           SequenceFormats.Plink      (readBimFile, readFamFile,
                                             readPlink, readPlinkBedFile,
                                             writePlink)
import           SequenceFormats.Utils      (Chrom (..))

import           Control.Foldl              (list, purely)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Vector                (fromList)
import           Pipes                      (each, runEffect, (>->))
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (runSafeT)
import           Test.Hspec

spec :: Spec
spec = do
    testReadBimFile
    testReadFamFile
    testReadBedFile
    testReadPlink
    testWritePlink

mockDatEigenstratSnp :: [EigenstratSnpEntry]
mockDatEigenstratSnp = [
    EigenstratSnpEntry (Chrom "11") 0      0.000000 "rs0000" 'A' 'C',
    EigenstratSnpEntry (Chrom "11") 100000 0.001000 "rs1111" 'A' 'G',
    EigenstratSnpEntry (Chrom "11") 200000 0.002000 "rs2222" 'A' 'T',
    EigenstratSnpEntry (Chrom "11") 300000 0.003000 "rs3333" 'C' 'A',
    EigenstratSnpEntry (Chrom "11") 400000 0.004000 "rs4444" 'G' 'A',
    EigenstratSnpEntry (Chrom "11") 500000 0.005000 "rs5555" 'T' 'A',
    EigenstratSnpEntry (Chrom "11") 600000 0.006000 "rs6666" 'G' 'T']

mockDatEigenstratInd :: [EigenstratIndEntry]
mockDatEigenstratInd = [
    EigenstratIndEntry "SAMPLE0" Female "1",
    EigenstratIndEntry "SAMPLE1" Male "2",
    EigenstratIndEntry "SAMPLE2" Female "3",
    EigenstratIndEntry "SAMPLE3" Male "4",
    EigenstratIndEntry "SAMPLE4" Female "5"]

mockDatPlinkBed :: [GenoLine]
mockDatPlinkBed = [
    fromList [Het,    Het,    Het,    HomAlt, HomAlt],
    fromList [HomAlt, Het,    HomRef, Het,    HomRef],
    fromList [HomRef, Het,    Het,    HomAlt, Het],
    fromList [HomAlt, HomAlt, Het,    HomRef, HomRef],
    fromList [HomRef, Het,    Het,    HomAlt, HomAlt],
    fromList [HomAlt, HomAlt, Het,    Het,    Het],
    fromList [HomRef, HomRef, Het,    Het,    HomAlt]]

testReadBimFile :: Spec
testReadBimFile = describe "readBimFile" $
    it "should read a BIM file correctly" $ do
        let esSnpProd = readBimFile "testDat/example.bim"
        (runSafeT $ purely P.fold list esSnpProd) `shouldReturn` mockDatEigenstratSnp

testReadFamFile :: Spec
testReadFamFile = describe "readFamFile" $
    it "should read a FAM file correctly" $ do
        readFamFile "testDat/example.fam" `shouldReturn` mockDatEigenstratInd

testReadBedFile :: Spec
testReadBedFile = describe "readBedFile" $
    it "should read genotypes correctly" $ do
        let fn = "testDat/example.plink.bed"
        bedDat <- runSafeT $ do
            bedProd <- readPlinkBedFile fn 5
            purely P.fold list bedProd
        bedDat `shouldBe` mockDatPlinkBed

testReadPlink :: Spec
testReadPlink = describe "readPlink" $ do
    it "should read the correct Plink files" $ do
        let bimFile = "testDat/example.plink.bim"
            famFile = "testDat/example.plink.fam"
            bedFile = "testDat/example.plink.bed"
        (indEntries, esProd) <- runSafeT $ readPlink bedFile bimFile famFile
        indEntries `shouldBe` mockDatEigenstratInd
        snpGenoEntries <- runSafeT $ purely P.fold list esProd
        (map fst snpGenoEntries) `shouldBe` mockDatEigenstratSnp
        (map snd snpGenoEntries) `shouldBe` mockDatPlinkBed

testWritePlink :: Spec
testWritePlink = describe "writePlink" $ do
    it "should write and read back Plink data correctly" $ do
        let tmpGeno = "/tmp/plinkWriteTest.bed"
            tmpSnp = "/tmp/plinkWriteTest.bim"
            tmpInd = "/tmp/plinkWriteTest.fam"
            testDatSnpProd = each mockDatEigenstratSnp
            testDatGenoProd = each mockDatPlinkBed
            testDatJointProd = P.zip testDatSnpProd testDatGenoProd
        liftIO . runSafeT . runEffect $
            testDatJointProd >-> writePlink tmpGeno tmpSnp tmpInd mockDatEigenstratInd
        (indEntries, esProd) <- liftIO . runSafeT $ readPlink tmpGeno tmpSnp tmpInd
        indEntries `shouldBe` mockDatEigenstratInd
        snpGenoEntries <- liftIO . runSafeT $ purely P.fold list esProd
        (map fst snpGenoEntries) `shouldBe` mockDatEigenstratSnp
        (map snd snpGenoEntries) `shouldBe` mockDatPlinkBed