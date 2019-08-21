{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.Eigenstrat.Test (bimReadTest, eigenstratReadTest, eigenstratWriteTest) where

import Control.Foldl (purely, list)
import Control.Monad.IO.Class (liftIO)
import Data.Vector (fromList)
import Pipes (each, runEffect, (>->))
import qualified Pipes.Prelude as P
import Pipes.Safe (runSafeT)
import SequenceFormats.Eigenstrat (readEigenstrat, writeEigenstrat, readBimFile,
    EigenstratSnpEntry(..), EigenstratIndEntry(..), GenoLine, Sex(..), GenoEntry(..))
import SequenceFormats.Utils (Chrom(..))
import System.IO.Temp (withTempFile)
import Test.Tasty.HUnit (Assertion, assertEqual)

bimReadTest :: Assertion
bimReadTest = do
    let esSnpProd = readBimFile "testDat/example.bim"
    esSnpDat <- runSafeT $ purely P.fold list esSnpProd
    assertEqual "" testDatEigenstratSnp esSnpDat 

eigenstratReadTest :: Assertion
eigenstratReadTest = do
    let esSnpFile = "testDat/example.snp"
        esIndFile = "testDat/example.ind"
        esGenoFile = "testDat/example.eigenstratgeno"
    (indEntries, esProd) <- runSafeT $ readEigenstrat esGenoFile esSnpFile esIndFile
    assertEqual "eigenstratReadTest_assertIndEntries" testDatEigenstratInd indEntries
    snpGenoEntries <- runSafeT $ purely P.fold list esProd
    assertEqual "eigenstratReadTest_assertSnpEntries" testDatEigenstratSnp (map fst snpGenoEntries)
    assertEqual "eigenstratReadTest_assertGenoEntries" testDatEigenstratGeno
        (map snd snpGenoEntries)

eigenstratWriteTest :: Assertion
eigenstratWriteTest = do
    withTempFile "testDat" "eigenstratWriteTest" $ (\tmpGeno _ -> 
        withTempFile "testDat" "eigenstratWriteTest" $ (\tmpSnp _ ->
            withTempFile "testDat" "eigenstratWriteTest" $ (\tmpInd _ -> do
                let testDatSnpProd = each testDatEigenstratSnp
                    testDatGenoProd = each testDatEigenstratGeno
                    testDatJointProd = P.zip testDatSnpProd testDatGenoProd
                liftIO . runSafeT . runEffect $
                    testDatJointProd >-> writeEigenstrat tmpGeno tmpSnp tmpInd testDatEigenstratInd
                (indEntries, esProd) <- liftIO . runSafeT $ readEigenstrat tmpGeno tmpSnp tmpInd
                liftIO $ assertEqual "eigenstratWriteTest_assertIndEntries" testDatEigenstratInd indEntries
                snpGenoEntries <- liftIO . runSafeT $ purely P.fold list esProd
                liftIO $ assertEqual "eigenstratWriteTest_assertIndEntries" testDatEigenstratSnp
                    (map fst snpGenoEntries)
                liftIO $ assertEqual "eigenstratWriteTest_assertIndEntries" testDatEigenstratGeno
                    (map snd snpGenoEntries))))

testDatEigenstratSnp :: [EigenstratSnpEntry]
testDatEigenstratSnp = [
    EigenstratSnpEntry (Chrom "11") 0      0.000000 "rs0000" 'A' 'C',
    EigenstratSnpEntry (Chrom "11") 100000 0.001000 "rs1111" 'A' 'G',
    EigenstratSnpEntry (Chrom "11") 200000 0.002000 "rs2222" 'A' 'T',
    EigenstratSnpEntry (Chrom "11") 300000 0.003000 "rs3333" 'C' 'A',
    EigenstratSnpEntry (Chrom "11") 400000 0.004000 "rs4444" 'G' 'A',
    EigenstratSnpEntry (Chrom "11") 500000 0.005000 "rs5555" 'T' 'A',
    EigenstratSnpEntry (Chrom "11") 600000 0.006000 "rs6666" 'G' 'T']


testDatEigenstratInd :: [EigenstratIndEntry]
testDatEigenstratInd = [
    EigenstratIndEntry "SAMPLE0" Female "Case",
    EigenstratIndEntry "SAMPLE1" Male "Case",
    EigenstratIndEntry "SAMPLE2" Female "Control",
    EigenstratIndEntry "SAMPLE3" Male "Control",
    EigenstratIndEntry "SAMPLE4" Female "Control"]

testDatEigenstratGeno :: [GenoLine]
testDatEigenstratGeno = [
    fromList [Het, Het, Het, HomAlt, HomAlt],
    fromList [HomAlt, Het, HomRef, Het, HomRef],
    fromList [HomRef, Het, Het, HomAlt, Het],
    fromList [HomAlt, Missing, Het, HomRef, HomRef],
    fromList [HomRef, Het, Het, HomAlt, HomAlt],
    fromList [HomAlt, HomAlt, Het, Missing, Het],
    fromList [HomRef, HomRef, Het, Missing, Missing]]

