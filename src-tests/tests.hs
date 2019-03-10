{-# LANGUAGE OverloadedStrings #-}

import Control.Foldl (purely, list)
import Pipes.Prelude (fold)
import Pipes.Safe (runSafeT)
import SequenceFormats.Eigenstrat (readEigenstratSnpFile, readBimFile, EigenstratSnpEntry(..))
import SequenceFormats.Utils (Chrom(..))

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (Assertion, testCase, assertEqual)

main :: IO ()
main = defaultMain eigenstratTests

eigenstratTests :: TestTree
eigenstratTests = testGroup "Eigenstrat Tests" [
    testCase "EigenstratSnp Read Test" eigenstratSnpReadTest,
    testCase "Bim Read Test" bimReadTest]

testEigenstratSnpDat :: [EigenstratSnpEntry]
testEigenstratSnpDat = [
    EigenstratSnpEntry (Chrom "11") 0      0.000000 "rs0000" 'A' 'C',
    EigenstratSnpEntry (Chrom "11") 100000 0.001000 "rs1111" 'A' 'G',
    EigenstratSnpEntry (Chrom "11") 200000 0.002000 "rs2222" 'A' 'T',
    EigenstratSnpEntry (Chrom "11") 300000 0.003000 "rs3333" 'C' 'A',
    EigenstratSnpEntry (Chrom "11") 400000 0.004000 "rs4444" 'G' 'A',
    EigenstratSnpEntry (Chrom "11") 500000 0.005000 "rs5555" 'T' 'A',
    EigenstratSnpEntry (Chrom "11") 600000 0.006000 "rs6666" 'G' 'T']

eigenstratSnpReadTest :: Assertion
eigenstratSnpReadTest = do
    let esSnpProd = readEigenstratSnpFile "testDat/example.snp"
    esSnpDat <- runSafeT $ purely fold list esSnpProd
    assertEqual "" testEigenstratSnpDat esSnpDat 

bimReadTest :: Assertion
bimReadTest = do
    let esSnpProd = readBimFile "testDat/example.bim"
    esSnpDat <- runSafeT $ purely fold list esSnpProd
    assertEqual "" testEigenstratSnpDat esSnpDat 

writeEigenstratSnpTest :: Assertion
writeEigenstratSnpTest = do
    let esProd = each testEigenstratSnpDat

