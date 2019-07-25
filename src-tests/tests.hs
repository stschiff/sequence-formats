{-# LANGUAGE OverloadedStrings #-}

import SequenceFormats.Eigenstrat.Test (eigenstratReadTest, bimReadTest, eigenstratWriteTest)
import SequenceFormats.FreqSum.Test (fsReadTest, fsWriteTest)
import SequenceFormats.Fasta.Test (testFastaRead)
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = defaultMain allTests
  where
    allTests = testGroup "Tests" [eigenstratTests, freqSumTests, fastaTests]

eigenstratTests :: TestTree
eigenstratTests = testGroup "Eigenstrat Tests" [
    testCase "Eigenstrat Read Test" eigenstratReadTest,
    testCase "Bim Read Test" bimReadTest,
    testCase "Eigenstrat Write Test" eigenstratWriteTest]

freqSumTests :: TestTree
freqSumTests = testGroup "FreqSum Tests" [
    testCase "FreqSum Read Test" fsReadTest,
    testCase "FreqSum Write Test" fsWriteTest]

fastaTests :: TestTree
fastaTests = testGroup "Fasta Tests" [testCase "Fasta Read Test" testFastaRead]