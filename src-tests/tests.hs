{-# LANGUAGE OverloadedStrings #-}

import SequenceFormats.RareAlleleHistogram.Test (testWriteHistogram, testReadHistogram)
import SequenceFormats.Eigenstrat.Test (eigenstratReadTest, bimReadTest, eigenstratWriteTest)
import SequenceFormats.FreqSum.Test (fsReadTest, fsWriteTest)
import SequenceFormats.Fasta.Test (testFastaRead)
import SequenceFormats.Utils.Test (testChromOrder)
import SequenceFormats.VCF.Test (testReadVCF, testGetGenotypes, testGetDosages, testIsTransversionSnp, testIsBiallelicSnp, testVcfToFreqsumEntry)
import SequenceFormats.Pileup.Test (testReadPileupFile)
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = defaultMain allTests
  where
    allTests = testGroup "Tests" [
        eigenstratTests, freqSumTests, fastaTests, rahTests, chromTests,
        vcfTests, pileupTests]

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

rahTests :: TestTree
rahTests = testGroup "Histogram Tests" [
  testCase "Histogram Read Test" testReadHistogram,
  testCase "Histogram Write Test" testWriteHistogram]

chromTests :: TestTree
chromTests = testGroup "Chrom tests" [
  testCase "Chromosome ordering tests" testChromOrder]

vcfTests :: TestTree
vcfTests = testGroup "VCF tests" [
    testCase "VCF read test" testReadVCF,
    testCase "VCF getGenotypes" testGetGenotypes,
    testCase "VCF getDosages" testGetDosages,
    testCase "VCF isTransitionSnp" testIsTransversionSnp,
    testCase "VCF isBiallelicSnp" testIsBiallelicSnp,
    testCase "VCF testVcfToFreqsumEntry" testVcfToFreqsumEntry]

pileupTests :: TestTree
pileupTests = testGroup "Pileup Tests" [
    testCase "readPileup Test" testReadPileupFile]