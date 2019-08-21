{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.VCF.Test (testReadVCF, testGetGenotypes, testGetDosages, testIsTransversionSnp, testIsBiallelicSnp, testVcfToFreqsumEntry) where

import Control.Foldl (list, purely)
import Control.Monad.IO.Class (liftIO)
import Pipes.Prelude (fold)
import Pipes.Safe (runSafeT)
import SequenceFormats.FreqSum (FreqSumEntry(..))
import SequenceFormats.Utils (Chrom(..))
import SequenceFormats.VCF (readVCFfromFile, getGenotypes, getDosages, isTransversionSnp, vcfToFreqSumEntry, isBiallelicSnp, VCFheader(..), VCFentry(..))
import Test.Tasty.HUnit (Assertion, assertEqual)

testReadVCF :: Assertion
testReadVCF = runSafeT $ do
    (vcfH, vcfProd) <- readVCFfromFile "testDat/example.vcf"
    let vcfHc = vcfHeaderComments vcfH
    liftIO $ assertEqual "headerLine1" "##fileformat=VCFv4.2" (vcfHc !! 0)
    liftIO $ assertEqual "headerLine19" "##bcftools_callCommand=call -c -v" (vcfHc !! 18)
    let n = vcfSampleNames vcfH
    liftIO $ assertEqual "sample names" ["12880A", "12881A", "12883A", "12884A", "12885A"] n
    rows <- purely fold list vcfProd
    liftIO $ assertEqual "vcf1" vcf1 (rows !! 0)
    liftIO $ assertEqual "vcf7" vcf7 (rows !! 6)

vcf1 :: VCFentry
vcf1 = VCFentry (Chrom "1") 10492 Nothing "C" ["T"] 15.0302 Nothing ["DP=28", "PV4=1,1,0.30985,1"]
  ["GT", "PL"] [["0/0", "0,3,37"], ["0/0", "0,6,67"], ["0/1", "51,0,28"], ["0/0", "0,54,255"],
  ["0/0", "0,9,83"]]

vcf7 :: VCFentry
vcf7 = VCFentry (Chrom "2") 30923 (Just "rs12345") "G" [] 110.112 Nothing ["DP=5", "FQ=-28.9619"]
  ["GT", "PL"] [["1/1", "0,0,0"], ["1/1", "0,0,0"], ["1/1", "40,6,0"], ["1/1", "105,9,0"], ["1/1", "0,0,0"]]

testGetGenotypes :: Assertion
testGetGenotypes = do
    assertEqual "getGenotypesRight" (Right ["0/0", "0/0", "0/1", "0/0", "0/0"]) (getGenotypes vcf1)
    assertEqual "getGenotypesLeft" (Left "GT format field not found") (getGenotypes (vcf1 {vcfFormatString=["PL"]}))

testGetDosages :: Assertion
testGetDosages = do
    assertEqual "getDosages1" (Right [Just 0, Just 0, Just 1, Just 0, Just 0]) (getDosages vcf1)
    assertEqual "getDosages2" (Right [Just 0, Just 0, Nothing, Just 0, Just 0])
        (getDosages (vcf1 {vcfGenotypeInfo=[["0/0", "0,3,37"], ["0/0", "0,6,67"], [".", "51,0,28"], ["0/0", "0,54,255"],
        ["0/0", "0,9,83"]]}))

testIsTransversionSnp :: Assertion
testIsTransversionSnp = do
    assertEqual "testTriAllelic" False (isTransversionSnp "A" ["C", "T"])
    assertEqual "testTransition1" False (isTransversionSnp "A" ["G"])
    assertEqual "testTransition2" False (isTransversionSnp "T" ["C"])
    assertEqual "testTransversion1" True (isTransversionSnp "T" ["A"])
    assertEqual "testTransversion2" True (isTransversionSnp "C" ["G"])

testVcfToFreqsumEntry :: Assertion
testVcfToFreqsumEntry = do
    assertEqual "vcfToFreqsumValid" (Right (FreqSumEntry (Chrom "1") 10492 'C' 'T' [Just 0, Just 0, Just 1, Just 0, Just 0])) (vcfToFreqSumEntry vcf1)

testIsBiallelicSnp :: Assertion
testIsBiallelicSnp = do
    assertEqual "testTriAllelic" False (isBiallelicSnp "A" ["C", "T"])
    assertEqual "testBiallelic" True (isBiallelicSnp "A" ["T"])
