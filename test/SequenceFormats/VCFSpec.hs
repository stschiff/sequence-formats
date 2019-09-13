{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.VCFSpec (spec) where

import Control.Foldl (list, purely)
import Pipes.Prelude (fold)
import Pipes.Safe (runSafeT)
import SequenceFormats.FreqSum (FreqSumEntry(..))
import SequenceFormats.Utils (Chrom(..))
import SequenceFormats.VCF (readVCFfromFile, getGenotypes, getDosages,
    isTransversionSnp, vcfToFreqSumEntry, isBiallelicSnp, VCFheader(..), VCFentry(..))
import Test.Hspec

spec :: Spec
spec = do
    testReadVCFfromFile
    testGetGenotypes
    testGetDosages
    testIsTransversionSnp
    testVcfToFreqsumEntry
    testIsBiallelicSnp

testReadVCFfromFile :: Spec
testReadVCFfromFile = describe "readVCFfromFile" $ do
    (vcfH, vcfRows) <- runIO . runSafeT $ do
        (vcfH_, vcfProd_) <- readVCFfromFile "testDat/example.vcf"
        vcfRows_ <- purely fold list vcfProd_
        return (vcfH_, vcfRows_)
    let vcfHc = vcfHeaderComments vcfH
    it "reads the correct header lines" $ do
        vcfHc !! 0 `shouldBe` "##fileformat=VCFv4.2"
        vcfHc !! 18 `shouldBe` "##bcftools_callCommand=call -c -v"
    it "reads the correct sample names" $
        vcfSampleNames vcfH `shouldBe` ["12880A", "12881A", "12883A", "12884A", "12885A"] 
    it "reads the correct vcf genotype rows" $ do
        vcfRows !! 0 `shouldBe` vcf1
        vcfRows !! 6 `shouldBe` vcf7

vcf1 :: VCFentry
vcf1 = VCFentry (Chrom "1") 10492 (Just "testId") "C" ["T"] 15.0302 Nothing ["DP=28", "PV4=1,1,0.30985,1"]
  ["GT", "PL"] [["0/0", "0,3,37"], ["0/0", "0,6,67"], ["0/1", "51,0,28"], ["0/0", "0,54,255"],
  ["0/0", "0,9,83"]]

vcf7 :: VCFentry
vcf7 = VCFentry (Chrom "2") 30923 Nothing "G" [] 110.112 Nothing ["DP=5", "FQ=-28.9619"]
  ["GT", "PL"] [["1/1", "0,0,0"], ["1/1", "0,0,0"], ["1/1", "40,6,0"], ["1/1", "105,9,0"], ["1/1", "0,0,0"]]

testGetGenotypes :: Spec
testGetGenotypes = describe "getGenotypes" $ do
    it "should successfully read genotypes if GT format field is there" $
        getGenotypes vcf1 `shouldBe` Right ["0/0", "0/0", "0/1", "0/0", "0/0"]
    it "should yield Left err if GT format field isn't found" $
        getGenotypes (vcf1 {vcfFormatString=["PL"]}) `shouldBe` Left "GT format field not found"

testGetDosages :: Spec
testGetDosages = describe "getDosages" $ do
    it "should read correct dosages" $ do
        getDosages vcf1 `shouldBe` Right [Just 0, Just 0, Just 1, Just 0, Just 0]
        let vcf1' = vcf1 {vcfGenotypeInfo=[
                ["0/0", "0,3,37"], ["0/0", "0,6,67"], [".", "51,0,28"], ["0/0", "0,54,255"],
                ["0/0", "0,9,83"]]}
        getDosages vcf1' `shouldBe` Right [Just 0, Just 0, Nothing, Just 0, Just 0]

testIsTransversionSnp :: Spec
testIsTransversionSnp = describe "isTransversionSnp" $ do
    it "should reject triAllelic SNPs" $
        isTransversionSnp "A" ["C", "T"] `shouldBe` False
    it "should reject transitions" $ do
        isTransversionSnp "A" ["G"] `shouldBe` False
        isTransversionSnp "T" ["C"] `shouldBe` False
    it "should accept transversions" $ do
        isTransversionSnp "T" ["A"] `shouldBe` True
        isTransversionSnp "C" ["G"] `shouldBe` True

testVcfToFreqsumEntry :: Spec
testVcfToFreqsumEntry = describe "vcfToFreqsumEntry" $
    it "should convert correctly" $ do
        let r = Right (FreqSumEntry (Chrom "1") 10492 (Just "testId") Nothing 'C' 'T' [Just 0, Just 0, Just 1, Just 0, Just 0])
        vcfToFreqSumEntry vcf1 `shouldBe` r

testIsBiallelicSnp :: Spec
testIsBiallelicSnp = describe "isBiallelicSnp" $ do
    it "should reject triAllelic" $
        isBiallelicSnp "A" ["C", "T"] `shouldBe` False
    it "should accept biallelic" $
        isBiallelicSnp "A" ["T"] `shouldBe` True
