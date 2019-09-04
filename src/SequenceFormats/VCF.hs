{-# LANGUAGE OverloadedStrings #-}

{-| A module to help with parsing VCF files. The VCF format is defined here:
<https://en.wikipedia.org/wiki/Variant_Call_Format>
-}

module SequenceFormats.VCF (VCFheader(..),
                     VCFentry(..),
                     readVCFfromStdIn,
                     readVCFfromFile,
                     readVCFfromProd,
                     getGenotypes,
                     getDosages,
                     isTransversionSnp,
                     vcfToFreqSumEntry,
                     isBiallelicSnp) where

import SequenceFormats.Utils (consumeProducer, Chrom(..),
    readFileProd, SeqFormatException(..), word)
import SequenceFormats.FreqSum (FreqSumEntry(..))

import Control.Applicative ((<|>))
import Control.Error (headErr, assertErr)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.State.Strict (runStateT)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as B
import Pipes (Producer)
import Pipes.Attoparsec (parse)
import Pipes.Safe (MonadSafe)
import qualified Pipes.ByteString as PB

-- |A datatype to represent the VCF Header. Most comments are simply parsed as entire lines, but the very last comment line, containing the sample names, is separated out
data VCFheader = VCFheader {
    vcfHeaderComments :: [String], -- ^A list of containing all comments starting with a single '#'
    vcfSampleNames :: [String] -- ^The list of sample names parsed from the last comment line 
                             -- starting with '##'
} deriving (Show)

-- |A Datatype representing a single VCF entry.
data VCFentry = VCFentry {
    vcfChrom :: Chrom, -- ^The chromosome
    vcfPos :: Int, -- ^The position
    vcfId :: Maybe B.ByteString, -- ^The SNP ID if non-missing
    vcfRef :: B.ByteString, -- ^ The reference allele (supports also multi-character alleles for Indels)
    vcfAlt :: [B.ByteString], -- ^The alternative alleles, each one possible of multiple characters 
    vcfQual :: Double, -- ^The quality value
    vcfFilter :: Maybe B.ByteString, -- ^The Filter value, if non-missing.
    vcfInfo :: [B.ByteString], -- ^A list of Info fields
    vcfFormatString :: [B.ByteString], -- ^A list of format tags
    vcfGenotypeInfo :: [[B.ByteString]] -- ^A list of format fields for each sample.
} deriving (Show, Eq)

-- |reads a VCFheader and VCFentries from a text producer.
readVCFfromProd :: (MonadThrow m) =>
    Producer B.ByteString m () -> m (VCFheader, Producer VCFentry m ())
readVCFfromProd prod = do
    (res, rest) <- runStateT (parse vcfHeaderParser) prod
    header <- case res of
        Nothing -> throwM $ SeqFormatException "freqSum file exhausted"
        Just (Left e) -> throwM (SeqFormatException (show e))
        Just (Right h) -> return h
    return (header, consumeProducer vcfEntryParser rest)

-- |Reading a VCF from StdIn. Returns a VCFHeader and a Producer over VCFentries.
readVCFfromStdIn :: (MonadIO m, MonadThrow m) => m (VCFheader, Producer VCFentry m ())
readVCFfromStdIn = readVCFfromProd PB.stdin

-- |Reading a VCF from a file. Returns a VCFHeader and a Producer over VCFentries.
readVCFfromFile :: (MonadSafe m) => FilePath -> m (VCFheader, Producer VCFentry m ())
readVCFfromFile = readVCFfromProd . readFileProd

vcfHeaderParser :: A.Parser VCFheader
vcfHeaderParser = VCFheader <$> A.many1' doubleCommentLine <*> singleCommentLine
  where
    doubleCommentLine = do
        c1 <- A.string "##"
        s_ <- A.takeWhile1 (/='\n')
        A.endOfLine
        return . B.unpack $ c1 <> s_
    singleCommentLine = do
        void $ A.char '#'
        s_ <- A.takeWhile1 (/='\n')
        A.endOfLine
        let fields = B.splitWith (=='\t') s_
        return . drop 9 . map B.unpack $ fields

vcfEntryParser :: A.Parser VCFentry
vcfEntryParser = vcfEntryParserFull <|> vcfEntryParserTruncated
  where
    vcfEntryParserFull = VCFentry <$> (Chrom . B.unpack <$> word) <* sp <*> A.decimal <* sp <*> parseId <*
        sp <*> word <* sp <*> parseAlternativeAlleles <* sp <*> A.double <* sp <*> parseFilter <* 
        sp <*> parseInfoFields <* sp <*> parseFormatStrings <* sp <*> parseGenotypeInfos <* 
        A.endOfLine
    vcfEntryParserTruncated = VCFentry <$> (Chrom . B.unpack <$> word) <* sp <*> A.decimal <* sp <*> parseId <*
        sp <*> word <* sp <*> parseAlternativeAlleles <* sp <*> A.double <* sp <*> parseFilter <*
        sp <*> parseInfoFields <*> pure [] <*> pure [] <* A.endOfLine
    sp = A.satisfy (\c -> c == ' ' || c == '\t')
    parseId = (parseDot *> pure Nothing) <|> (Just <$> word)
    parseDot = A.char '.'
    parseAlternativeAlleles = (parseDot *> pure []) <|> (parseAllele `A.sepBy1` A.char ',')
    parseAllele = A.takeTill (\c -> c == ',' || isSpace c)
    parseFilter = (parseDot *> pure Nothing) <|> (Just <$> word)
    parseInfoFields = (parseDot *> pure []) <|> (parseInfoField `A.sepBy1` A.char ';')
    parseInfoField = A.takeTill (\c -> c == ';' || isSpace c)
    parseFormatStrings = parseFormatString `A.sepBy1` A.char ':'
    parseFormatString = A.takeTill (\c -> c == ':' || isSpace c)
    parseGenotypeInfos = parseGenotype `A.sepBy1` sp
    parseGenotype = parseGenoField `A.sepBy1` A.char ':'
    parseGenoField = A.takeTill (\c -> c == ':' || isSpace c) 

-- |returns True if the SNP is biallelic.
isBiallelicSnp :: B.ByteString -> [B.ByteString] -> Bool
isBiallelicSnp ref alt = validRef && validAlt
  where
    validRef = (ref `elem` ["A", "C", "G", "T"])
    validAlt = case alt of
        [alt'] -> alt' `elem` ["A", "C", "G", "T"]
        _ -> False

-- |returns True if the SNp is a biallelic Transversion SNP (i.e. one of G/T, G/C, A/T, A/C)
isTransversionSnp :: B.ByteString -> [B.ByteString] -> Bool
isTransversionSnp ref alt =
    case alt of
        [alt'] -> isBiallelicSnp ref alt && (not $ isTransition ref alt')
        _ -> False
  where
    isTransition r a = ((r == "A") && (a == "G")) || ((r == "G") && (a == "A")) ||
                       ((r == "C") && (a == "T")) || ((r == "T") && (a == "C"))

-- |Extracts the genotype fields (for each sapmle) from a VCF entry
getGenotypes :: VCFentry -> Either String [B.ByteString]
getGenotypes vcfEntry = do
    gtIndex <- fmap fst . headErr "GT format field not found" . filter ((=="GT") . snd) .
               zip [0..] . vcfFormatString $ vcfEntry
    return $ map (!!gtIndex) (vcfGenotypeInfo vcfEntry)

-- |Extracts the dosages (the sum of non-reference alleles) per sample (returns a Left Error if it fails.)
getDosages :: VCFentry -> Either String [Maybe Int]
getDosages vcfEntry = do
    genotypes <- getGenotypes vcfEntry
    let dosages = do
            gen <- genotypes
            if '.' `elem` (B.unpack gen) then
                return Nothing
            else
                return . Just $ B.count '1' gen
    return dosages

-- |Converts a VCFentry to the simpler FreqSum format (returns a Left Error if it fails.)
vcfToFreqSumEntry :: VCFentry -> Either String FreqSumEntry
vcfToFreqSumEntry vcfEntry = do
    dosages <- getDosages vcfEntry
    assertErr "multi-site reference allele" $ B.length (vcfRef vcfEntry) == 1
    assertErr "need exactly one alternative allele" $ length (vcfAlt vcfEntry) == 1
    assertErr "multi-site alternative allele" $ B.length (head . vcfAlt $ vcfEntry) == 1
    let ref = B.head (vcfRef vcfEntry)
    let alt = B.head . head . vcfAlt $ vcfEntry
    assertErr "Invalid Reference Allele" $ ref `elem` ['A', 'C', 'T', 'G', 'N']
    assertErr "Invalid Alternative Allele" $ alt `elem` ['A', 'C', 'T', 'G', '.']
    return $ FreqSumEntry (vcfChrom vcfEntry) (vcfPos vcfEntry) (B.unpack <$> vcfId vcfEntry) ref alt dosages
    
