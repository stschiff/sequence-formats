{-# LANGUAGE OverloadedStrings #-}

{-| A module to help with parsing VCF files. The VCF format is defined here:
<https://en.wikipedia.org/wiki/Variant_Call_Format>
-}

module SequenceFormats.VCF (VCFheader(..),
                     VCFentry(..),
                     readVCFfromStdIn,
                     readVCFfromFile,
                     getGenotypes,
                     getDosages,
                     isTransversionSnp,
                     vcfToFreqSumEntry,
                     isBiallelicSnp) where

import SequenceFormats.Utils (consumeProducer, Chrom(..))
import SequenceFormats.FreqSum (FreqSumEntry(..))

import Control.Applicative ((<|>), empty)
import Control.Error (headErr, assertErr)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.State.Strict (runStateT)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import qualified Data.Text as T
import Pipes (Producer)
import Pipes.Attoparsec (parse, ParsingError(..))
import Pipes.Safe (MonadSafe)
import qualified Pipes.Text.IO as PT

type Text = T.Text

-- |A datatype to represent the VCF Header. Most comments are simply parsed as entire lines, but the very last comment line, containing the sample names, is separated out
data VCFheader = VCFheader {
    vcfHeaderComments :: [Text], -- ^A list of containing all comments starting with a single '#'
    vcfSampleNames :: [Text] -- ^The list of sample names parsed from the last comment line 
                             -- starting with '##'
} deriving (Show)

-- |A Datatype representing a single VCF entry.
data VCFentry = VCFentry {
    vcfChrom :: Chrom, -- ^The chromosome
    vcfPos :: Int, -- ^The position
    vcfId :: Maybe Text, -- ^The SNP ID if non-missing
    vcfRef :: Text, -- ^ The reference allele (supports also multi-character alleles for Indels)
    vcfAlt :: [Text], -- ^The alternative alleles, each one possible of multiple characters 
    vcfQual :: Double, -- ^The quality value
    vcfFilter :: Maybe Text, -- ^The Filter value, if non-missing.
    vcfInfo :: [Text], -- ^A list of Info fields
    vcfFormatString :: [Text], -- ^A list of format tags
    vcfGenotypeInfo :: [[Text]] -- ^A list of format fields for each sapmle.
} deriving (Show)

readVCFfromProd :: (MonadThrow m) =>
    Producer Text m () -> m (VCFheader, Producer VCFentry m ())
readVCFfromProd prod = do
    (res, rest) <- runStateT (parse vcfHeaderParser) prod
    header <- case res of
        Nothing -> throwM $ ParsingError [] "freqSum file exhausted"
        Just (Left e) -> throwM e
        Just (Right h) -> return h
    return (header, consumeProducer vcfEntryParser rest)

-- |Reading a VCF from StdIn. Returns a VCFHeader and a Producer over VCFentries.
readVCFfromStdIn :: (MonadIO m, MonadThrow m) => m (VCFheader, Producer VCFentry m ())
readVCFfromStdIn = readVCFfromProd PT.stdin

-- |Reading a VCF from a file. Returns a VCFHeader and a Producer over VCFentries.
readVCFfromFile :: (MonadSafe m) => FilePath -> m (VCFheader, Producer VCFentry m ())
readVCFfromFile = readVCFfromProd . PT.readFile

vcfHeaderParser :: A.Parser VCFheader
vcfHeaderParser = VCFheader <$> A.many1' doubleCommentLine <*> singleCommentLine
  where
    doubleCommentLine = do
        c1 <- A.string "##"
        s_ <- A.takeWhile1 (not . A.isEndOfLine)
        A.endOfLine
        return $ T.append c1 s_
    singleCommentLine = do
        void $ A.char '#'
        s_ <- A.takeWhile1 (not . A.isEndOfLine)
        A.endOfLine
        let fields = T.splitOn "\t" s_
        return . drop 9 $ fields

vcfEntryParser :: A.Parser VCFentry
vcfEntryParser = VCFentry <$> (Chrom <$> word) <* sp <*> A.decimal <* sp <*> parseId <* sp <*>
    word <* sp <*> parseAlternativeAlleles <* sp <*> A.double <* sp <*> parseFilter <* sp <*> 
    parseInfoFields <* sp <*> parseFormatStrings <* sp <*> parseGenotypeInfos <* A.endOfLine
  where
    word = A.takeTill A.isHorizontalSpace
    sp = A.satisfy A.isHorizontalSpace
    parseId = parseDot <|> (Just <$> word)
    parseDot = A.char '.' *> empty
    parseAlternativeAlleles = parseDot <|> (parseAllele `A.sepBy1` A.char ',')
    parseAllele = A.takeTill (\c -> c == ',' || A.isHorizontalSpace c)
    parseFilter = parseDot <|> (Just <$> word)
    parseInfoFields = parseDot <|> (parseInfoField `A.sepBy1` A.char ';')
    parseInfoField = A.takeTill (\c -> c == ';' || A.isHorizontalSpace c)
    parseFormatStrings = A.takeTill (\c -> c == ':' || A.isHorizontalSpace c) `A.sepBy1` A.char ':'
    parseGenotypeInfos = parseGenotype `A.sepBy1` (A.satisfy A.isHorizontalSpace)
    parseGenotype = parseGenoField `A.sepBy1` (A.char ':')
    parseGenoField = A.takeTill (\c -> c == ':' || isSpace c) 

-- |returns True if the SNP is biallelic.
isBiallelicSnp :: Text -> [Text] -> Bool
isBiallelicSnp ref alt = validRef && validAlt
  where
    validRef = (ref `elem` ["A", "C", "G", "T"])
    validAlt = case alt of
        [alt'] -> alt' `elem` ["A", "C", "G", "T"]
        _ -> False

-- |returns True if the SNp is a biallelic Transversion SNP (i.e. one of G/T, G/C, A/T, A/C)
isTransversionSnp :: Text -> [Text] -> Bool
isTransversionSnp ref alt =
    case alt of
        [alt'] -> isBiallelicSnp ref alt && (not $ isTransition ref alt')
        _ -> False
  where
    isTransition r a = ((r == "A") && (a == "G")) || ((r == "G") && (a == "A")) ||
                       ((r == "C") && (a == "T")) || ((r == "T") && (a == "C"))

-- |Extracts the genotype fields (for each sapmle) from a VCF entry
getGenotypes :: VCFentry -> Either String [Text]
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
            if '.' `elem` (T.unpack gen) then
                return Nothing
            else
                return . Just $ T.count "1" gen
    return dosages

-- |Converts a VCFentry to the simpler FreqSum format (returns a Left Error if it fails.)
vcfToFreqSumEntry :: VCFentry -> Either String FreqSumEntry
vcfToFreqSumEntry vcfEntry = do
    dosages <- getDosages vcfEntry
    assertErr "multi-site reference allele" $ T.length (vcfRef vcfEntry) == 1
    assertErr "need exactly one alternative allele" $ length (vcfAlt vcfEntry) == 1
    assertErr "multi-site alternative allele" $ T.length (head . vcfAlt $ vcfEntry) == 1
    let ref = T.head (vcfRef vcfEntry)
    let alt = T.head . head . vcfAlt $ vcfEntry
    assertErr "Invalid Reference Allele" $ ref `elem` ['A', 'C', 'T', 'G', 'N']
    assertErr "Invalid Alternative Allele" $ alt `elem` ['A', 'C', 'T', 'G', '.']
    return $ FreqSumEntry (vcfChrom vcfEntry) (vcfPos vcfEntry) ref alt dosages
    
