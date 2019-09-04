{-# LANGUAGE OverloadedStrings #-}

{-| Module to parse and write freqSum files. The freqsum format is defined here:
<https://rarecoal-docs.readthedocs.io/en/latest/rarecoal-tools.html#vcf2freqsum>
-}

module SequenceFormats.FreqSum (readFreqSumStdIn, readFreqSumFile, FreqSumEntry(..),  
    FreqSumHeader(..), printFreqSumStdOut, printFreqSumFile, freqSumEntryToText) where

import SequenceFormats.Utils (consumeProducer, Chrom(..))

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (runStateT)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Char (isAlphaNum, isSpace)
import qualified Data.ByteString.Char8 as B
import Pipes (Producer, (>->), Consumer)
import Pipes.Attoparsec (parse, ParsingError(..))
import qualified Pipes.Prelude as P
import Pipes.Safe (MonadSafe)
import Pipes.Safe.Prelude (withFile)
import qualified Pipes.ByteString as PB
import Prelude hiding (putStr)
import System.IO (IOMode(..))

-- |A Datatype representing the Header
data FreqSumHeader = FreqSumHeader {
    fshNames :: [B.ByteString], -- ^A list of individual or group names
    fshCounts :: [Int] -- ^A list of haplotype counts per individual/group.
} deriving (Eq, Show)

freqSumHeaderToText :: FreqSumHeader -> B.ByteString
freqSumHeaderToText (FreqSumHeader names nCounts) =
    "#CHROM\tPOS\tREF\tALT\t" <> B.intercalate "\t" tuples <> "\n"
  where
    tuples = zipWith (\n c -> n <> "(" <> B.pack (show c) <> ")") names nCounts

-- |A Datatype to denote a single freqSum line
data FreqSumEntry = FreqSumEntry {
    fsChrom  :: Chrom, -- ^The chromosome of the site
    fsPos    :: Int, -- ^The position of the site
    fsSnpId  :: Maybe String, -- ^An optional parameter to take the snpId. This is not parsed from or printed to freqSum format but is used in internal conversions from Eigenstrat.
    fsRef    :: Char, -- ^The reference allele
    fsAlt    :: Char, -- ^The alternative allele
    fsCounts :: [Maybe Int] -- ^A list of allele counts in each group. Nothing denotes missing data.
} deriving (Eq, Show)

-- |This function converts a single freqSum entry to a printable freqSum line.
freqSumEntryToText :: FreqSumEntry -> B.ByteString
freqSumEntryToText (FreqSumEntry chrom pos _ ref alt maybeCounts) =
    B.intercalate "\t" [B.pack (unChrom chrom), B.pack (show pos), B.singleton ref, B.singleton alt, countStr] <> "\n"
  where
    countStr = B.intercalate "\t" . map (B.pack . show . convertToNum) $ maybeCounts 
    convertToNum Nothing = -1
    convertToNum (Just a) = a

readFreqSumProd :: (MonadThrow m) =>
    Producer B.ByteString m () -> m (FreqSumHeader, Producer FreqSumEntry m ())
readFreqSumProd prod = do
    (res, rest) <- runStateT (parse parseFreqSumHeader) prod
    header <- case res of
        Nothing -> throwM $ ParsingError [] "freqSum file exhausted"
        Just (Left e) -> throwM e
        Just (Right h) -> return h
    return (header, consumeProducer parseFreqSumEntry rest)

-- |A function to read a freqsum file from StdIn. Returns a pair of a freqSum Header and a Producer over all lines.
readFreqSumStdIn :: (MonadIO m, MonadThrow m) => m (FreqSumHeader, Producer FreqSumEntry m ())
readFreqSumStdIn = readFreqSumProd PB.stdin

-- |A function to read a freqsum file from a file. Returns a pair of a freqSum Header and a Producer over all lines.
readFreqSumFile :: (MonadSafe m) => FilePath -> m (FreqSumHeader, Producer FreqSumEntry m ())
readFreqSumFile file = readFreqSumProd $ withFile file ReadMode PB.fromHandle

parseFreqSumHeader :: A.Parser FreqSumHeader
parseFreqSumHeader = do
    tuples <- A.string "#CHROM\tPOS\tREF\tALT\t" >> A.sepBy' tuple A.space <* A.endOfLine
    let names = map fst tuples
        counts = map snd tuples
    return $ FreqSumHeader names counts
  where
    tuple = (,) <$> A.takeWhile (\c -> isAlphaNum c || c == '_' || c == '-') <* A.char '(' <*> A.decimal <* A.char ')'

parseFreqSumEntry :: A.Parser FreqSumEntry
parseFreqSumEntry = FreqSumEntry <$> (Chrom . B.unpack <$> A.takeTill isSpace) <* A.skipSpace <*> A.decimal <*
    A.skipSpace <*> pure Nothing <*> base <* A.skipSpace <*> baseOrDot <* A.skipSpace <*> counts <* A.endOfLine
  where
    counts = (parseMissing <|> parseCount) `A.sepBy` A.char '\t'
    parseMissing = A.string "-1" *> pure Nothing
    parseCount = Just <$> A.decimal
    base = A.satisfy (A.inClass "ACTGN")
    baseOrDot = A.satisfy (A.inClass "ACTG.")

-- |A function to write freqSum data to StdOut. Expects the freqSum header as argument, and then returns a Consumer that accepts freqSum entries.
printFreqSumStdOut :: (MonadIO m) => FreqSumHeader -> Consumer FreqSumEntry m ()
printFreqSumStdOut fsh = do
    liftIO . B.putStr . freqSumHeaderToText $ fsh
    P.map freqSumEntryToText >-> PB.stdout

-- |A function that writes a freqSum file. Expects the FilePath and the freqSum header as arguments, and then returns a Consumer that accepts freqSum entries.
printFreqSumFile :: (MonadSafe m) => FilePath -> FreqSumHeader -> Consumer FreqSumEntry m ()
printFreqSumFile outFile fsh = withFile outFile WriteMode go
  where
    go h = do
        liftIO . B.hPutStr h . freqSumHeaderToText $ fsh
        P.map freqSumEntryToText >-> PB.toHandle h
