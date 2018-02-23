{-# LANGUAGE OverloadedStrings #-}

module SequenceFormats.FreqSum (readFreqSumStdIn, readFreqSumFile, FreqSumEntry(..),  
    FreqSumHeader(..), printFreqSumStdOut, printFreqSumFile) where

import SequenceFormats.Utils (consumeProducer)

import Control.Applicative ((<|>))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (runStateT)
import qualified Data.Attoparsec.Text as A
import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text, intercalate, singleton)
import Data.Text.IO (putStr, hPutStr)
import Pipes (Producer, (>->), Consumer)
import Pipes.Attoparsec (parse, ParsingError(..))
import qualified Pipes.Prelude as P
import Pipes.Safe (MonadSafe)
import Pipes.Safe.Prelude (withFile)
import qualified Pipes.Text.IO as PT
import Prelude hiding (putStr)
import System.IO (IOMode(..))
import Turtle (format, s, d, (%))

data FreqSumHeader = FreqSumHeader {
    fshNames :: [Text],
    fshCounts :: [Int]
} deriving (Eq)

freqSumHeaderToText :: FreqSumHeader -> Text
freqSumHeaderToText (FreqSumHeader names nCounts) =
    format ("#CHROM\tPOS\tREF\tALT\t"%s%"\n") (intercalate "\t" tuples)
  where
    tuples = zipWith (\n c -> format (s%"("%d%")") n c) names nCounts

data FreqSumEntry = FreqSumEntry {
    fsChrom  :: Text,
    fsPos    :: Int,
    fsRef    :: Char,
    fsAlt    :: Char,
    fsCounts :: [Maybe Int]
}

freqSumEntryToText :: FreqSumEntry -> Text
freqSumEntryToText (FreqSumEntry chrom pos ref alt maybeCounts) =
    format (s%"\t"%d%"\t"%s%"\t"%s%"\t"%s%"\n") chrom pos (singleton ref) (singleton alt) countStr 
  where
    countStr = intercalate "\t" . map (format d . convertToNum) $ maybeCounts 
    convertToNum Nothing = -1
    convertToNum (Just a) = a

readFreqSumProd :: (MonadThrow m) =>
    Producer Text m () -> m (FreqSumHeader, Producer FreqSumEntry m ())
readFreqSumProd prod = do
    (res, rest) <- runStateT (parse parseFreqSumHeader) prod
    header <- case res of
        Nothing -> throwM $ ParsingError [] "freqSum file exhausted"
        Just (Left e) -> throwM e
        Just (Right h) -> return h
    return (header, consumeProducer parseFreqSumEntry rest)

readFreqSumStdIn :: (MonadIO m, MonadThrow m) => m (FreqSumHeader, Producer FreqSumEntry m ())
readFreqSumStdIn = readFreqSumProd PT.stdin

readFreqSumFile :: (MonadSafe m) => FilePath -> m (FreqSumHeader, Producer FreqSumEntry m ())
readFreqSumFile = readFreqSumProd . PT.readFile

parseFreqSumHeader :: A.Parser FreqSumHeader
parseFreqSumHeader = do
    tuples <- A.string "#CHROM\tPOS\tREF\tALT\t" >> A.sepBy' tuple A.space <* A.endOfLine
    let names = map fst tuples
        counts = map snd tuples
    return $ FreqSumHeader names counts
  where
    tuple = (,) <$> A.takeWhile (\c -> isAlphaNum c || c == '_' || c == '-') <* A.char '(' <*> A.decimal <* A.char ')'

parseFreqSumEntry :: A.Parser FreqSumEntry
parseFreqSumEntry = FreqSumEntry <$> A.takeTill isSpace <* A.skipSpace <*> A.decimal <*
    A.skipSpace <*> A.letter <* A.skipSpace <*> A.letter <* A.skipSpace <*> counts <* A.endOfLine
  where
    counts = (parseMissing <|> parseCount) `A.sepBy` A.char '\t'
    parseMissing = A.string "-1" *> pure Nothing
    parseCount = Just <$> A.decimal

printFreqSumStdOut :: (MonadIO m) => FreqSumHeader -> Consumer FreqSumEntry m ()
printFreqSumStdOut fsh = do
    liftIO . putStr . freqSumHeaderToText $ fsh
    P.map freqSumEntryToText >-> PT.stdout

printFreqSumFile :: (MonadSafe m) => FilePath -> FreqSumHeader -> Consumer FreqSumEntry m ()
printFreqSumFile outFile fsh = do
    outFileH <- withFile outFile WriteMode return
    liftIO . hPutStr outFileH . freqSumHeaderToText $ fsh
    P.map freqSumEntryToText >-> PT.toHandle outFileH
