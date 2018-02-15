{-# LANGUAGE OverloadedStrings #-}

module SequenceFormats.FreqSum (readFreqSumStdIn, readFreqSumFile, FreqSumEntry(..),  
    FreqSumHeader(..), printFreqSum) where

import SequenceFormats.Utils (consumeProducer)

import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (runStateT)
import qualified Data.Attoparsec.Text as A
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Data.Text (unpack, Text)
import Pipes (Producer, runEffect, (>->))
import Pipes.Attoparsec (parse, ParsingError(..))
import qualified Pipes.Prelude as P
import Pipes.Safe (MonadSafe)
import qualified Pipes.Text.IO as PT

data FreqSumHeader = FreqSumHeader {
    fshNames :: [String],
    fshCounts :: [Int]
} deriving (Eq)

instance Show FreqSumHeader where
    show (FreqSumHeader names nCounts) = "#CHROM\tPOS\tREF\tALT\t" ++ intercalate "\t" tuples
      where
        tuples = zipWith (\n c -> n ++ "(" ++ show c ++ ")") names nCounts

data FreqSumEntry = FreqSumEntry {
    fsChrom  :: Int,
    fsPos    :: Int,
    fsRef    :: Char,
    fsAlt    :: Char,
    fsCounts :: [Int]
}

instance Show FreqSumEntry where
    show (FreqSumEntry chrom pos ref alt counts) =
        intercalate "\t" [show chrom, show pos, [ref], [alt], intercalate "\t" . map show $ counts]

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
    let names = map (unpack . fst) tuples
        counts = map snd tuples
    return $ FreqSumHeader names counts
  where
    tuple = (,) <$> A.takeWhile (\c -> isAlphaNum c || c == '_' || c == '-') <* A.char '(' <*> A.decimal <* A.char ')'

parseFreqSumEntry :: A.Parser FreqSumEntry
parseFreqSumEntry = FreqSumEntry <$> A.decimal <* A.skipSpace <*> A.decimal <* A.skipSpace <*>
                                     A.letter <* A.skipSpace <*> A.letter <* A.skipSpace <*>
                                     counts <* A.endOfLine
  where
    counts = (A.signed A.decimal) `A.sepBy` A.char '\t'

printFreqSum :: MonadIO m => (FreqSumHeader, Producer FreqSumEntry m ()) -> m ()
printFreqSum (fsh, prod) = do
    liftIO . print $ fsh
    runEffect $ prod >-> P.map show >-> P.stdoutLn
