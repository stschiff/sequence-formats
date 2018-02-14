{-# LANGUAGE OverloadedStrings #-}

module SequenceFormats.Eigenstrat (EigenstratSnpEntry(..), EigenstratIndEntry(..), 
    eigenstratSnpParser, 
    eigenstratGenoParser, eigenstratIndParser, readEigenstratInd, GenoEntry(..), GenoLine,
    streamEigenstratGeno, streamEigenstratSnp) where

import SequenceFormats.Utils (consumeProducer)

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import qualified Data.Text as T
import Pipes (Producer)
import Pipes.Prelude (toListM)
import Pipes.Text.IO (fromHandle)
import System.IO (withFile, IOMode(..), Handle)

data EigenstratSnpEntry = EigenstratSnpEntry T.Text Int Char Char deriving (Show)
    -- Chrom Pos Ref Alt
data EigenstratIndEntry = EigenstratIndEntry T.Text Sex T.Text deriving (Show)
data Sex = Male | Female | Unknown deriving (Show)

data GenoEntry = HomRef | Het | HomAlt | Missing
type GenoLine = [GenoEntry]

eigenstratSnpParser :: A.Parser EigenstratSnpEntry
eigenstratSnpParser = do
    A.skipMany A.space
    void word
    A.skipMany1 A.space
    chrom <- word
    A.skipMany1 A.space
    void word
    A.skipMany1 A.space
    pos <- A.decimal
    A.skipMany1 A.space
    ref <- A.satisfy (A.inClass "ACTGX")
    A.skipMany1 A.space
    alt <- A.satisfy (A.inClass "ACTGX")
    void A.endOfLine
    return $ EigenstratSnpEntry chrom pos ref alt

word :: A.Parser T.Text
word = A.takeTill isSpace

eigenstratIndParser :: A.Parser EigenstratIndEntry
eigenstratIndParser = do
    name <- word
    A.skipMany1 A.space
    sex <- parseSex
    A.skipMany1 A.space
    popName <- word
    void A.endOfLine
    return $ EigenstratIndEntry name sex popName

parseSex :: A.Parser Sex
parseSex = parseMale <|> parseFemale <|> parseUnknown
  where
    parseMale = A.char 'M' >> return Male
    parseFemale = A.char 'F' >> return Female
    parseUnknown = A.char 'U' >> return Unknown

readEigenstratInd :: (MonadIO m) => FilePath -> m [EigenstratIndEntry]
readEigenstratInd fn = do
    liftIO . withFile fn ReadMode $ \handle -> do
        toListM $ consumeProducer eigenstratIndParser (fromHandle handle)

eigenstratGenoParser :: A.Parser GenoLine
eigenstratGenoParser = do
    line <- A.takeWhile1 isValidNum
    void A.endOfLine
    return $ do
        l <- T.unpack line
        case l of
            '0' -> return HomAlt
            '1' -> return Het
            '2' -> return HomRef
            '9' -> return Missing
            _ -> error "this should never happen"
  where
    isValidNum c = c == '0' || c == '1' || c == '2' || c == '9'

streamEigenstratGeno :: (MonadThrow m, MonadIO m) => Handle -> Producer GenoLine m ()
streamEigenstratGeno handle = consumeProducer eigenstratGenoParser (fromHandle handle)

streamEigenstratSnp :: (MonadThrow m, MonadIO m) => Handle -> Producer EigenstratSnpEntry m ()
streamEigenstratSnp handle = consumeProducer eigenstratSnpParser (fromHandle handle)
