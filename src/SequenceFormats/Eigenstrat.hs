{-# LANGUAGE OverloadedStrings #-}

module SequenceFormats.Eigenstrat (EigenstratSnpEntry(..), EigenstratIndEntry(..), 
    readEigenstratInd, GenoEntry(..), GenoLine, Sex(..), 
    readEigenstratSnpStdIn, readEigenstratSnpFile, readEigenstrat, writeEigenstrat) where

import SequenceFormats.Utils (consumeProducer, FormatException(..))

import Control.Applicative ((<|>))
import Control.Monad (void, forM_)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import Data.Vector (Vector, fromList, toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pipes (Producer, Pipe, (>->), for, cat, yield, Consumer)
import Pipes.Safe (MonadSafe)
import qualified Pipes.Prelude as P
import qualified Pipes.Text.IO as PT
import System.IO (withFile, IOMode(..))
import Turtle (format, w, d, (%), s)

data EigenstratSnpEntry = EigenstratSnpEntry T.Text Int Char Char deriving (Eq, Show)
    -- Chrom Pos Ref Alt
data EigenstratIndEntry = EigenstratIndEntry T.Text Sex T.Text deriving (Show)
data Sex = Male | Female | Unknown deriving (Show)

data GenoEntry = HomRef | Het | HomAlt | Missing deriving (Show)
type GenoLine = Vector GenoEntry

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
    ref <- A.satisfy (A.inClass "ACTGN")
    A.skipMany1 A.space
    alt <- A.satisfy (A.inClass "ACTG")
    void A.endOfLine
    return $ EigenstratSnpEntry chrom pos ref alt

word :: A.Parser T.Text
word = A.takeTill isSpace

eigenstratIndParser :: A.Parser EigenstratIndEntry
eigenstratIndParser = do
    A.skipMany A.space
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
        P.toListM $ consumeProducer eigenstratIndParser (PT.fromHandle handle)

eigenstratGenoParser :: A.Parser GenoLine
eigenstratGenoParser = do
    line <- A.takeWhile1 isValidNum
    void A.endOfLine
    return . fromList $ do
        l <- T.unpack line
        case l of
            '0' -> return HomAlt
            '1' -> return Het
            '2' -> return HomRef
            '9' -> return Missing
            _ -> error "this should never happen"
  where
    isValidNum c = c == '0' || c == '1' || c == '2' || c == '9'

readEigenstratSnpStdIn :: (MonadThrow m, MonadIO m) => Producer EigenstratSnpEntry m ()
readEigenstratSnpStdIn = consumeProducer eigenstratSnpParser PT.stdin

readEigenstratSnpFile :: (MonadSafe m) => FilePath -> Producer EigenstratSnpEntry m ()
readEigenstratSnpFile = consumeProducer eigenstratSnpParser . PT.readFile

readEigenstrat :: (MonadSafe m) => FilePath -> FilePath -> FilePath ->
    m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
readEigenstrat genoFile snpFile indFile = do
    indEntries <- readEigenstratInd indFile
    let snpProd = readEigenstratSnpFile snpFile
        genoProd = consumeProducer eigenstratGenoParser (PT.readFile genoFile) >-> 
            validateEigenstratEntries (length indEntries)
    return (indEntries, P.zip snpProd genoProd)

validateEigenstratEntries :: (MonadThrow m) => Int -> Pipe GenoLine GenoLine m ()
validateEigenstratEntries nr = for cat $ \line -> do
    if length line /= nr
    then do
        let msg = format ("inconsistent nr of genotypes ("%d%", but should be "%d%") in \
                \genotype line "%w) (length line) nr line
        throwM $ FormatException msg
    else
        yield line

writeEigenstrat :: (MonadSafe m) => FilePath -> FilePath -> FilePath -> 
    [EigenstratIndEntry] -> Consumer (EigenstratSnpEntry, GenoLine) m ()
writeEigenstrat genoFile snpFile indFile indEntries = do
    liftIO $ writeEigenstratIndFile indFile indEntries
    let snpOutTextConsumer = PT.writeFile snpFile
        genoOutTextConsumer = PT.writeFile genoFile
        toTextPipe = P.map (\(EigenstratSnpEntry chrom pos ref alt, genoLine) ->
            let n = format (s%"_"%d) chrom pos
                snpLine = format (s%"\t"%s%"\t0\t"%d%"\t"%s%"\t"%s%"\n") n chrom pos
                    (T.singleton ref) (T.singleton alt)
                genoLineStr = T.concat . map (format d . toEigenStratNum) . toList $ genoLine
            in  (snpLine, format (s%"\n") genoLineStr))
    toTextPipe >-> P.tee (P.map fst >-> snpOutTextConsumer) >-> P.map snd >-> genoOutTextConsumer
  where
    toEigenStratNum c = case c of
        HomRef -> 2 :: Int
        Het -> 1
        HomAlt -> 0
        Missing -> 9

writeEigenstratIndFile :: (MonadIO m) => FilePath -> [EigenstratIndEntry] -> m ()
writeEigenstratIndFile f indEntries = do
    liftIO . withFile f WriteMode $ \h -> do
        forM_ indEntries $ \(EigenstratIndEntry name sex popName) -> do
            liftIO . T.hPutStrLn h $ format (s%"\t"%s%"\t"%s) name (sexToStr sex) popName
  where
    sexToStr sex = case sex of
        Male -> "M"
        Female -> "F"
        Unknown -> "U"
