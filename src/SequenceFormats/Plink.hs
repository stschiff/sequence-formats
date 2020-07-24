{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.Plink (readBimStdIn, readBimFile, writeBim, readFamFile, readPlinkBedFile) where

import           SequenceFormats.Eigenstrat       (EigenstratIndEntry (..),
                                                   EigenstratSnpEntry (..),
                                                   GenoEntry (..), GenoLine,
                                                   Sex (..))
import           SequenceFormats.Utils            (Chrom (..), consumeProducer,
                                                   readFileProd, word)

import           Control.Applicative              ((<|>))
import           Control.Monad                    (void)
import           Control.Monad.Catch              (MonadThrow, throwM)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.State.Strict (runStateT)
import qualified Data.Attoparsec.ByteString       as AB
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Bits                        (shiftR, (.&.))
import qualified Data.ByteString                  as BB
import qualified Data.ByteString.Char8            as B
import           Data.Vector                      (fromList)
import           Pipes                            (Consumer, Producer, (>->))
import           Pipes.Attoparsec                 (ParsingError (..), parse)
import qualified Pipes.ByteString                 as PB
import qualified Pipes.Prelude                    as P
import           Pipes.Safe                       (MonadSafe, runSafeT)
import qualified Pipes.Safe.Prelude
import           System.IO                        (Handle, IOMode (..),
                                                   withFile)


bimParser :: A.Parser EigenstratSnpEntry
bimParser = do
    chrom      <- word
    snpId_     <- A.skipMany1 A.space >> word
    geneticPos <- A.skipMany1 A.space >> A.double
    pos        <- A.skipMany1 A.space >> A.decimal
    ref        <- A.skipMany1 A.space >> A.satisfy (A.inClass "ACTGN")
    alt        <- A.skipMany1 A.space >> A.satisfy (A.inClass "ACTGX")
    void A.endOfLine
    return $ EigenstratSnpEntry (Chrom chrom) pos geneticPos snpId_ ref alt

famParser :: A.Parser EigenstratIndEntry
famParser = do
    A.skipMany A.space
    pop <- word
    ind <- A.skipMany1 A.space >> word
    _   <- A.skipMany1 A.space >> A.decimal
    _   <- A.skipMany1 A.space >> A.decimal
    sex <- A.skipMany1 A.space >> parseSex
    _   <- A.skipMany1 A.space >> word
    void A.endOfLine
    return $ EigenstratIndEntry (B.unpack ind) sex (B.unpack pop)
  where
    parseSex = parseMale <|> parseFemale <|> parseUnknown
    parseMale = A.char '1' >> return Male
    parseFemale = A.char '2' >> return Female
    parseUnknown = A.anyChar >> return Unknown

bedHeaderParser :: AB.Parser ()
bedHeaderParser = do
    void $ AB.word8 0b01101100 -- magic number I for BED files
    void $ AB.word8 0b00011011 -- magic number II for BED files
    void $ AB.word8 0b00000001 -- we can only parse SNP-major order

bedGenotypeParser :: Int -> AB.Parser GenoLine
bedGenotypeParser nrInds = do
    let nrBytes = if nrInds `rem` 4 == 0 then nrInds `quot` 4 else (nrInds `quot` 4) + 1
    bytes <- BB.unpack <$> AB.take nrBytes
    let indBitPairs = concatMap getBitPairs bytes
    return . fromList . take nrInds . map bitPairToGenotype $ indBitPairs
  where
    getBitPairs byte = map (0b00000011 .&.) [byte, shiftR byte 2, shiftR byte 4, shiftR byte 6]
    bitPairToGenotype 0b00000000 = HomRef
    bitPairToGenotype 0b00000010 = Het
    bitPairToGenotype 0b00000011 = HomAlt
    bitPairToGenotype 0b00000001 = Missing
    bitPairToGenotype _          = error "This should never happen"

readPlinkBedProd :: (MonadThrow m) => Int -> Producer B.ByteString m () -> m (Producer GenoLine m ())
readPlinkBedProd nrInds prod = do
    (res, rest) <- runStateT (parse bedHeaderParser) prod
    _ <- case res of
        Nothing -> throwM $ ParsingError [] "Bed file exhausted prematurely"
        Just (Left e) -> throwM e
        Just (Right h) -> return h
    return $ consumeProducer (bedGenotypeParser nrInds) rest

-- |A function to read a bed file from a file. Returns a Producer over all lines.
readPlinkBedFile :: (MonadSafe m) => FilePath -> Int -> m (Producer GenoLine m ())
readPlinkBedFile file nrInds = readPlinkBedProd nrInds (Pipes.Safe.Prelude.withFile file ReadMode PB.fromHandle)

-- |Function to read a Bim File from StdIn. Returns a Pipes-Producer over the EigenstratSnpEntries.
readBimStdIn :: (MonadThrow m, MonadIO m) => Producer EigenstratSnpEntry m ()
readBimStdIn = consumeProducer bimParser PB.stdin

-- |Function to read a Bim File from a file. Returns a Pipes-Producer over the EigenstratSnpEntries.
readBimFile :: (MonadSafe m) => FilePath -> Producer EigenstratSnpEntry m ()
readBimFile = consumeProducer bimParser . readFileProd

-- |Function to read a Plink fam file. Returns the Eigenstrat Individual Entries as list.
readFamFile :: (MonadIO m) => FilePath -> m [EigenstratIndEntry]
readFamFile fn =
    liftIO . withFile fn ReadMode $ \handle ->
        P.toListM $ consumeProducer famParser (PB.fromHandle handle)

-- |Function to read a full Plink dataset from files. Returns a pair of the Plink Individual Entries, and a joint Producer over the snp entries and the genotypes.
readPlink :: (MonadSafe m) => FilePath -- ^The Bed file
               -> FilePath -- ^The Bim File
               -> FilePath -- ^The Fam file
               -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ()) -- The return pair of individual entries and a joint Snp/Geno Producer.
readPlink bedFile bimFile famFile = do
    indEntries <- readFamFile famFile
    let nrInds = length indEntries
        snpProd = readBimFile bimFile
    genoProd <- readPlinkBedFile bedFile nrInds
    return (indEntries, P.zip snpProd genoProd)

-- |Function to write a Bim file. Returns a consumer expecting EigenstratSnpEntries.
writeBim :: (MonadIO m) => Handle -- ^The Eigenstrat Snp File handle.
    -> Consumer EigenstratSnpEntry m () -- ^A consumer to read EigenstratSnpEntries
writeBim snpFileH =
    let snpOutTextConsumer = PB.toHandle snpFileH
        toTextPipe = P.map (\(EigenstratSnpEntry chrom pos gpos gid ref alt) ->
            B.intercalate "\t" [unChrom chrom, gid, B.pack (show gpos), B.pack (show pos), B.singleton ref, B.singleton alt])
    in  toTextPipe >-> snpOutTextConsumer
