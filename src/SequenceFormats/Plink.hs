{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.Plink (readBimStdIn,
                              readBimFile,
                              writeBim,
                              readFamFile,
                              readPlinkBedFile,
                              readPlink,
                              writePlink,
                              PlinkFamEntry(..),
                              plinkFam2EigenstratInd,
                              eigenstratInd2PlinkFam,
                              PlinkPopNameMode(..)) where

import           SequenceFormats.Eigenstrat       (EigenstratIndEntry (..),
                                                   EigenstratSnpEntry (..),
                                                   GenoEntry (..), GenoLine,
                                                   Sex (..))
import           SequenceFormats.Utils            (Chrom (..), consumeProducer,
                                                   readFileProdCheckCompress,
                                                   word)

import           Control.Applicative              ((<|>))
import           Control.Monad                    (forM_, void)
import           Control.Monad.Catch              (MonadThrow, throwM)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.State.Strict (runStateT)
import qualified Data.Attoparsec.ByteString       as AB
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Bits                        (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString                  as BB
import qualified Data.ByteString.Char8            as B
import           Data.List                        (intercalate)
import           Data.Vector                      (fromList, toList)
import           Data.Word                        (Word8)
import           Pipes                            (Consumer, Producer, (>->))
import           Pipes.Attoparsec                 (ParsingError (..), parse)
import qualified Pipes.ByteString                 as PB
import qualified Pipes.Prelude                    as P
import           Pipes.Safe                       (MonadSafe)
import qualified Pipes.Safe.Prelude               as PS
import           System.IO                        (Handle, IOMode (..),
                                                   hPutStrLn, withFile)

-- see https://www.cog-genomics.org/plink/2.0/formats#fam
data PlinkFamEntry = PlinkFamEntry {
    _famFamilyID     :: String,
    _famIndividualID :: String,
    _famFatherID     :: String,
    _famMotherID     :: String,
    _famSexCode      :: Sex,
    _famPhenotype    :: String
} deriving (Eq, Show)

data PlinkPopNameMode = PlinkPopNameAsFamily | PlinkPopNameAsPhenotype | PlinkPopNameAsBoth deriving (Eq, Show)

bimParser :: A.Parser EigenstratSnpEntry
bimParser = do
    chrom      <- word
    snpId_     <- A.skipMany1 A.space >> word
    geneticPos <- A.skipMany1 A.space >> A.double
    pos        <- A.skipMany1 A.space >> A.decimal
    ref        <- A.skipMany1 A.space >> A.satisfy (A.inClass "ACTGNX01234")
    alt        <- A.skipMany1 A.space >> A.satisfy (A.inClass "ACTGNX01234")
    void A.endOfLine
    let refConvert = convertNum ref
        altConvert = convertNum alt
    return $ EigenstratSnpEntry (Chrom chrom) pos geneticPos snpId_ refConvert altConvert
  where
    convertNum '0' = 'N'
    convertNum '1' = 'A'
    convertNum '2' = 'C'
    convertNum '3' = 'G'
    convertNum '4' = 'T'
    convertNum x   = x

famParser :: A.Parser PlinkFamEntry
famParser = do
    A.skipMany A.space
    famID    <- B.unpack <$> word
    indID    <- B.unpack <$> (A.skipMany1 A.space >> word)
    fatherID <- B.unpack <$> (A.skipMany1 A.space >> word)
    motherID <- B.unpack <$> (A.skipMany1 A.space >> word)
    sex      <- A.skipMany1 A.space >> parseSex
    phen     <- B.unpack <$> (A.skipMany1 A.space >> word)
    void A.endOfLine
    return $ PlinkFamEntry famID indID fatherID motherID sex phen
  where
    parseSex = parseMale <|> parseFemale <|> parseUnknown
    parseMale = A.char '1' >> return Male
    parseFemale = A.char '2' >> return Female
    parseUnknown = A.anyChar >> return Unknown

plinkFam2EigenstratInd :: PlinkPopNameMode -> PlinkFamEntry -> EigenstratIndEntry
plinkFam2EigenstratInd plinkPopNameMode (PlinkFamEntry famId indId _ _ sex phen) =
    let popName = case plinkPopNameMode of
            PlinkPopNameAsFamily    -> famId
            PlinkPopNameAsPhenotype -> phen
            -- If the two differ but you want both, then merge them somehow.
            PlinkPopNameAsBoth -> if famId == phen then famId else famId ++ ":" ++ phen
    in  EigenstratIndEntry indId sex popName

eigenstratInd2PlinkFam :: PlinkPopNameMode -> EigenstratIndEntry -> PlinkFamEntry
eigenstratInd2PlinkFam plinkPopNameMode (EigenstratIndEntry indId sex popName)=
    case plinkPopNameMode of
        PlinkPopNameAsFamily    -> PlinkFamEntry popName indId "0" "0" sex "0"
        PlinkPopNameAsPhenotype -> PlinkFamEntry "DummyFamily" indId "0" "0" sex popName
        PlinkPopNameAsBoth      -> PlinkFamEntry popName indId "0" "0" sex popName

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
readPlinkBedFile file nrInds = readPlinkBedProd nrInds . readFileProdCheckCompress $ file

-- |Function to read a Bim File from StdIn. Returns a Pipes-Producer over the EigenstratSnpEntries.
readBimStdIn :: (MonadThrow m, MonadIO m) => Producer EigenstratSnpEntry m ()
readBimStdIn = consumeProducer bimParser PB.stdin

-- |Function to read a Bim File from a file. Returns a Pipes-Producer over the EigenstratSnpEntries.
readBimFile :: (MonadSafe m) => FilePath -> Producer EigenstratSnpEntry m ()
readBimFile = consumeProducer bimParser . readFileProdCheckCompress

-- |Function to read a Plink fam file. Returns the Eigenstrat Individual Entries as list.
readFamFile :: (MonadIO m) => FilePath -> m [PlinkFamEntry]
readFamFile fn =
    liftIO . withFile fn ReadMode $ \handle ->
        P.toListM $ consumeProducer famParser (PB.fromHandle handle)

-- |Function to read a full Plink dataset from files. Returns a pair of the Plink Individual Entries, and a joint Producer over the snp entries and the genotypes.
readPlink :: (MonadSafe m) => FilePath -- ^The Bed file
               -> FilePath -- ^The Bim File
               -> FilePath -- ^The Fam file
               -> m ([PlinkFamEntry], Producer (EigenstratSnpEntry, GenoLine) m ()) -- The return pair of individual entries and a joint Snp/Geno Producer.
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
            let bimLine = B.intercalate "\t" [unChrom chrom, gid, B.pack (show gpos),
                    B.pack (show pos), B.singleton ref, B.singleton alt]
            in  bimLine <> "\n")
    in  toTextPipe >-> snpOutTextConsumer

-- |Function to write a Plink Fam file.
writeFam :: (MonadIO m) => FilePath -> [PlinkFamEntry] -> m ()
writeFam f indEntries =
    liftIO . withFile f WriteMode $ \h ->
        forM_ indEntries $ \(PlinkFamEntry famId indId fatherId motherId sex phen) ->
            hPutStrLn h . intercalate "\t" $ [famId, indId, fatherId, motherId, sexToStr sex, phen]
  where
    sexToStr sex = case sex of
        Male    -> "1"
        Female  -> "2"
        Unknown -> "0"

-- |Function to write an Eigentrat Geno File. Returns a consumer expecting Eigenstrat Genolines.
writeBed :: (MonadIO m) => Handle -- ^The Bed file handle
                -> Consumer GenoLine m () -- ^A consumer to read Genotype entries.
writeBed bedFileH = do
    liftIO $ BB.hPut bedFileH (BB.pack [0b01101100, 0b00011011, 0b00000001])
    let bedOutConsumer = PB.toHandle bedFileH
        toPlinkPipe = P.map (BB.pack . genoLineToBytes)
    toPlinkPipe >-> bedOutConsumer
  where
    genoLineToBytes :: GenoLine -> [Word8]
    genoLineToBytes genoLine = go (toList genoLine)
      where
        go :: [GenoEntry] -> [Word8]
        go []                         = [] -- empty list for recursion stop
        go (g1 : g2 : g3 : g4 : rest) = constructByte [g1, g2, g3, g4] : go rest -- at least 5 entries -> more than 1 byte
        go genoEntries                = [constructByte genoEntries] -- four or less entries -> 1 byte
        constructByte :: [GenoEntry] -> Word8
        constructByte []     = error "constructByte - should never happen"
        constructByte [g]    = genoEntryToByte g
        constructByte (g:gs) = shiftL (constructByte gs) 2 .|. genoEntryToByte g

genoEntryToByte :: GenoEntry -> Word8
genoEntryToByte HomRef  = 0b00000000
genoEntryToByte HomAlt  = 0b00000011
genoEntryToByte Het     = 0b00000010
genoEntryToByte Missing = 0b00000001

-- |Function to write a Plink Database. Returns a consumer expecting joint Snp- and Genotype lines.
writePlink :: (MonadSafe m) => FilePath -- ^The Bed file
                -> FilePath -- ^The Bim File
                -> FilePath -- ^The Fam file
                -> [PlinkFamEntry] -- ^The list of individual entries
                -> Consumer (EigenstratSnpEntry, GenoLine) m () -- ^A consumer to read joint Snp/Genotype entries.
writePlink bedFile bimFile famFile indEntries = do
    liftIO $ writeFam famFile indEntries
    let bimOutConsumer = PS.withFile bimFile WriteMode writeBim
        bedOutConsumer = PS.withFile bedFile WriteMode writeBed
    P.tee (P.map fst >-> bimOutConsumer) >-> P.map snd >-> bedOutConsumer
