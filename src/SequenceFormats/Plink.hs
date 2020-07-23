{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.Plink (readBimStdIn, readBimFile, writeBim) where

import           SequenceFormats.Eigenstrat       (EigenstratSnpEntry (..))
import           SequenceFormats.Utils            (Chrom (..), consumeProducer,
                                                   readFileProd, word)

import           Control.Monad                    (void)
import           Control.Monad.Catch              (MonadThrow)
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B
import           Pipes                            (Consumer, Producer, (>->))
import qualified Pipes.ByteString                 as PB
import qualified Pipes.Prelude                    as P
import           Pipes.Safe                       (MonadSafe)
import           System.IO                        (Handle)


bimParser :: A.Parser EigenstratSnpEntry
bimParser = do
    chrom <- word
    snpId_ <- A.skipMany1 A.space >> word
    geneticPos <- A.skipMany1 A.space >> A.double
    pos <- A.skipMany1 A.space >> A.decimal
    ref <- A.skipMany1 A.space >> A.satisfy (A.inClass "ACTGN")
    alt <- A.skipMany1 A.space >> A.satisfy (A.inClass "ACTGX")
    void A.endOfLine
    return $ EigenstratSnpEntry (Chrom chrom) pos geneticPos snpId_ ref alt

-- |Function to read a Bim File from StdIn. Returns a Pipes-Producer over the EigenstratSnpEntries.
readBimStdIn :: (MonadThrow m, MonadIO m) => Producer EigenstratSnpEntry m ()
readBimStdIn = consumeProducer bimParser PB.stdin

-- |Function to read a Bim File from a file. Returns a Pipes-Producer over the EigenstratSnpEntries.
readBimFile :: (MonadSafe m) => FilePath -> Producer EigenstratSnpEntry m ()
readBimFile = consumeProducer bimParser . readFileProd

-- |Function to write a Bim file. Returns a consumer expecting EigenstratSnpEntries.
writeBim :: (MonadIO m) => Handle -- ^The Eigenstrat Snp File handle.
    -> Consumer EigenstratSnpEntry m () -- ^A consumer to read EigenstratSnpEntries
writeBim snpFileH =
    let snpOutTextConsumer = PB.toHandle snpFileH
        toTextPipe = P.map (\(EigenstratSnpEntry chrom pos gpos gid ref alt) ->
            B.intercalate "\t" [unChrom chrom, gid, B.pack (show gpos), B.pack (show pos), B.singleton ref, B.singleton alt])
    in  toTextPipe >-> snpOutTextConsumer
