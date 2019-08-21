{-# LANGUAGE OverloadedStrings #-}

-- |This module contains helper functions for file parsing.

module SequenceFormats.Utils (liftParsingErrors,
                              consumeProducer, readFileProd,
                              SeqFormatException(..),
                              Chrom(..)) where

import Control.Error (readErr)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as A
import Pipes (Producer, next)
import Pipes.Attoparsec (ParsingError(..), parsed)
import qualified Pipes.ByteString as PB
import qualified Pipes.Safe as PS
import qualified Pipes.Safe.Prelude as PS
import System.IO (IOMode(..))

-- |A wrapper datatype for Chromosome names.
newtype Chrom = Chrom {unChrom :: String} deriving (Eq)

-- |Show instance for Chrom
instance Show Chrom where
    show (Chrom c) = show c

-- |Ord instance for Chrom
instance Ord Chrom where
    compare (Chrom c1) (Chrom c2) = 
        let c1' = if take 3 c1 == "chr" then drop 3 c1 else c1
            c2' = if take 3 c2 == "chr" then drop 3 c2 else c2
        in  case (,) <$> readChrom c1' <*> readChrom c2' of
                Left e -> error e
                Right (cn1, cn2) -> cn1 `compare` cn2

readChrom :: String -> Either String Int
readChrom c = readErr ("cannot parse chromosome " ++ c) $ c

-- |An exception type for parsing BioInformatic file formats.
data SeqFormatException = SeqFormatException String
    deriving Show

instance Exception SeqFormatException

-- |A function to help with reporting parsing errors to stderr. Returns a clean Producer over the 
-- parsed datatype.
liftParsingErrors :: (MonadThrow m) =>
    Either (ParsingError, Producer B.ByteString m r) () -> Producer a m ()
liftParsingErrors res = case res of
    Left (ParsingError _ msg, restProd) -> do
        x <- lift $ next restProd
        case x of
            Right (chunk, _) -> do
                let msg' = "Error while parsing: " <> msg <> ". Error occurred when trying to parse this chunk: " ++ show chunk
                throwM $ SeqFormatException msg'
            Left _ -> error "should not happen"
    Right () -> return ()

-- |A helper function to parse a text producer, properly reporting all errors to stderr.
consumeProducer :: (MonadThrow m) => A.Parser a -> Producer B.ByteString m () -> Producer a m ()
consumeProducer parser prod = parsed parser prod >>= liftParsingErrors

readFileProd :: (PS.MonadSafe m) => FilePath -> Producer B.ByteString m ()
readFileProd f = PS.withFile f ReadMode PB.fromHandle

