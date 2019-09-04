{-# LANGUAGE OverloadedStrings #-}

-- |This module contains helper functions for file parsing.

module SequenceFormats.Utils (liftParsingErrors,
                              consumeProducer, readFileProd,
                              SeqFormatException(..),
                              Chrom(..), word) where

import Control.Error (readErr)
import Control.Exception (Exception, throw)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Class (lift)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Pipes (Producer, next)
import Pipes.Attoparsec (ParsingError(..), parsed)
import qualified Pipes.ByteString as PB
import qualified Pipes.Safe as PS
import qualified Pipes.Safe.Prelude as PS
import System.IO (IOMode(..))

-- |An exception type for parsing BioInformatic file formats.
data SeqFormatException = SeqFormatException String
    deriving (Show, Eq)

instance Exception SeqFormatException

-- |A wrapper datatype for Chromosome names.
newtype Chrom = Chrom {unChrom :: String} deriving (Eq)

-- |Show instance for Chrom
instance Show Chrom where
    show (Chrom c) = show c

-- |Ord instance for Chrom
instance Ord Chrom where
    compare (Chrom c1) (Chrom c2) = 
        let [c1NoChr, c2NoChr] = map removeChr [c1, c2]
            [c1XYMTconvert, c2XYMTconvert] = map convertXYMT [c1NoChr, c2NoChr]
        in  case (,) <$> readChrom c1XYMTconvert <*> readChrom c2XYMTconvert of
                Left e -> throw $ SeqFormatException e
                Right (cn1, cn2) -> cn1 `compare` cn2
      where
        removeChr c = if take 3 c == "chr" then drop 3 c else c
        convertXYMT c = case c of
            "X"  -> "23"
            "Y"  -> "24"
            "MT" -> "90"
            n    -> n
        readChrom :: String -> Either String Int
        readChrom c = readErr ("cannot parse chromosome " ++ c) $ c

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

word :: A.Parser B.ByteString
word = A.takeTill isSpace
