{-# LANGUAGE OverloadedStrings #-}

-- |This module contains helper functions for file parsing.

module SequenceFormats.Utils (liftParsingErrors,
                              consumeProducer, readFileProd, readFileProdCheckCompress,
                              decompressMultiMember,
                              SeqFormatException(..), deflateFinaliser,
                              Chrom(..), word, gzipConsumer, writeFromPopper, Z.Deflate) where

import           Control.Error                    (readErr)
import           Control.Exception                (Exception, throw, throwIO)
import           Control.Monad                    (unless)
import           Control.Monad.Catch              (MonadThrow, throwM)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Class        (lift)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as B
import           Data.Char                        (isSpace)
import           Data.List                        (isSuffixOf)
import qualified Data.Streaming.Zlib              as Z
import           Pipes                            (Consumer, Producer, await,
                                                   next, yield)
import           Pipes.Attoparsec                 (ParsingError (..), parsed)
import qualified Pipes.ByteString                 as PB
import qualified Pipes.Safe                       as PS
import qualified Pipes.Safe.Prelude               as PS
import           System.IO                        (Handle, IOMode (..))

-- |An exception type for parsing BioInformatic file formats.
data SeqFormatException = SeqFormatException String
    deriving (Eq)

instance Show SeqFormatException where
    show (SeqFormatException msg) = "SeqFormatException: " ++ msg

instance Exception SeqFormatException

-- |A wrapper datatype for Chromosome names.
newtype Chrom = Chrom {unChrom :: B.ByteString} deriving (Eq)

-- |Show instance for Chrom
instance Show Chrom where
    show (Chrom c) = B.unpack c

-- |Ord instance for Chrom
instance Ord Chrom where
    compare (Chrom c1) (Chrom c2) =
        let (c1NoChr, c2NoChr) = (removeChr c1, removeChr c2)
            (c1XYMTconvert, c2XYMTconvert) = (convertXYMT c1NoChr, convertXYMT c2NoChr)
        in  case (,) <$> readChrom c1XYMTconvert <*> readChrom c2XYMTconvert of
                Left e           -> throw e
                Right (cn1, cn2) -> cn1 `compare` cn2
      where
        removeChr :: B.ByteString -> B.ByteString
        removeChr c = if B.take 3 c == "chr" then B.drop 3 c else c
        convertXYMT :: B.ByteString -> B.ByteString
        convertXYMT c = case c of
            "X"  -> "23"
            "Y"  -> "24"
            "MT" -> "90"
            n    -> n
        readChrom :: B.ByteString -> Either SeqFormatException Int
        readChrom c = readErr (SeqFormatException $ "cannot parse chromosome " ++ B.unpack c) . B.unpack $ c

-- |A function to help with reporting parsing errors to stderr. Returns a clean Producer over the
-- parsed datatype.
liftParsingErrors :: (MonadThrow m) =>
    Either (ParsingError, Producer B.ByteString m r) () -> Producer a m ()
liftParsingErrors res = case res of
    Left (ParsingError _ msg, restProd) -> do
        x <- lift $ next restProd
        case x of
            Right (chunk, _) -> do
                let firstLine = B.unpack . B.takeWhile (/= '\n') $ chunk
                    msg' = "Error while parsing: " <> msg <> ". Offending line: " ++ firstLine
                throwM $ SeqFormatException msg'
            Left _ -> error "should not happen"
    Right () -> return ()

-- |A helper function to parse a text producer, properly reporting all errors to stderr.
consumeProducer :: (MonadThrow m) => A.Parser a -> Producer B.ByteString m () -> Producer a m ()
consumeProducer parser prod = parsed parser prod >>= liftParsingErrors

readFileProd :: (PS.MonadSafe m) => FilePath -> Producer B.ByteString m ()
readFileProd f = PS.withFile f ReadMode PB.fromHandle

readFileProdCheckCompress :: (PS.MonadSafe m) => FilePath -> Producer B.ByteString m ()
readFileProdCheckCompress f =
    let decompressFunc = if ".gz" `isSuffixOf` f then decompressGzip ("gzip file " ++ f) else id
    in  decompressFunc $ PS.withFile f ReadMode PB.fromHandle

-- |Decompresses a gzip stream that may consist of multiple concatenated gzip members, as is the
-- case for BGZF files written by bgzip, bcftools or GATK. Pipes.GZip.decompress on its own stops
-- after the first member and silently drops the rest of the input. Throws a SeqFormatException
-- if the input is not valid gzip data, or if it ends in the middle of a gzip member (e.g. a
-- truncated file), which Pipes.GZip.decompress would silently accept.
decompressMultiMember :: (MonadIO m) => Producer B.ByteString m r -> Producer B.ByteString m r
decompressMultiMember = decompressGzip "gzip stream"

-- |Like decompressMultiMember, but takes a description of the input (e.g. the file name) for
-- error messages.
decompressGzip :: (MonadIO m) => String -> Producer B.ByteString m r -> Producer B.ByteString m r
decompressGzip descr = newMember True
  where
    newMember isFirst prod = liftIO (Z.initInflate (Z.WindowBits 31)) >>= go isFirst False prod
    -- hasInput tracks whether the current member has received any bytes yet, so that we can
    -- tell a clean end of input (after a completed member) from a truncated member.
    go isFirst hasInput prod inf = do
        res <- lift (next prod)
        case res of
            Left r -> do
                complete <- liftIO (Z.isCompleteInflate inf)
                if complete || (not hasInput && not isFirst) then return r else
                    liftIO . throwIO . SeqFormatException $ if hasInput
                        then descr ++ " ended unexpectedly. The file seems to be truncated"
                        else descr ++ " is empty, which is not valid gzip"
            Right (bs, prod')
                | B.null bs -> go isFirst hasInput prod' inf
                | otherwise -> do
                    popper <- liftIO (Z.feedInflate inf bs)
                    yieldPopper popper
                    rest <- liftIO (Z.flushInflate inf)
                    unless (B.null rest) (yield rest)
                    complete <- liftIO (Z.isCompleteInflate inf)
                    if complete then do
                        leftover <- liftIO (Z.getUnusedInflate inf)
                        newMember False (yield leftover >> prod')
                    else
                        go isFirst True prod' inf
    yieldPopper popper = do
        popRes <- liftIO popper
        case popRes of
            Z.PRDone -> return ()
            Z.PRNext bs -> yield bs >> yieldPopper popper
            Z.PRError (Z.ZlibException code) -> liftIO . throwIO . SeqFormatException $
                "could not decompress " ++ descr ++ " (zlib error code " ++ show code ++
                    "). The file seems to be corrupt or not gzip-compressed"

word :: A.Parser B.ByteString
word = A.takeTill isSpace

gzipConsumer :: (MonadIO m) => Z.Deflate -> Handle -> Consumer B.ByteString m ()
gzipConsumer def h = do
    bs <- await
    pop <- liftIO (Z.feedDeflate def bs)
    liftIO (writeFromPopper pop h)
    gzipConsumer def h

writeFromPopper :: (MonadIO m) => Z.Popper -> Handle -> m ()
writeFromPopper pop h = do
   popResult <- liftIO pop
   case popResult of
      Z.PRDone    -> return ()
      Z.PRError e -> liftIO $ throwIO e
      Z.PRNext bs -> do
         liftIO $ B.hPut h bs
         writeFromPopper pop h

deflateFinaliser :: (MonadIO m) => Z.Deflate -> Handle -> m ()
deflateFinaliser def h = do
    let finalPop = liftIO $ Z.finishDeflate def
    writeFromPopper finalPop h
