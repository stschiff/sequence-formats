{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.Utils (liftParsingErrors,
                              consumeProducer,
                              FormatException(..),
                              Chrom(..)) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import Pipes (Producer, next)
import Pipes.Attoparsec (ParsingError(..), parsed)

newtype Chrom = Chrom {unChrom :: T.Text} deriving (Eq)

instance Show Chrom where
    show (Chrom c) = show c

instance Ord Chrom where
    compare (Chrom c1) (Chrom c2) = 
        let c1' = if T.take 3 c1 == "chr" then T.drop 3 c1 else c1
            c2' = if T.take 3 c2 == "chr" then T.drop 3 c2 else c2
            cn1 = read . T.unpack $ c1' :: Int
            cn2 = read . T.unpack $ c2' :: Int
        in  cn1 `compare` cn2

data FormatException = FormatException T.Text
    deriving Show

instance Exception FormatException

liftParsingErrors :: (MonadThrow m) =>
    Either (ParsingError, Producer T.Text m r) () -> Producer a m ()
liftParsingErrors res = case res of
    Left (ParsingError cont msg, restProd) -> do
        Right (chunk, _) <- lift $ next restProd
        let msg' = msg ++ " Error occurred while trying to parse this chunk: " ++ show chunk
        throwM (ParsingError cont msg')
    Right () -> return ()

consumeProducer :: (MonadThrow m) => A.Parser a -> Producer T.Text m () -> Producer a m ()
consumeProducer parser prod = parsed parser prod >>= liftParsingErrors
