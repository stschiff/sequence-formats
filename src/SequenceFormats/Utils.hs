{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.Utils (liftParsingErrors, consumeProducer, FormatException(..)) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import Pipes (Producer, next)
import Pipes.Attoparsec (ParsingError(..), parsed)

data FormatException = FormatException Text
    deriving Show

instance Exception FormatException

liftParsingErrors :: (MonadThrow m) =>
    Either (ParsingError, Producer Text m r) () -> Producer a m ()
liftParsingErrors res = case res of
    Left (ParsingError cont msg, restProd) -> do
        Right (chunk, _) <- lift $ next restProd
        let msg' = msg ++ " Error occurred while trying to parse this chunk: " ++ show chunk
        throwM (ParsingError cont msg')
    Right () -> return ()

consumeProducer :: (MonadThrow m) => A.Parser a -> Producer Text m () -> Producer a m ()
consumeProducer parser prod = parsed parser prod >>= liftParsingErrors
