{-# LANGUAGE OverloadedStrings #-}
module Utils (liftParsingErrors, consumeProducer) where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.Text (Text)
import Pipes (Producer)
import Pipes.Attoparsec (ParsingError(..), parsed)
import qualified Data.Attoparsec.Text as A

liftParsingErrors :: (MonadThrow m) =>
    Either (ParsingError, Producer Text m r) () -> Producer a m ()
liftParsingErrors res = case res of
    Left (e, _) -> throwM e
    Right () -> return ()

consumeProducer :: (MonadThrow m) => A.Parser a -> Producer Text m () -> Producer a m ()
consumeProducer parser prod = parsed parser prod >>= liftParsingErrors
