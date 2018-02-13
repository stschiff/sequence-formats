{-# LANGUAGE OverloadedStrings #-}

module SequenceFormats.Eigenstrat (EigenstratSnpEntry(..), eigenstratSnpParser) where

import Control.Monad (void)
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import qualified Data.Text as T

data EigenstratSnpEntry = EigenstratSnpEntry T.Text Int Char Char deriving (Show)
    -- Chrom Pos Ref Alt

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
    ref <- A.satisfy (A.inClass "ACTGX")
    A.skipMany1 A.space
    alt <- A.satisfy (A.inClass "ACTGX")
    void A.endOfLine
    return $ EigenstratSnpEntry chrom pos ref alt
  where
    word = A.takeTill isSpace
