module SequenceFormats.Bed (BedEntry(..), readBedFile) where

import SequenceFormats.Utils (Chrom(..), readFileProd, consumeProducer)

import Data.Char (isSpace)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Pipes (Producer)
import Pipes.Safe (MonadSafe)

data BedEntry = BedEntry Chrom Int Int deriving (Show, Eq)

bedFileParser :: A.Parser BedEntry
bedFileParser = BedEntry <$> chrom <* A.skipSpace <*> A.decimal <* A.skipSpace <*> A.decimal <* A.endOfLine
  where
    chrom = Chrom <$> A.takeTill isSpace

readBedFile :: (MonadSafe m) => FilePath -> Producer BedEntry m ()
readBedFile bedFile = consumeProducer bedFileParser (readFileProd bedFile)
