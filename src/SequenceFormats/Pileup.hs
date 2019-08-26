{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.Pileup (pileupParser, processPileupEntry, PileupRow(..)) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)

import SequenceFormats.Utils (Chrom(..), word)

data PileupRow = PileupRow Chrom Int Char [String]

pileupParser :: A.Parser PileupRow
pileupParser = do
    chrom <- word
    _ <- A.space
    pos <- A.decimal
    _ <- A.space
    refA <- A.satisfy (A.inClass "ACTGNactgnM")
     -- for some reason, there is an M in the human reference at
     -- position 3:60830534 (both in hs37d5 and in hg19)
    _ <- A.space
    entries <- parsePileupPerSample refA `A.sepBy1`
        A.satisfy (\c -> c == ' ' || c == '\t')
    A.endOfLine
    let ret = PileupRow (Chrom $ B.unpack chrom) pos refA entries
    --trace (show ret) $ return ret
    return ret
  where
    parsePileupPerSample refA =
        processPileupEntry refA <$> A.decimal <* A.space <*> (B.unpack <$> word) <*
            A.space <* word

processPileupEntry :: Char -> Int -> String -> String
processPileupEntry refA cov readBaseString =
    if cov == 0 then "" else go readBaseString
  where
    go (x:xs)
        | (x == '.' || x == ',') = refA : go xs
        | x `elem` ("ACTGNactgn" :: String) = toUpper x : go xs
        | x `elem` ("$*" :: String) = go xs
        | x == '^' = go (drop 1 xs)
        | (x == '+' || x == '-') =
            let [(num, rest)] = reads xs in go (drop num rest)
        | otherwise = error $ "cannot parse read base string: " ++ (x:xs)
    go [] = []
