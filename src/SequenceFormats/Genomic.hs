module SequenceFormats.Genomic where

import SequenceFormats.Bed (BedEntry(..))
import SequenceFormats.Eigenstrat (EigenstratSnpEntry(..))
import SequenceFormats.FreqSum (FreqSumEntry(..))
import SequenceFormats.Pileup (PileupRow(..))
import SequenceFormats.Utils (Chrom)
import SequenceFormats.VCF (VCFentry(..))

import Control.Monad.Trans.Class (lift)
import Pipes (Producer, next, yield)

class Genomic a where
    genomicPosition :: a -> (Chrom, Int)

    genomicChrom :: a -> Chrom
    genomicChrom = fst . genomicPosition

    genomicBase :: a -> Int 
    genomicBase = snd . genomicPosition

instance Genomic EigenstratSnpEntry where
    genomicPosition (EigenstratSnpEntry c p _ _ _ _) = (c, p)

instance Genomic FreqSumEntry where
    genomicPosition (FreqSumEntry c p _ _ _ _ _) = (c, p)

instance Genomic PileupRow where
    genomicPosition (PileupRow c p _ _ _) = (c, p)

instance Genomic VCFentry where
    genomicPosition (VCFentry c p _ _ _ _ _ _ _ _) = (c, p)

data IntervalStatus = BedBehind | BedOn | BedAhead

filterThroughBed :: (Monad m, Genomic e) => Producer BedEntry m () -> Producer e m () -> Producer e m ()
filterThroughBed bedProd gProd = do
    b <- lift $ next bedProd
    let (bedCurrent, bedRest) = case b of
            Left _ -> error "Bed file empty or not readable"
            Right r -> r
    f' <- lift $ next gProd
    let (gCurrent, gRest) = case f' of
            Left _ -> error "Genomic stream empty or not readable"
            Right r -> r
    go bedCurrent gCurrent bedRest gRest
  where
    go bedCurrent gCurrent bedRest gRest = do
        let recurseNextBed = do
                b <- lift $ next bedRest
                case b of
                    Left () -> return ()
                    Right (nextBed, bedRest') -> go nextBed gCurrent bedRest' gRest
            recurseNextG = do
                f' <- lift $ next gRest
                case f' of
                    Left () -> return ()
                    Right (nextG, gRest') -> go bedCurrent nextG bedRest gRest'
        case bedCurrent `checkIntervalStatus` gCurrent of
            BedBehind -> recurseNextBed
            BedAhead -> recurseNextG
            BedOn -> do
                yield gCurrent
                recurseNextG
    checkIntervalStatus :: (Genomic e) => BedEntry -> e -> IntervalStatus
    checkIntervalStatus (BedEntry bedChrom bedStart bedEnd) g =
        case bedChrom `compare` genomicChrom g of
            LT -> BedBehind
            GT -> BedAhead
            EQ -> if bedStart + 1 > genomicBase g then
                      BedAhead
                  else
                      if bedEnd < genomicBase g then BedBehind else BedOn


chromFilter :: (Genomic e) => [Chrom] -> e -> Bool
chromFilter exclusionList = (`notElem` exclusionList) . genomicChrom
