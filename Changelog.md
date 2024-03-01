# Changelog

- V 1.8.0.1: Allow reading arbitrary letters as reference base in Pileup format. Before only ACTG, N and M were allowed.
    Now all letters are allowed, as we found out that reference fasta files can contain more letters than just those and we
    don't want to immediately stop parsing in that case.
- V 1.8.0.0: Fixed a bug in the VCF parser that would not accept missing values in QUAL fields (even though the spec explicitly allows for it). This also changed the VCF row structure data type
- V 1.7.1: Made compatible with latest GHC
- V 1.7.0: Bumped major version for breaking change (correct mistake from 1.6.7.0). Also correct lower pvp-bounds on hackage to be compatible with LTS-18.28.
- V 1.6.7.0:
    * Added an option to Plink output, which allows users to store the population mame in either the first or last column, or both
    * Fixed a bug in the pileup-parser which failed at reference-skip symbols (">" and "<").
- V 1.6.6.0: Added a minimal Bed parser and a bed-filter
- V 1.6.5.0: Long chromosome names are now parsed correctly
- V 1.6.4: Fixed a bug in Fasta parsing which would skip the first line if the header was minimal.
- V 1.6.3: Pileup reference-base is read as upper case, even if it's not in the file.
- V 1.6.2: Made compatible with GHC 9 (thanks to github user sjakobi)
- V 1.6.0: Plink Output now supported.
- V 1.5.1.4: Plink BIM parsing now automatically converts from Numbers (0,1,2,3,4) to Letters (N,A,C,T,G).
- V 1.5.1.3: added possibility to parse allele names 01234 in bim files.
- V 1.5.1.2: added readPlink to export list of Plink module.
- V 1.5.1: minor updates for hackage
- V 1.5.0: Added Plink support.
- V 1.4.1:
    * Added optional genetic position to FreqSumformat,
    * changed various internal strings toByteStrings and vice versa.
- V 1.4.0.1: Added test file example.pileup to cabal extra-source-files to make tests work.
- V 1.4.0: Added three features:
    * Chromosomes now include `X`, `Y` and `MT` (or `chrX`, `chrY`, `chrMT`), in that order after `chr22`. 
    * SNP rsId information is now internallyincluded as an option in the FreqSum data format.
    * Pileup Format now also records strandorientation
- V 1.3.3: Added Pileup as new format. Changed all tests to Hspec.
- V 1.3.2.1: Fixed a hard-coded absolute path in the test-suite.
- V 1.3.2: Added testDat to Cabal file to make tests work off the tarball.
- V 1.3.1: Moved test suite outside of the main library into the test source directory. Cleaner setup.
- V 1.3.0: Removed pipes-text, text and turtle dependencies and some more. Restructured all datatypes to use Bytestring instead of text. 
- V 1.2.0: Added tests for VCF, and several bugfixes. Now runs on LTS-14.1 with pipes-text as legacy dependency.
- V 1.1.8.3: Added tests for Fasta import. Succeed.
- V 1.1.8.2: Added Eq and Show classes to various FreqSum entities. Fixed writing function, added tests.
- V 1.1.8.1: Added Eq class to EigenstratInd and Sex
- V 1.1.8: Added more consumers for Bim and Eigenstrat Snp files
- V 1.1.7: Added option to parse Bim file (slightly different layout to Eigenstrat Snp Format), and added genetic position and snpId to the EigenstratSnpEntry datastructure. This will cause breaking changes in code linking against this library.
- V 1.1.6: VCF parser now allows for truncated VCF files with no Format and Genotypes (sites-only VCF files)
- V 1.1.5: Fixed VCF parser: Now breaks if lines end prematurely
- V 1.1.4.2: Exporting readVCFfromProd
- V 1.1.4.1: First entry in the Changelog. Added Haddock documentation to all modules and prepare for releasing on Hackage.




















