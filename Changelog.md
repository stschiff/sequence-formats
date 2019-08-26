V 1.1.4.1: First entry in the Changelog. Added Haddock documentation to all modules and prepare for releasing on Hackage.

V 1.1.4.2: Exporting readVCFfromProd

V 1.1.5: Fixed VCF parser: Now breaks if lines end prematurely

V 1.1.6: VCF parser now allows for truncated VCF files with no Format and Genotypes (sites-only VCF files)

V 1.1.7: Added option to parse Bim file (slightly different layout to Eigenstrat Snp Format), and added genetic position and snpId to the EigenstratSnpEntry datastructure. This will cause breaking changes in code linking against this library.

V 1.1.8: Added more consumers for Bim and Eigenstrat Snp files

V 1.1.8.1: Added Eq class to EigenstratInd and Sex

V 1.1.8.2: Added Eq and Show classes to various FreqSum entities. Fixed writing function, added tests.

V 1.1.8.3: Added tests for Fasta import. Succeed.

V 1.2.0: Added tests for VCF, and several bugfixes. Now runs on LTS-14.1 with pipes-text as legacy dependency.

V 1.3.0: Removed pipes-text, text and turtle dependencies and some more. Restructured all datatypes to use Bytestring instead of text. 

V 1.3.1: Moved test suite outside of the main library into the test source directory. Cleaner setup.

V 1.3.2: Added testDat to Cabal file to make tests work off the tarball.

V 1.3.2.1: Fixed a hard-coded absolute path in the test-suite.

V. 1.3.3: Added Pileup as new format. Lacking tests yet.