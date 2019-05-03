V 1.1.4.1: First entry in the Changelog. Added Haddock documentation to all modules and prepare for releasing on Hackage.

V 1.1.4.2: Exporting readVCFfromProd

V 1.1.5: Fixed VCF parser: Now breaks if lines end prematurely

V 1.1.6: VCF parser now allows for truncated VCF files with no Format and Genotypes (sites-only VCF files)

V 1.1.7: Added option to parse Bim file (slightly different layout to Eigenstrat Snp Format), and added genetic position and snpId to the EigenstratSnpEntry datastructure. This will cause breaking changes in code linking against this library.

V 1.1.8: Added more consumers for Bim and Eigenstrat Snp files

V 1.1.8.1: Added Eq class to EigenstratInd and Sex