# CRDSscreening
Data and code for manuscript on spectral screening and correction of CRDS water isotope data

## data/
Data assets used in analysis

- **cc/** Picarro ChemCorrect files for CRDS runs
- **cf/** Picarro coordinator files for CRDS runs
- **irms.csv** IRMS analysis results

## code/
Scripts used for data analysis and plotting

- **X1_pullCRDS.R** Compile Picarro CRDS files from instrument directories and saves them in *data/*; not portable
- **01_prepData.R** Download CRDS results, basic cleaning/prep, save locally
- **02_irms.R** Clean, prep, merge and explore IRMS results
- **03_spectralParms.R** Extract, merge, and save spectral metrics for all samples
- **04_spectralCal.R** Fit offset correction models, explore
- **05_spectralCorrection.R** Apply offset correction models, explore
- **06_summaries.R** Summary metrics for offset corrections
- **07_chemCorrect.R** Compare observed and modeled bias with flags from commercial software
- **10_plotting.R** Generate figures

## output/
Objects output by code, including intermediate data products, models, and figures
