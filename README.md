# SNU_ASDsync

Code for feature extraction of synchrony prediction in autism study (Koehler, Dong, et al., 2023)

## 1. Software Requirements

This code is has been tested on macOS (Version 12.6) and Linux (Ubuntu 20.04), but is also supported for Windows.

It uses data as outputted from Motion Energy Analysis (https://psync.ch/downloads/) and Praat (https://www.fon.hum.uva.nl/praat/).

R (https://cran.rstudio.com) and RStudio (https://posit.co/download/rstudio-desktop/) have to be installed.

## 2. Instructions

### Synchrony scripts (INTERsync.R & INTRAsync.R):

These script calculates the inter- and intrapersonal synchrony per dyad and extract machine learning features.

### SVM decision scores followup (decisionscores_followup_analysis.R):

This script performs correlational analyses between the resulting SVM decision scores and clinical variables.

### Cross-validation scripts:

The following scripts are for creating the CV structure unique to this study by splitting the training and testing folds using dyads instead of individual participants. The output CV structure can be imported by NeuroMiner directly. To create the CV structure file, first run the .ipynb (jupyter notebook) file, and then take the output from this file as input to run the .m (matlab) script.

### Supplementary scripts:

These scripts were used for supplementary analyses in our study on vocal output (vocal_output_analysis.R) and meta-information on our video database (followup_video_analysis.R)
