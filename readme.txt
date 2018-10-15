Environmental filtering improves ecological niche models across multiple scales
Adrian A. Castellanos, Jerry W. Huntley, Gary Voelker, and A. Michelle Lawing
All code written by Adrian A. Castellanos

This repository contains all scripts (hopefully with reasonably helpful comments to lead one through them) and data for the manuscript named above. All work was done using R version 3.4.2 and the packages used for each script are detailed at the beginning (although some may not be necessary; I didn't double check that). 

Castellanos2018CrossValid.R - Script that contains the cross.valid function for taking presence, background, predictor variables, and arguments used and performing a number of iterations of k-fold cross validation to grab AUC, AUCdiff, omission rate, TSS, and contribution of the variables used. Below this are functions used to grab filter localities across a range of grid sizes in environmental space (this requires Dr. Sara Varela's environmental filter code found on her Github https://github.com/SaraVarela/envSample) and for performing the random sampling measures detailed in supplementary.

Castellanos2018SpatialThinCode - Script that provides 1) revision of spThin::thin function that reduced runtime in half (from when I last checked), 2) function to choose appropriate points when ties occur (so as to maximize distance), and 3) an example of how it works to run all of it and get results.

Castellanos2018Figure1 - Script that provides the workflow to create the map panels for each species and place them together in one plot. 

Castellanos2018Figure3 - Script to grab the results of each species, visualize them as line plots, and take filter localities to create model predictions and determine Schoener's D

Castellanos2018Figures4_5 - Script to produce prediction and anomaly maps for each filtered model and plot them in a 25 panel figure. 

All presence, background, and results files are .CSVs and are labeled as Aud_ (Icterus graudacauda), Dipo_ (Dipodomys compactus), Cerc_ (Cercotrichas leucophrys), and Loph_ (Lophuromys woosnami) depending on what species they are. Filtered datasets are contained in the bio1_12, bio*_*, PCA2, PCA3, PCA4, and spatial FilterLocalities .CSV files. 