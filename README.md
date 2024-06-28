# BASE


## Introduction

This contains the workflow for preprocessing the data, fitting the models, and analysing and plotting the output to build  BASE (Burnt Area Simulator for Europe) v1.0.  

Paper will be submitted to Biogeosciences:

Understanding and simulating cropland and non-cropland burning in Europe using the BASE (Burnt Area Simulator for Europe) model
Matthew Forrest, Jessica Hetzer, Maik Billing, Simon P.K. Bowring, Erik Kosczor, Luke Oberhagemann, Oliver Perkins, Dan Warren, Fátima Arrogante-Funes, Kirsten Thonicke, and Thomas Hickler 

## Workflow and code components

0. (documention only) Some data pre-processing scripts (but not all) can be found in `scripts/data_prep` to prepare the data at the FirEUrisk 9km grid. These are just provided as additional documentation, the main large table used for the fitting is avaialble from Zenodo. 
1. (optional) Navigate to `external_files` and set up links to the raw FireCCI51 data, the raw LandCoverCCI data.  This is only necessary if you want to repeat the burnt fraction per land cover type analyis.
2. (option) Run `scripts/calc_BF_per_LCT.R` to repeat the burnt fraction per landcover type analyis.
3. Download the large data table from the Zenodo repository and put it into `data_tables/BASE_v1.0`.
4. Fit all the models by running `scripts/fute_BASE_v1.0.R`.  The fitted models and all associated plots should appear in `fitted_models/BASE_v1.0`
5. To reproduce all the manuscript plots run `scripts/publication_plots/BASE_v1.0/run_all_pub_v1.0.R`.  Plots should appear in `plots/BASE_v1.0/manuscript`.

