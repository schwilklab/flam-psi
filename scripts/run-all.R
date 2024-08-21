#!/usr/bin/Rscript --vanilla

# run-all.R
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# February 2023

# This steering script sources all of the non stand-lone code below to read in
# data, conduct PCAs, fit models, build tables and figures.

source("./scripts/ggplot_theme.R")


#############################################################################
# read, clean and merged all the data Produces "alldata"
#############################################################################

source("./scripts/read_data.R") # This will produce alldata as the combined dataset for 2023 and 2024

#############################################################################
# Read hobos data
############################################################################

DATA_CACHE_DIR <- "./results"

#source("./scripts/read_hobos.R") # read this once, this will produce hobos_wider.RDS for both 2023 and 2024

hobos_wider <- readRDS(file.path(DATA_CACHE_DIR, "hobos_wider.RDS"))

###########################################################################
# produce pca_data object
##########################################################################

source("./scripts/flam_pca.R")

# flam_pca creates one main data file used later: final_data 

source("./scripts/hobo_bench_drying.R") 
source("./scripts/analysis.R")  
#source("./scripts/segmented.R") 
#source("./scripts/figures.R")
