#!/usr/bin/Rscript --vanilla

# run-all.R
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# February 2023

# This steering script sources all of the non stand-lone code below to read in
# data, conduct PCAs, fit models, build tables and figures.

source("./scripts/ggplot_theme.R")

DATA_CACHE_DIR <- "./results"

#############################################################################
# read, clean and merged all the data Produces "alldata"
#############################################################################

source("./scripts/read_data.R")

#############################################################################
# Read hobos data
############################################################################

#source("./scripts/read_hobos.R")

#source("./scripts/read_hobos_2024.R")


###########################################################################
# produce pca_data object
##########################################################################

source("./scripts/flam_pca.R")
# flam_pca creates the two main data files sued later: final_data (2023 data)
# and final_data_2024 (summer 2024 data)

source("./scripts/hobo_bench_drying.R") # only 2023 stuff
source("./scripts/analysis.R")  # only uses final_data (2023).
source("./scripts/segmented.R") # only 2023
source("./scripts/figures.R") # only 2023

#########################################################################
# Exploratory  figures done by Dr. Schwilk, this scripts is used
# alldata which has 148 observations and some of columns have NA
# and will return  rows containing non-finite values (`stat_smooth()`). 
# rows containing missing values (`geom_point()`). 
# Removed 3 rows containing non-finite values (`stat_smooth()`). 
# Removed 3 rows containing missing values (`geom_point()`)
########################################################################

source("./scripts/exploratory_figures.R") # 2023 and 2024


