#!/usr/bin/Rscript --vanilla

# run-all.R
# Shrub Flammability project
# Dylan Schwilk, Azaj Mahmud
# February 2023

# This steering script sources all of the non stand-lone code below to read in
# data, conduct PCAs, fit models, build tables and figures.

DATA_CACHE_DIR <- "./results"

#############################################################################
# read, clean and merged all the data Produces "alldata"
#############################################################################

source("./scripts/read_data.R")

#############################################################################
# Read hobos data
############################################################################

source("./scripts/read_hobos.R")


###########################################################################
# produce pca_data object, and out put were assigned to the alldata, 
# removed all the NAs and named it final_data, the final_data
# contains 137 observation without any NAs
##########################################################################

source("./scripts/flam_pca.R")

source("./scripts/hobo_bench_drying.R")

source("./scripts/analysis.R")

source("./scripts/segmented.R")

source("./scripts/ggplot_theme.R")

source("./scripts/figures.R")

#########################################################################
# Exploratory  figures done by Dr. Schwilk, this scripts is used
# alldata which has 148 observations and some of columns have NA
# and will return  rows containing non-finite values (`stat_smooth()`). 
# rows containing missing values (`geom_point()`). 
# Removed 3 rows containing non-finite values (`stat_smooth()`). 
# Removed 3 rows containing missing values (`geom_point()`)
########################################################################

source("./scripts/exploratory_figures.R")


