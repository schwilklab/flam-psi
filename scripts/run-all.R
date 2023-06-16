#!/usr/bin/Rscript --vanilla

# run-all.R

DATA_CACHE_DIR <- "./results"

# read, clean and merged all the morphological and flammability traits data.
# Produces "alldata"
source("./scripts/read_data.R")

# script that reads the thermocouple data logger data during burning. Produces
# "hobos_wider"
#source("./scripts/read_hobos.R")  ## Do this once
## After running the above once, just read saved data:
hobos_wider <- readRDS(file.path(DATA_CACHE_DIR, "hobos_wider.RDS"))
alldata <- left_join(alldata, hobos_wider)

library(ggplot2)
ggplot(alldata, aes(wp, fmc, color=spcode)) + geom_point() + geom_smooth(method="lm")

ggplot(alldata, aes(wp, massloss, color=spcode)) + geom_point() + geom_smooth(method="lm")

ggplot(alldata, aes(wp, vol_burned, color=spcode)) + geom_point() + geom_smooth(method="lm")

ggplot(filter(alldata, spcode!= "JUPI"), aes(wp, flame_duration, color=spcode)) + geom_point() + geom_smooth(method="lm")

ggplot(alldata, aes(mass_pre, ignition_delay, color=spcode)) + geom_point()


#source("./scripts/flam_pca.R") 
#source("./scripts/analysis.R")

# run alldata from the 2021
#source("./scripts/read_data_2021.R")

# read hobos data for 2021
#source("./scripts/hobo_flam_pca_2021.R")

# read herbivore analysis script

#source("./scripts/herbivore_analysis.R")

# figures
#source("./scripts/ms_figures.R")

