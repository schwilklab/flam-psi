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

# produce pca_data object
source("./scripts/flam_pca.R")
alldata <- alldata %>%
  left_join(dplyr::select(pca_data, sample_id, PC1, PC2), by = "sample_id")

# figures
source("./scripts/figs.R")

