# read-data.R
# Dylan Schwilk 2023

library(readr)
library(dplyr)
library(tidyr)

###############################################################################
# Constants
###############################################################################

# Xiulin Gao used 0.921 J/g as the Specific heat of the aluminum alloy of which
# our discs are made. From the grass experiments we had 52.91g and 53.21g which
# are the weight of Disc one and Disc two respectively(Gao and Schwilk, 2021).
# The gas flow from the Blue Rhino gas cylinder was 20.35 gram per minute.

SPECIFIC_HEAT_AL <- 0.921 # in J/g
MASS_DISK_1 <- 52.91  # 
MASS_DISK_2 <- 53.21 # g


###############################################################################
## Read all the data files
###############################################################################

## We need to read them all in first before cleaning because some calculations
## require columns from multiple files

species <- read_csv("./data/species.csv") %>%
  mutate(display_name = paste(substr(genus, 1,1), ". ", specific_epithet, sep="")) %>%
  dplyr::select(spcode, display_name, scientific_name)
samples <-  read_csv("./data/samples.csv") %>% select(sample_id, spcode)

water_potentials <- read_csv("./data/water_potentials.csv") %>%
  select(sample_id, wp)

fmc <- read_csv("./data/fmc.csv") %>%
  mutate(fmc=(fresh_mass-dry_mass)/dry_mass) %>%
  select(sample_id, fmc)

###############################################################################
#
burn_trials <- read_csv("./data/burn_trials.csv") %>%
  mutate(heat1 = (disc1_post - disc1_pre) * MASS_DISK_1 * SPECIFIC_HEAT_AL,
         heat2 = (disc2_post - disc2_pre) * MASS_DISK_2 * SPECIFIC_HEAT_AL,
         heat_release_j = (heat1 + heat2)/2, # average heat release of two disks
         pre_combustion = pre_combustion=="yes"
         ) %>%
  select(sample_id, pre_combustion, ignition_delay, flame_duration,
         smoke_duration, flame_height, heat_release_j, vol_burned,
         self_ignition, self_ig_start)

burn_trials_wx <- read_csv("./data/burn_trials_wx.csv") %>% 
  mutate(massloss = (mass_pre - mass_post)/mass_pre) %>%
  select(sample_id, rh, temperature, wind_speed, mass_pre, massloss)

burn_trials <- left_join(burn_trials, burn_trials_wx)
alldata <- left_join(samples, species) %>% left_join(water_potentials) %>%
  left_join(fmc) %>% left_join(burn_trials)
