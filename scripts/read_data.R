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

species <- read_csv("./data/species.csv")

samples <-  read_csv("./data/samples.csv") 

water_potentials <- read_csv("./data/water_potentials.csv") 

fmc <- read_csv("./data/fmc.csv")

burn_trials <- read_csv("./data/burn_trials.csv")

burn_trials_wx <- read_csv("./data/burn_trials_wx.csv")

###############################################################################
## Cleaning the data
##############################################################################

species <- species %>%
  mutate(display_name = paste(substr(genus, 1,1), ". ", specific_epithet, sep="")) %>%
  dplyr::select(spcode, display_name, scientific_name)

samples <-  samples %>% 
  dplyr::select(sample_id, spcode)

water_potentials <- water_potentials %>%
  mutate(wp = -wp) %>%
  dplyr::select(sample_id, wp)

fmc <- fmc %>%
  mutate(fmc=((fresh_mass-dry_mass)/dry_mass)*100) %>%
  mutate(fmc = round(fmc, 2)) %>%
  dplyr::select(sample_id, fmc)

burn_trials_wx <- burn_trials_wx %>% 
  mutate(massloss = (mass_pre - mass_post)/mass_pre) %>%
  dplyr::select(sample_id, rh, temperature, wind_speed, mass_pre, massloss)

###############################################################################
## burn trials
##############################################################################

burn_trials <- read_csv("./data/burn_trials.csv") %>%
  mutate(heat1 = (disc1_post - disc1_pre) * MASS_DISK_1 * SPECIFIC_HEAT_AL,
         heat2 = (disc2_post - disc2_pre) * MASS_DISK_2 * SPECIFIC_HEAT_AL,
         heat_release_j = (heat1 + heat2)/2, # average heat release of two disks
         pre_combustion = pre_combustion=="yes" ,
         pre_burning_temp = (disc1_pre + disc2_pre)/2) %>%
  dplyr::select(sample_id, pre_combustion, ignition_delay, flame_duration,
         smoke_duration, flame_height, heat_release_j, vol_burned,
         self_ignition, self_ig_start, pre_burning_temp)

# Correct heat release to set lowest value at 0 (all relative anyway)
burn_trials$heat_release_j <- burn_trials$heat_release_j - min(burn_trials$heat_release_j, na.rm=TRUE)


############################################################################
## Combining them all
############################################################################

burn_trials <- left_join(burn_trials, burn_trials_wx)

alldata <- left_join(samples, species) %>% left_join(water_potentials) %>%
  left_join(fmc) %>% left_join(burn_trials)

alldata$spcode <- as.factor(alldata$spcode)


dim(alldata)

################################################################################
# Initially creating two subset of final_data because we need them later
# Initially samples without self ignition and then samples which ignited 
# within 20 seconds
###############################################################################

without_self_ignition <- alldata %>%
  filter(ignition_delay != 0)

dim(without_self_ignition) # 143


filtered_data <- without_self_ignition %>%
  filter(ignition_delay <= 20)

dim(filtered_data)

filtered_sample_id <- filtered_data$sample_id

alldata$ignition_delay <- log(alldata$ignition_delay + 1)

without_self_ignition <- without_self_ignition %>%
  mutate(ignition_delay = log(ignition_delay + 1))

######################################################################################
## Cleaning up work space, only keeping the alldata
######################################################################################

rm("burn_trials", "burn_trials_wx", "fmc", "MASS_DISK_1", "MASS_DISK_2", "samples",
   "species", "SPECIFIC_HEAT_AL", "water_potentials")
