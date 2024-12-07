# read-data.R
# Dylan Schwilk

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

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

species <- read_csv("./data/2024/species.csv")

samples <-  read_csv("./data/combined/samples.csv") 

water_potentials_fmc <- read_csv("./data/combined/wp_fmc.csv") 

burn_trials <- read_csv("./data/combined/burn_trials.csv")

burn_trials_weather <- read_csv("./data/combined/burn_trials_weather.csv")

leaf_traits <- read_csv("./data/combined/leaf_traits.csv")

juniperus_leaf_traits <- read_csv("./data/combined/juniperus_lma.csv")

time_wp <- read_csv("./data/2024/time_wp.csv")

pv_summary <- read_csv("./data/2024/pv_curve_summary.csv")

###############################################################################
## Cleaning the data
##############################################################################

species <- species %>%
  mutate(display_name = paste(substr(genus, 1,1), ". ", specific_epithet, sep="")) %>%
  dplyr::select(spcode, display_name, scientific_name)

wp_fmc <- water_potentials_fmc %>%
  mutate(wp = -1*wp) %>%
  mutate(lfmc = ((leaf_fresh_mass - leaf_dry_mass)/leaf_dry_mass)*100) %>%
  mutate(lfmc = round(lfmc, 2)) %>%
  mutate(cmc = ((twig_leaf_fresh_mass - twig_leaf_dry_mass)/twig_leaf_dry_mass)*100) %>%
  mutate(cmc = round(cmc, 2)) %>%
  dplyr::select(sample_id, wp, lfmc, cmc, twig_leaf_fresh_mass, twig_leaf_dry_mass)

dim(wp_fmc)

burn_trials_weather <- burn_trials_weather %>%
  mutate(spcode = ifelse(spcode == "PHTR", "RHTR", spcode))

dim(burn_trials_weather)

###############################################################################
## burn trials
##############################################################################

burn_trials <- burn_trials %>%
  mutate(year = format(as.Date(date, format = "%m/%d/%y"), "%Y")) %>%
  mutate(mass_pre = ifelse(year == 2023, mass_pre*1000, mass_pre),
         mass_post = ifelse(year == 2023, mass_post*1000, mass_post)) %>%
  mutate(heat1 = (max_temp - disc1_pre) * MASS_DISK_1 * SPECIFIC_HEAT_AL,
         heat2 = (max_temp - disc2_pre) * MASS_DISK_2 * SPECIFIC_HEAT_AL,
         heat_release_j = (heat1 + heat2)/2, # average heat release of two disks
         pre_burning_temp = (disc1_pre + disc2_pre)/2) %>%
  mutate(massloss = (mass_pre - mass_post)/mass_pre) %>%
  dplyr::select(sample_id, year, pre_combustion, ignition_delay, flame_duration,
                flame_height, heat_release_j, vol_burned, massloss,
                self_ignition, pre_burning_temp, mass_pre)

# Correct heat release to set lowest value at 0 (all relative anyway)
burn_trials$heat_release_j <- burn_trials$heat_release_j - min(burn_trials$heat_release_j, na.rm=TRUE)

dim(burn_trials)

##########################################################################################
# Leaf traits
#########################################################################################

ldmc_leaf_length <- leaf_traits %>%
  mutate(ldmc = (ldmc_dry*1000)/ldmc_fresh) %>%
  mutate(ldmc = round(ldmc, 2)) %>%
  rowwise() %>%
  mutate(leaf_length = mean(c_across(10:14), na.rm = TRUE)) %>%
  mutate(leaf_length = round(leaf_length, 2)) %>%
  ungroup() %>%
  dplyr::select(sample_id, ldmc, leaf_length)

dim(ldmc_leaf_length)

juniperus_leaf_area_2024 <- juniperus_leaf_traits %>% 
  mutate(branchlet_radius_cm = branchlet_diameter_mm/20,  # mm to cm by dividing by 10 and dividing by to 2 to get the radius 
         branchlet_length_cm = branchlet_length_mm/10,
         leaf_area_per_branchlet = pi*branchlet_radius_cm*branchlet_length_cm) %>% # pi*D*L/2 as one sided
  group_by(sample_id) %>%
  summarise(leaf_area = sum(leaf_area_per_branchlet), # Total leaf area which will be 
            # used to calculate LMA  
            leaf_area_per_leaflet = mean(leaf_area_per_branchlet)) %>%
  mutate(leaf_area = round(leaf_area, 2)) %>%
  mutate(leaf_area_per_leaflet = round(leaf_area_per_leaflet, 2)) %>%
  dplyr::select(sample_id, leaf_area, leaf_area_per_leaflet)

dim(juniperus_leaf_area_2024)

lma_leaf_area <- leaf_traits %>%
  filter(! sample_id %in% juniperus_leaf_area_2024$sample_id) %>%
  mutate(leaf_area_per_leaflet = leaf_area/5) %>%
  mutate(leaf_area_per_leaflet = round(leaf_area_per_leaflet, 2)) %>%
  dplyr::select(sample_id, leaf_area, leaf_area_per_leaflet) %>%
  rbind(juniperus_leaf_area_2024)

dim(lma_leaf_area)

all_leaf_traits <- leaf_traits %>%
  dplyr::select(sample_id, lma_dry) %>%
  right_join(ldmc_leaf_length, by = "sample_id") %>%
  right_join(lma_leaf_area, by = "sample_id") %>%
  mutate(lma = lma_dry/leaf_area) %>%
  mutate(lma = round(lma, 3)) %>%
  dplyr::select(- lma_dry)

dim(all_leaf_traits)

#######################################################################################
# Time vs wp 
######################################################################################

time_wp <- time_wp %>%
  mutate(fmc = ((fresh_weight - dry_weight)/dry_weight)*100) %>%
  mutate(date_time = mdy_hms(paste(time_wp$date, time_wp$time))) %>%
  group_by(sample_id) %>%
  mutate(hours = as.numeric(difftime(date_time, first(date_time), units = "hours")),
         hours = round(hours, 2)) %>%
  left_join(species, by = "spcode") %>%
  dplyr::select(-scientific_name)

time_wp <- time_wp[-c(242:247),]

pv_summary <- pv_summary %>%
  dplyr::select(-saturated_moisture_content) %>%
  mutate(swc = 100*swc,
         swc = round(swc, 2))

############################################################################
## Combining them all
############################################################################

burn_trials <- burn_trials %>%
  left_join(burn_trials_weather, by = "sample_id")

dim(burn_trials)

samples <- left_join(samples, species)

dim(samples)

alldata <- samples %>% full_join(wp_fmc) %>%
  full_join(all_leaf_traits) %>% full_join(burn_trials) %>%
  mutate(total_dry_mass = mass_pre*(twig_leaf_dry_mass/twig_leaf_fresh_mass)) %>%
  dplyr::select(- mass_pre, - twig_leaf_dry_mass, -twig_leaf_fresh_mass) %>%
  filter(!spcode %in% c("CELAR", "QUVI", "FOPU2", "SEBE2")) %>%
  mutate(across(c(notes, sex), ~ replace_na(., ""))) %>%
  mutate(spcode = ifelse(sex == "female" & spcode == "JUPI", paste0(spcode, "F"), spcode)) %>%
  dplyr::select(-species, -notes, -sex) %>%
  filter(! is.na(heat_release_j)) %>%
  filter(! is.na(vol_burned)) %>%
  filter(! is.na(wp)) %>%
  mutate(wp = ifelse(spcode == "DITE3" & wp == -7, -7.83, wp)) %>%
  mutate(wp = ifelse(spcode == "MATR3" & wp == -7, -8.06, wp)) %>%
  mutate(wp = ifelse(spcode == "PRGL2" & wp == -7, -8.24, wp)) 
  
dim(alldata) # 356

alldata_2024 <- alldata %>%
  filter(sample_id != "FT17") %>%
  filter(sample_id != "FT15") %>%
  filter(sample_id != "SXT19") %>%
  filter(year == 2024 & spcode != "JUPIF") %>%
  filter(wp != -8.24) # leaving out the mesquite samples which didn't rehydrate

######################################################################################
## Cleaning up work space, only keeping the alldata, pv_summary and time_wp
######################################################################################

rm("all_leaf_traits", "burn_trials",  "burn_trials_weather", "juniperus_leaf_area_2024",
   "juniperus_leaf_traits", "ldmc_leaf_length", "leaf_traits", "lma_leaf_area", "MASS_DISK_1",
   "MASS_DISK_2", "samples", "species", "SPECIFIC_HEAT_AL", "water_potentials_fmc", "wp_fmc")

