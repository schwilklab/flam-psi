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
## Read all the data files for 2023
###############################################################################

species <- read_csv("./data/2023/species.csv")

samples <-  read_csv("./data/2023/samples.csv") 

water_potentials <- read_csv("./data/2023/water_potentials.csv") 

fmc <- read_csv("./data/2023/fmc.csv")

burn_trials <- read_csv("./data/2023/burn_trials.csv")

burn_trials_wx <- read_csv("./data/2023/burn_trials_wx.csv")


###############################################################################
## Cleaning the data 2023
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

burn_trials <- read_csv("./data/2023/burn_trials.csv") %>%
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

##########################################################################
# Converting spcode as factor and removing one outlier for JUPI because
# this sample is showing exceptionally high fmc
########################################################################

alldata$spcode <- as.factor(alldata$spcode)

alldata <- alldata %>%
   filter(sample_id != "JDK31") 

dim(alldata)

################################################################################
# Initially creating two subset of final_data because we might need them later
# Initially samples without self ignition and then samples which ignited 
# within 20 seconds
###############################################################################

without_self_ignition <- alldata %>%
  filter(ignition_delay != 0)

dim(without_self_ignition) 

filtered_data <- without_self_ignition %>%
  filter(ignition_delay <= 20)

dim(filtered_data)

filtered_sample_id <- filtered_data$sample_id

alldata$ignition_delay <- log(alldata$ignition_delay + 1)

without_self_ignition <- without_self_ignition %>%
  mutate(ignition_delay = log(ignition_delay + 1))



###############################################################################
## Read all the data files for 2024
###############################################################################

species_2024 <- read_csv("./data/2024/species.csv")

samples_2024 <-  read_csv("./data/2024/samples.csv") 

water_potentials_fmc <- read_csv("./data/2024/wp_fmc.csv") 

leaf_traits <- read_csv("./data/2024/leaf_traits.csv")

juniperus_leaf_traits <- read_csv("./data/2024/juniperus_lma.csv")

burn_trials_2024 <- read_csv("./data/2024/burn_trials.csv")

burn_trials_weather_2024 <- read_csv("./data/2024/burn_trials_weather.csv")

######################################################################################
#  Data cleaning 2024
#####################################################################################

species_2024 <- species_2024 %>%
  mutate(display_name = paste(substr(genus, 1,1), ". ", specific_epithet, sep="")) %>%
  dplyr::select(spcode, display_name, scientific_name)

wp_fmc <- water_potentials_fmc %>%
  mutate(wp = -wp) %>%
  mutate(lfmc = ((leaf_fresh_mass - leaf_dry_mass)/leaf_dry_mass)*100) %>%
  mutate(lfmc = round(lfmc, 2)) %>%
  mutate(cmc = ((twig_leaf_fresh_mass - twig_leaf_dry_mass)/twig_leaf_dry_mass)*100) %>%
  mutate(cmc = round(cmc, 2)) %>%
  dplyr::select(sample_id, wp, lfmc, cmc, twig_leaf_fresh_mass, twig_leaf_dry_mass)

burn_trials_weather_2024 <- burn_trials_weather_2024 %>% 
  dplyr::select(sample_id, rh, temp, wind_speed)

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


lma_leaf_area <- leaf_traits %>%
  filter(! spcode %in% c("JUPI", "JUAS")) %>%
  mutate(leaf_area_per_leaflet = leaf_area/5) %>%
  mutate(leaf_area_per_leaflet = round(leaf_area_per_leaflet, 2)) %>%
  dplyr::select(sample_id, leaf_area, leaf_area_per_leaflet) %>%
  rbind(juniperus_leaf_area_2024)
  

all_leaf_traits <- leaf_traits %>%
  dplyr::select(sample_id, lma_dry) %>%
  right_join(ldmc_leaf_length, by = "sample_id") %>%
  right_join(lma_leaf_area, by = "sample_id") %>%
  mutate(lma = lma_dry/leaf_area) %>%
  mutate(lma = round(lma, 3)) %>%
  dplyr::select(- lma_dry)


burn_trials_2024 <- burn_trials_2024 %>%
  mutate(heat1 = (max_temp - disc1_pre) * MASS_DISK_1 * SPECIFIC_HEAT_AL,
         heat2 = (max_temp - disc2_pre) * MASS_DISK_2 * SPECIFIC_HEAT_AL,
         heat_release_j = (heat1 + heat2)/2, # average heat release of two disks
         pre_burning_temp = (disc1_pre + disc2_pre)/2) %>%
  mutate(massloss = (mass_pre - mass_post)/mass_pre) %>%
  dplyr::select(sample_id, pre_combustion, ignition_delay, flame_duration,
                flame_height, heat_release_j, vol_burned, massloss,
                self_ignition, pre_burning_temp, mass_pre)

# Correct heat release to set lowest value at 0 (all relative anyway)
burn_trials_2024$heat_release_j <- burn_trials_2024$heat_release_j - min(burn_trials_2024$heat_release_j, na.rm=TRUE)

alldata_2024 <- left_join(samples_2024, species_2024) %>% left_join(wp_fmc) %>%
  left_join(all_leaf_traits) %>% left_join(burn_trials_2024) %>%
  mutate(total_dry_mass = mass_pre*(twig_leaf_dry_mass/twig_leaf_fresh_mass)) %>%
  dplyr::select(- mass_pre, - twig_leaf_dry_mass, -twig_leaf_fresh_mass)

#View(alldata_2024)

alldata_2024 <- alldata_2024 %>%
  filter(!spcode %in% c("CELAR", "QUVI", "FOPU2", "SEBE2")) %>%
  filter(! sample_id %in% c("ST38", "ST39")) %>%
  mutate(across(c(notes, sex), ~ replace_na(., ""))) %>%
  mutate(spcode = ifelse(sex == "female", paste0(spcode, "F"), spcode)) %>%
  dplyr::select(-species)


#View(alldata_2024)

######################################################################################
## Cleaning up work space, only keeping the alldata
######################################################################################

rm("burn_trials", "burn_trials_wx", "fmc", "MASS_DISK_1", "MASS_DISK_2", "samples",
   "species", "SPECIFIC_HEAT_AL", "water_potentials", "species_2024", "burn_trials_2024",
   "all_leaf_traits", "lma_leaf_area", "juniperus_leaf_area_2024", "ldmc_leaf_length",
   "burn_trials_weather_2024", "wp_fmc", "juniperus_leaf_traits", "leaf_traits",
   "samples_2024", "water_potentials_fmc")
