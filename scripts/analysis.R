## Stats
## Dr. Dylan Schwilk
# Azaj Mahmud

library(tidyr)
library(broom)
library(purrr)
library(forcats)
library(lsmeans)

################################################################################
# Initially water potential and fuel moisture relationship
################################################################################

dim(final_data)

wp_lfmc_model <- lm(lfmc ~ wp*spcode, data = final_data)

anova(wp_lfmc_model)

summary(wp_lfmc_model)

wp_cmc_model <- lm(cmc ~  wp*spcode, data = final_data)

anova(wp_cmc_model)

summary(wp_cmc_model)

wp_cmc_ldmc_model <- lm(cmc ~  wp*spcode*ldmc, data = final_data)

summary(wp_cmc_ldmc_model)

anova(wp_cmc_ldmc_model)

#############################################################################
# Which one between water potential and fmc is better in predicting 
# shoot flammability, initially ignitibility.
#########################################################################

wp_species_ignition <- lm(ignition_delay ~ wp*spcode + pre_burning_temp + wind_speed, 
                          data = final_data)

cmc_species_ignition <- lm(ignition_delay ~ cmc*spcode + pre_burning_temp + wind_speed, 
                           data = final_data)

lfmc_species_ignition <- lm(ignition_delay ~ lfmc*spcode + pre_burning_temp + wind_speed, 
                           data = final_data)

AIC(wp_species_ignition,
    cmc_species_ignition, lfmc_species_ignition) 

anova(wp_species_ignition)

anova(cmc_species_ignition)

anova(lfmc_species_ignition)

###########################################################################
# which one is better in predicting heat release
# between fmc and wp?
#############################################################################

wp_species_heat_release <- lm(heat_release_j/1000 ~ wp*spcode +  pre_burning_temp + wind_speed,
                              data = final_data)

cmc_species_heat_release <- lm(heat_release_j/1000 ~ cmc*spcode +  pre_burning_temp + wind_speed,
                         data = final_data)

lfmc_species_heat_release <- lm(heat_release_j/1000 ~ lfmc*spcode +  pre_burning_temp + wind_speed,
                               data = final_data)

AIC(wp_species_heat_release, cmc_species_heat_release, lfmc_species_heat_release) 

anova(cmc_species_heat_release)
anova(wp_species_heat_release)
anova(lfmc_species_heat_release)

####################################################################################
# Next part is summarising data
# Summarising leaf traits data, total dry mass also included
#####################################################################################

leaf_sum <- final_data %>%
  group_by(spcode) %>%
  summarize(across(c(lma, ldmc, leaf_length, leaf_area_per_leaflet, total_dry_mass), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(ldmc = round(ldmc, 2),
         lma = round(lma, 2),
         leaf_length = round(leaf_length, 2),
         leaf_area_per_leaflet = round(leaf_area_per_leaflet, 2),
         total_dry_mass = round(total_dry_mass, 2))

leaf_sum

######################################################################################
# Summarising pressure volume data
#######################################################################################

pv_sum <- pv_summary %>%
  group_by(spcode) %>%
  summarize(across(c(tlp, rwc_tlp, capacitance_above_tlp,
                     capacitance_below_tlp, modulus_elasticity, swc, osmotic_potential), mean)) %>%
  mutate(tlp = round(tlp, 2),
         rwc_tlp = round(rwc_tlp, 2),
         capacitance_above_tlp = round(capacitance_above_tlp, 2),
         capacitance_below_tlp = round(capacitance_below_tlp, 2),
         modulus_elasticity = round(modulus_elasticity, 2),
         swc = round(swc, 2),
         osmotic_potential = round(osmotic_potential, 2))
pv_sum

#######################################################################################
# Combining these three
#######################################################################################

leaf_pv_sum <- left_join(leaf_sum, pv_sum, by = "spcode") 

#####################################################################################################
# water potential vs ignition_delay
####################################################################################################

species_wp_ign_sensitivity <- final_data %>%
  nest(data = c(-spcode, -display_name)) %>%
  mutate(fit = map(data, ~ lm(ignition_delay ~ wp, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "wp") %>%
  dplyr::select(spcode, display_name, wp_ign_sens = estimate)

species_wp_ign_sensitivity <- species_wp_ign_sensitivity %>%
  mutate(wp_ign_sens = round(wp_ign_sens, 2))

####################################################################################################
# cmc vs heat release
####################################################################################################

species_cmc_heat_release_sensitivity <- final_data %>%
  nest(data = c(-spcode, -display_name)) %>%
  mutate(fit = map(data, ~ lm(heat_release_j/1000 ~ cmc, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "cmc") %>%
  dplyr::select(spcode, display_name, cmc_heat_rlease_sens = estimate)

species_cmc_heat_release_sensitivity <- species_cmc_heat_release_sensitivity %>%
  mutate(cmc_heat_rlease_sens = round(cmc_heat_rlease_sens, 2))

######################################################################################################
# Now combining these four
######################################################################################################

species_ig_heat_release_sum <- left_join(species_wp_ign_sensitivity, 
                                         species_cmc_heat_release_sensitivity, by = c("spcode", "display_name"))

species_ig_heat_release_sum

#####################################################################################################
# Now combining all the summary
#####################################################################################################

species_sum <- left_join(species_ig_heat_release_sum, leaf_pv_sum)

species_sum

################################################################################################
## Calculation of whole shoot "capacitance"
## Sensitivity of fuel moisture to wp
################################################################################################

species_wp_cmc <- final_data %>%
  nest(data = -spcode) %>%
  mutate(fit = map(data, ~ lm(cmc ~ wp, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "wp") %>%
  dplyr::select(spcode, wp_sens = estimate)

  
species_sum <- left_join(species_sum, species_wp_cmc) %>%
  mutate(wp_sens = round(wp_sens, 2))

species_sum

##################################################################################################
# lfmc loss rate calculation 
##################################################################################################

dry_down_sensitivity <- time_wp %>%
  nest(data = c(-spcode, -display_name)) %>%
  mutate(fit = map(data, ~ lm(fmc ~ hours, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "hours") %>%
  select(spcode, display_name, dry_down_rate = estimate)

species_sum <- left_join(species_sum, dry_down_sensitivity) %>%
  mutate(dry_down_rate = round(dry_down_rate, 3))

######################################################################################################
# Correlation among leaf and hydraulic traits
######################################################################################################

cor_data <- species_sum[, -c(1:2)] %>%
  rename(lfmc_loss_rate = dry_down_rate) %>%
  mutate(lfmc_loss_rate = -1*(lfmc_loss_rate)) %>%
  rename(ignitibility_sense = wp_ign_sens) %>%
  rename(heat_release_sense = cmc_heat_rlease_sens) %>%
  rename(leaf_capacitance = capacitance_above_tlp)

traits_cor <- cor(cor_data, method = "kendall",
                                use = "pairwise")

##############################################################################################
# Does shootcapacitance, lfmc loss rate predicts the ignititibility sensitivity?
##############################################################################################

wp_ig_shoot_capacitance <- lm(wp_ign_sens ~ wp_sens, data=species_sum)

summary(wp_ig_shoot_capacitance)

anova(wp_ig_shoot_capacitance)

dry_down_ig <- lm(wp_ign_sens ~ dry_down_rate, data=species_sum)

summary(dry_down_ig)

anova(dry_down_ig)

#############################################################################################
# Now heat release
############################################################################################

cmc_heat_release_shoot_capacitance <- lm(cmc_heat_rlease_sens ~ wp_sens, data=species_sum)

summary(cmc_heat_release_shoot_capacitance)

anova(cmc_heat_release_shoot_capacitance)

dry_down_heat <- lm(cmc_heat_rlease_sens ~ dry_down_rate, data = species_sum)

summary(dry_down_heat)

anova(dry_down_heat)

############################################################################################################
# Now the models, does leaf traits and pv measurements predict flammability sensitivity?
#########################################################################################################

swc_ig <- lm(wp_ign_sens ~ swc, data = species_sum)

summary(swc_ig)

anova(swc_ig)

leaf_capacitance_ig_mod <- lm(wp_ign_sens ~ capacitance_above_tlp, data = species_sum)

summary(leaf_capacitance_ig_mod)

anova(leaf_capacitance_ig_mod)

swc_heat <- lm(cmc_heat_rlease_sens ~ swc, data=species_sum)

summary(swc_heat)

anova(swc_heat)

leaf_capacitance_heat_release_mod <- lm(cmc_heat_rlease_sens ~ capacitance_above_tlp, data = species_sum)

summary(leaf_capacitance_heat_release_mod)

anova(leaf_capacitance_heat_release_mod)

#############################################################################################
# Now leaf traits
#############################################################################################

ldmc_ig <- lm(wp_ign_sens ~ ldmc, data = species_sum)
summary(ldmc_ig)
anova(ldmc_ig)

leaf_length_ig <- lm(wp_ign_sens ~ leaf_length, data = species_sum)
summary(leaf_length_ig)
anova(leaf_length_ig)

ldmc_heat <- lm(cmc_heat_rlease_sens ~ ldmc, data = species_sum)
summary(ldmc_heat)
anova(ldmc_heat)

leaf_length_heat <- lm(cmc_heat_rlease_sens ~ leaf_length, data = species_sum)
summary(leaf_length_heat)
anova(leaf_length_heat)

