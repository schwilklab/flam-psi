## Summarising and stats
## D. Schwilk

library(tidyr)
library(broom)
library(purrr)
library(forcats)
library(lsmeans)


####################################################################################
# Next part is summarising data
####################################################################################

ajb_heat_release_data <- final_data %>%
  filter(year == 2024) %>%
  filter(spcode != "JUPIF") %>%
  filter(ignition_delay <= 60)

dim(ajb_heat_release_data)

ajb_ignition_delay_data <- final_data %>%
  filter(spcode != "JUPIF")

dim(ajb_ignition_delay_data)

####################################################################################
# Summarising leaf traits data
#####################################################################################

leaf_sum <- final_data %>%
  group_by(spcode) %>%
  summarize(across(c(lma, ldmc, leaf_area, leaf_area_per_leaflet), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(ldmc = round(ldmc, 2),
         lma = round(lma, 2),
         leaf_area = round(leaf_area, 2),
         leaf_area_per_leaflet = round(leaf_area_per_leaflet, 2))

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

#####################################################################################
# Flammability 2 dimensional and heat release dominate one in these data. But
# heat release and ignition
######################################################################################

high_fmc_sum <- ajb_heat_release_data %>%
  group_by(spcode, display_name) %>%
  summarize(across(c(ignition_delay, heat_release_j), mean)) %>%
  mutate(ignition_delay = round(ignition_delay, 2),
         heat_release_j = round(heat_release_j, 2))

high_fmc_sum

ig_heat_release_cor <- cor.test(high_fmc_sum$ignition_delay, high_fmc_sum$heat_release_j,
                                method = "spearman")

ig_heat_release_cor

#######################################################################################
# Combining these three
#######################################################################################

leaf_pv_high_fmc_sum <- left_join(leaf_sum, high_fmc_sum, by = "spcode") %>%
  left_join(pv_sum)

leaf_pv_high_fmc_sum
  
#######################################################################################
# This part is for slecting appropiate color for figures done by Dr. Schwilk,
# will move it to figures.R later
#######################################################################################

species_sorted <- high_fmc_sum %>% arrange(ignition_delay) %>% dplyr::select(display_name)
species_sorted <- unname(unlist(as.vector(species_sorted[,2])))

high_fmc_sum$display_name <- factor(high_fmc_sum$display_name, levels = species_sorted)
ajb_ignition_delay_data$display_name <- factor(ajb_ignition_delay_data$display_name, levels = species_sorted)
final_data$display_name <- factor(final_data$display_name, levels = species_sorted)

names(schwilkcolors) <- species_sorted
schwilkpalette <- scale_color_manual(name = "display_name", values=schwilkcolors)

#####################################################################################################
# water potential vs ignition_delay
####################################################################################################

species_wp_ign_sensitivity <- ajb_ignition_delay_data %>%
  nest(data = c(-spcode, -display_name)) %>%
  mutate(fit = map(data, ~ lm(ignition_delay ~ wp, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "wp") %>%
  dplyr::select(spcode, display_name, wp_ign_sens = estimate)

species_wp_ign_sensitivity

#####################################################################################################
# canopy moisture content vs ignition_delay
####################################################################################################

species_cmc_ign_sensitivity <- ajb_ignition_delay_data %>%
  nest(data = c(-spcode, -display_name)) %>%
  mutate(fit = map(data, ~ lm(ignition_delay ~ cmc, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "cmc") %>%
  dplyr::select(spcode, display_name, cmc_ign_sens = estimate)

species_cmc_ign_sensitivity

####################################################################################################
# Now heat release, initially wp vs heat release
####################################################################################################

species_wp_heat_release_sensitivity <- ajb_heat_release_data %>%
  nest(data = c(-spcode, -display_name)) %>%
  mutate(fit = map(data, ~ lm(heat_release_j ~ wp, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "wp") %>%
  dplyr::select(spcode, display_name, wp_heat_rlease_sens = estimate)

species_wp_heat_release_sensitivity 

####################################################################################################
# cmc vs heat release
####################################################################################################

species_cmc_heat_release_sensitivity <- ajb_heat_release_data %>%
  nest(data = c(-spcode, -display_name)) %>%
  mutate(fit = map(data, ~ lm(heat_release_j ~ cmc, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "cmc") %>%
  dplyr::select(spcode, display_name, cmc_heat_rlease_sens = estimate)

species_cmc_heat_release_sensitivity

######################################################################################################
# Now combining these four
######################################################################################################

species_ig_heat_release_sum <- left_join(species_wp_ign_sensitivity, species_cmc_ign_sensitivity, 
                                         by = c("spcode", "display_name")) %>%
  left_join(species_wp_heat_release_sensitivity,  by = c("spcode", "display_name")) %>%
  left_join(species_cmc_heat_release_sensitivity, by = c("spcode", "display_name"))
           
species_ig_heat_release_sum

#####################################################################################################
# Now combining all the summary
#####################################################################################################

species_sum <- left_join(species_ig_heat_release_sum, leaf_pv_high_fmc_sum)

############################################################################################################
# Now the models, does leaf traits and pv measurements predict ignition and heat release sensitivity
#########################################################################################################

leaf_wp_ig_mod <- lm(wp_ign_sens ~ lma + ldmc + leaf_area + leaf_area_per_leaflet, data = species_sum)

summary(leaf_wp_ig_mod)

anova(leaf_wp_ig_mod)


pv_wp_ig_mod <- lm(wp_ign_sens ~ tlp + rwc_tlp + capacitance_above_tlp + capacitance_below_tlp +
                  modulus_elasticity + swc + osmotic_potential, data = species_sum)

summary(pv_wp_ig_mod)

anova(pv_wp_ig_mod)


leaf_cmc_ig_mod <- lm(cmc_ign_sens ~ lma + ldmc + leaf_area + leaf_area_per_leaflet, data = species_sum)

summary(leaf_cmc_ig_mod)

anova(leaf_cmc_ig_mod)

pv_cmc_ig_mod <- lm(cmc_ign_sens ~ tlp + rwc_tlp + capacitance_above_tlp + capacitance_below_tlp +
                     modulus_elasticity + swc + osmotic_potential, data = species_sum)

summary(pv_cmc_ig_mod)

anova(pv_cmc_ig_mod)

##############################################################################################################
# Now heat release
#############################################################################################################

leaf_wp_heat_release_mod <- lm(wp_heat_rlease_sens ~ lma + ldmc + leaf_area + leaf_area_per_leaflet, data = species_sum)

summary(leaf_wp_heat_release_mod)

anova(leaf_wp_heat_release_mod)

pv_wp_heat_release_mod <- lm(wp_heat_rlease_sens ~ tlp + rwc_tlp + capacitance_above_tlp + capacitance_below_tlp +
                                 modulus_elasticity + swc + osmotic_potential, data = species_sum)

summary(pv_wp_heat_release_mod)

anova(pv_wp_heat_release_mod)



leaf_cmc_heat_release_mod <- lm(cmc_heat_rlease_sens ~ lma + ldmc + leaf_area + leaf_area_per_leaflet, data = species_sum)

summary(leaf_cmc_heat_release_mod)

anova(leaf_cmc_heat_release_mod)

pv_cmc_heat_release_mod <- lm(cmc_heat_rlease_sens ~ tlp + rwc_tlp + capacitance_above_tlp + capacitance_below_tlp +
                                 modulus_elasticity + swc + osmotic_potential, data = species_sum)

summary(pv_cmc_heat_release_mod)

anova(pv_cmc_heat_release_mod)


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

species_sum <- left_join(species_sum, species_wp_cmc)


##################################################################################################
# Dioes whole shoot capacitance can predict ignition sensitivity and heat release sensitivity to
# water status?
#################################################################################################

wp_ig_shoot_capacitance <- lm(wp_ign_sens ~ 1/wp_sens, data=species_sum)

summary(wp_ig_shoot_capacitance)

anova(wp_ig_shoot_capacitance)


cmc_ig_shoot_capacitance <- lm(cmc_ign_sens ~ 1/wp_sens, data=species_sum)

summary(cmc_ig_shoot_capacitance)

anova(cmc_ig_shoot_capacitance)


wp_heat_release_shoot_capacitance <- lm(wp_heat_rlease_sens ~ 1/wp_sens, data = species_sum)

summary(wp_heat_release_shoot_capacitance)

anova(wp_heat_release_shoot_capacitance)


cmc_heat_release_shoot_capacitance <- lm(cmc_heat_rlease_sens ~ 1/wp_sens, data=species_sum)

summary(cmc_heat_release_shoot_capacitance)

anova(cmc_heat_release_shoot_capacitance)


##############################################################################################################
# Now any leaf traits can predict pv measurements?
##############################################################################################################

leaf_swc <- lm(swc ~ lma + ldmc + leaf_area + leaf_area_per_leaflet, data = species_sum)

summary(leaf_swc)

anova(leaf_swc) # lma

leaf_tlp <- lm(tlp ~ lma + ldmc + leaf_area + leaf_area_per_leaflet, data = species_sum)

summary(leaf_tlp)

anova(leaf_tlp)

leaf_modulus_elasticity <- lm(modulus_elasticity ~ lma + ldmc + leaf_area + leaf_area_per_leaflet, data = species_sum)

summary(leaf_modulus_elasticity)

anova(leaf_modulus_elasticity)

leaf_rwc_tlp <- lm(rwc_tlp ~ lma + ldmc + leaf_area + leaf_area_per_leaflet, data = species_sum)

summary(leaf_rwc_tlp)

anova(leaf_rwc_tlp)

leaf_capacitance_above_tlp <- lm(capacitance_above_tlp ~ lma + ldmc + leaf_area + leaf_area_per_leaflet, data = species_sum)

summary(leaf_capacitance_above_tlp)

anova(leaf_capacitance_above_tlp)

leaf_capacitance_below_tlp <- lm(capacitance_below_tlp ~ lma + ldmc + leaf_area + leaf_area_per_leaflet, data = species_sum)

summary(leaf_capacitance_below_tlp)

anova(leaf_capacitance_below_tlp)


leaf_osmotic_potential <- lm(osmotic_potential ~ lma + ldmc + leaf_area + leaf_area_per_leaflet, data = species_sum)

summary(leaf_osmotic_potential)

anova(leaf_osmotic_potential)


