# This script is for conducting mixed effect model,
# and generating anova table 
################################################################################
# Initially water potential and leaf fuel moisture and canopy fuel moisture relationship
################################################################################

wp_lfmc_model <- lm(lfmc ~  wp*spcode, data = filter(final_data, year == 2024))

anova(wp_lfmc_model)

summary(wp_lfmc_model)

wp_cmc_model <- lm(cmc ~  wp*spcode, data = filter(final_data, year == 2024))

anova(wp_cmc_model)

summary(wp_cmc_model)


AIC(wp_lfmc_model, wp_cmc_model) # water potential is more tightly linked to moisture
# content of twig with leaf than just leaf moisture content!

#############################################################################
# Which one between water potential and cmc is better in predicting 
# shoot flammability, initially ignitibility?
#########################################################################

cmc_wp_species_ignition <- lm(log10(ignition_delay + 1) ~ cmc*wp*spcode, 
                              data = filter(final_data, year == 2024 & spcode != "JUPIF"))

anova(cmc_wp_species_ignition)

summary(cmc_wp_species_ignition)


wp_species_ignition <- lm(log10(ignition_delay + 1) ~ wp*spcode, 
                          data = filter(final_data, year == 2024 & spcode != "JUPIF"))

cmc_species_ignition <- lm(log10(ignition_delay + 1) ~ cmc*spcode, 
                           data = filter(final_data, year == 2024 & spcode != "JUPIF"))

species_ignition <- lm(log10(ignition_delay +1) ~ spcode, 
                       data = filter(final_data, year == 2024 & spcode != "JUPIF"))

cmc_wp_species_ignition_pre_temp <- lm(log10(ignition_delay +1) ~ cmc*wp*spcode + pre_burning_temp + wind_speed, 
                                   data = filter(final_data, year == 2024 & spcode != "JUPIF"))


AIC(cmc_wp_species_ignition, wp_species_ignition,
    cmc_species_ignition, species_ignition , cmc_wp_species_ignition_pre_temp) # interaction between wp*cmc*spcode

########################################################################
# How about both year
#######################################################################

cmc_wp_species_ignition_2324 <- lm(log10(ignition_delay + 1) ~ cmc*wp*spcode, 
                              data = filter(final_data, spcode != "JUPIF"))

anova(cmc_wp_species_ignition_2324)

summary(cmc_wp_species_ignition_2324)


wp_species_ignition_2324 <- lm(log10(ignition_delay + 1) ~ wp*spcode, 
                          data = filter(final_data, spcode != "JUPIF"))

cmc_species_ignition_2324 <- lm(log10(ignition_delay + 1) ~ cmc*spcode, 
                           data = filter(final_data, spcode != "JUPIF"))

species_ignition_2324 <- lm(log10(ignition_delay +1) ~ spcode, 
                       data = filter(final_data, spcode != "JUPIF"))


AIC(cmc_wp_species_ignition_2324, wp_species_ignition_2324,
    cmc_species_ignition_2324, species_ignition_2324) # Now wp

###########################################################################
# which one is better in predicting heat release
# between cmc and wp?
#############################################################################

short_ignition <- final_data %>%
  filter(ignition_delay <= 60)

cmc_wp_species_heat_release <- lm(heat_release_j ~ cmc*wp*spcode,
                            data = filter(short_ignition, year == 2024 & spcode != "JUPIF"))

anova(cmc_wp_species_heat_release)

summary(cmc_wp_species_heat_release)


wp_species_heat_release <- lm(heat_release_j ~ wp*spcode,
                              data = filter(short_ignition, year == 2024 & spcode != "JUPIF"))

cmc_species_heat_release <- lm(heat_release_j ~ cmc*spcode,
                         data = filter(short_ignition, year == 2024 & spcode != "JUPIF"))

species_heat_release <- lm(heat_release_j ~ spcode,
                     data = filter(short_ignition, year == 2024 & spcode != "JUPIF"))



AIC(cmc_wp_species_heat_release, wp_species_heat_release, cmc_species_heat_release,
    species_heat_release) # cmc


cmc_species__heat_release_pre_burn_temp <- lm(heat_release_j ~ cmc*spcode + pre_burning_temp + wind_speed,
                           data = filter(short_ignition, year == 2024 & spcode != "JUPIF"))


AIC(cmc_species_heat_release, cmc_species__heat_release_pre_burn_temp) # Nope, didn't improve


anova(cmc_species_heat_release) 


####################################################################################################################
# How about both year
#####################################################################################################################

cmc_wp_species_heat_release_2324 <- lm(heat_release_j ~ cmc*wp*spcode,
                                  data = filter(short_ignition, spcode != "JUPIF"))

anova(cmc_wp_species_heat_release_2324)

summary(cmc_wp_species_heat_release_2324)

##############################################################################
# which one is better in predicting temperature integration
# between fmc and wp?
##############################################################################

wp_species_heat_release_2324 <- lm(heat_release_j ~ wp*spcode,
                              data = filter(short_ignition, spcode != "JUPIF"))

cmc_species_heat_release_2324 <- lm(heat_release_j ~ cmc*spcode,
                               data = filter(short_ignition, spcode != "JUPIF"))

species_heat_release_2324 <- lm(heat_release_j ~ spcode,
                           data = filter(short_ignition, spcode != "JUPIF"))



AIC(cmc_wp_species_heat_release_2324, wp_species_heat_release_2324, cmc_species_heat_release_2324,
    species_heat_release_2324) # Interaction between cmc*wp*spcode


cmc_species__heat_release_pre_burn_temp_2324 <- lm(heat_release_j ~ wp*cmc*spcode + pre_burning_temp + wind_speed,
                                              data = filter(short_ignition, spcode != "JUPIF"))


AIC(cmc_wp_species_heat_release_2324, cmc_species__heat_release_pre_burn_temp_2324)


anova(cmc_wp_species_heat_release_2324)# cmc*wp*spcode

