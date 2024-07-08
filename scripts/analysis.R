# This script is for conducting mixed effect model,
# and generating anova table 
################################################################################
# Initially water potential and fmc relationship
################################################################################

wp_fmc_model <- lm(fmc ~  wp*spcode, data = final_data)

anova(wp_fmc_model)

summary(wp_fmc_model)

#############################################################################
# Does the relationship between ignition delay and water status 
# varies among species? 
#########################################################################

fmc_wp_species_ignition <- lm(ignition_delay ~ fmc*wp*spcode, data = final_data)

anova(fmc_wp_species_ignition)

summary(fmc_wp_species_ignition)

############################################################################
# Samples without self ignition
##########################################################################

fmc_wp_species_ignition_withoutselfig <- lm(ignition_delay ~ fmc*wp*spcode, 
                                            data = filter(final_data, self_ignition != 1))

anova(fmc_wp_species_ignition_withoutselfig)

summary(fmc_wp_species_ignition_withoutselfig)

##############################################################################
# which one is better in predicting ignitibility
# between fmc and wp?
##############################################################################

wp_species_ignition <- lm(ignition_delay ~ wp*spcode, data = filter(final_data, self_ignition != 1))

fmc_species_ignition <- lm(ignition_delay ~ fmc*spcode, data = filter(final_data, self_ignition != 1))

species_ignition <- lm(ignition_delay ~ spcode, data = filter(final_data, self_ignition != 1))

wp_species_ignition_pre_temp <- lm(ignition_delay ~ wp*spcode + pre_burning_temp + wind_speed, data = filter(final_data, self_ignition != 1))

#wp_ignition <- lm(ignition_delay ~ wp, data = filter(final_data, self_ignition != 1))

#fmc_ignition <- lm(ignition_delay ~ fmc, data = filter(final_data, self_ignition != 1))

AIC(fmc_wp_species_ignition_withoutselfig, wp_species_ignition,
    fmc_species_ignition, species_ignition, wp_species_ignition_pre_temp) # wp

###########################################################################
# # Does the relationship between temperature integration and water status 
# varies among species?
#############################################################################

fmc_wp_species_degsec <- lm(degsec_100 ~ fmc*wp*spcode, data = filtered_data)

anova(fmc_wp_species_degsec)

summary(fmc_wp_species_degsec)

##############################################################################
# which one is better in predicting temperature integration
# between fmc and wp?
##############################################################################

wp_species_degsec <- lm(degsec_100 ~ wp*spcode, data = filtered_data)

fmc_species_degsec <- lm(degsec_100 ~ fmc*spcode, data = filtered_data)

species_degsec <- lm(degsec_100 ~ spcode, data = filtered_data)

wp_species_degsec_pre_temp <- lm(degsec_100 ~ wp*spcode + pre_burning_temp + wind_speed, data = filtered_data)

#wp_degsec <- lm(degsec_100 ~ wp, data = filtered_data)

#fmc_degsec <- lm(degsec_100 ~ fmc, data = filtered_data)

AIC(fmc_wp_species_degsec, species_degsec, wp_species_degsec, fmc_species_degsec, wp_species_degsec_pre_temp) # species

