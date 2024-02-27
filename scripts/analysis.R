# This script is for conducting mixed effect model,
# and generating anova table 
################################################################################
# Initially water potential and fmc relationship
################################################################################

wp_fmc_anova_table_model <- lme4::lmer(fmc ~  wp + wp:spcode +
                                         (1|spcode), data = final_data)

anova(wp_fmc_anova_table_model)


wp_fmc__anova <- car::Anova(wp_fmc_anova_table_model, type = 3, 
                                           test.statistic = "F")

wp_fmc_xtable <-  xtable::xtable(wp_fmc__anova, digits = 3)

wp_fmc_anova_coefficients <- summary(wp_fmc_anova_table_model)$coefficients

wp_fmc_coeff <- xtable::xtable(wp_fmc_anova_coefficients, digits = 3)

wp_fmc <- afex::mixed(fmc ~ wp +
                        (1|spcode), data = final_data,
                      method = "KR", REML = TRUE)
summary(wp_fmc)

wp_fmc_kr_model <- afex::mixed(fmc ~ wp:spcode +
                                 (1|spcode), data = final_data,
                               method = "KR", REML = TRUE)


summary(wp_fmc_kr_model)

#############################################################################
# Does the relationship between ignition delay and water status 
# varies among species?
#########################################################################

fmc_wp_species_ignition <- lm(ignition_delay ~ fmc*wp*spcode, data = filter(final_data, self_ignition != 1))

summary(fmc_wp_species_ignition)

anova(fmc_wp_species_ignition)


###########################################################################
# # Does the relationship between temperature integration and water status 
# varies among species?
#############################################################################

fmc_wp_degsec <- lm(degsec_100 ~ fmc*wp*spcode, data = filtered_data)

anova(fmc_wp_degsec)

summary(fmc_wp_degsec)





