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
# Which one is better in predicting flammability between fmc and wp?
# Ignition delay and then temperature integration
# data without self ignition
#########################################################################

fmc_ignition_model_withoutselfig <- lme4::lmer(ignition_delay ~ fmc +
                                   (1|spcode), data = without_self_ignition)


wp_ignition_model_withoutselfig <- lme4::lmer(ignition_delay ~ wp +
                                  (1|spcode), data = without_self_ignition)

AIC(fmc_ignition_model_withoutselfig, wp_ignition_model_withoutselfig)

###########################################################################
# Now for temperature integration for samples those ignited within tweenty seconds
#############################################################################

fmc_degsec_model_within20s <- lme4::lmer(degsec_100 ~ fmc +
                                 (1|spcode), data = filtered_data)


wp_degsec_model_within20se <- lme4::lmer(degsec_100 ~ wp +
                                (1|spcode), data = filtered_data)


AIC(fmc_degsec_model_within20s, wp_degsec_model_within20se) # wp is bteer than fmc

#############################################################################
# Now wp and ignition delay, including wind speed and pre-combustion temperature,
# for samples those hadn't self ignition
##############################################################################

wp_withoutself_ignition_table_model <- lme4::lmer(ignition_delay ~ wp + wp:spcode +
                                                    wind_speed + pre_burning_temp +
                                         (1|spcode), data = without_self_ignition)

wp__withoutself_ignition_anova <- car::Anova(wp_withoutself_ignition_table_model, 
                                              type = 3, test.statistic = "F")
wp__withoutself_ignition_anova

wp_withoutself_ignition_xtable <-  xtable::xtable(wp__withoutself_ignition_anova, digits = 3)

wp_withoutself_ignition_anova_coefficients <- summary(wp_withoutself_ignition_table_model)$coefficients

wp_withoutself_ignition_coeff <- xtable::xtable(wp_withoutself_ignition_anova_coefficients, digits = 3)

wp_without_selig <- afex::mixed(ignition_delay ~  wp +
                                  (1|spcode), data = without_self_ignition,
                                method = "KR", REML = TRUE)

summary(wp_without_selig)


wp_withoutself_ignition_kr_model <- afex::mixed(ignition_delay ~  wp:spcode  +
                                       (1|spcode), data = without_self_ignition,
                                     method = "KR", REML = TRUE)


summary(wp_withoutself_ignition_kr_model)


##################################################################################
# Now wp vs temperature integration since it had the highest
# loading in PC1, We will use filtered_data, the wind speed and
# precombustion temperature were added as the covaraites
##################################################################################

wp_heat_release_table_model <- lme4::lmer(degsec_100 ~ wp + wp:spcode +
                                            wind_speed + pre_burning_temp + 
                                         (1|spcode), data = filtered_data)

anova(wp_heat_release_table_model) 

wp__heat_release_anova <- car::Anova(wp_heat_release_table_model, type = 3, 
                            test.statistic = "F")

wp__heat_release_anova # No effect

wp_heat_release_xtable <-  xtable::xtable(wp__heat_release_anova, digits = 3)

wp_heat_release_anova_coefficients <- summary(wp_heat_release_table_model)$coefficients

wp_heat_release_coeff <- xtable::xtable(wp_heat_release_anova_coefficients, digits = 3)

wp_heat_release_kr_model <- afex::mixed(degsec_100 ~  wp:spcode +
                                       (1|spcode), data = filtered_data,
                                     method = "KR", REML = TRUE)



summary(wp_heat_release_kr_model) 


