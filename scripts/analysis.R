# This script is for conducting mixed effect model,
# and generating anova table 

################################################################################
# Initially water potential and fmc relationship
################################################################################

wp_fmc_anova_table_model <- lme4::lmer(fmc ~  wp*spcode +
                                         (1|spcode), data = final_data)

anova(wp_fmc_anova_table_model)

wp_fmc_kr_model <- afex::mixed(fmc ~  wp:spcode +
                              (1|spcode), data = final_data,
                            method = "KR", REML = TRUE)


summary(wp_fmc_kr_model)





wp_fmc__anova <- car::Anova(wp_fmc_anova_table_model, type = 2, 
                                           test.statistic = "F")
wp_fmc_xtable <-  xtable::xtable(wp_fmc__anova, digits = 3)
wp_fmc_anova_coefficients <- summary(wp_fmc_anova_table_model)$coefficients
wp_fmc_coeff <- xtable::xtable(wp_fmc_anova_coefficients, digits = 3)


#############################################################################
# Now fmc and ignition delay
############################################################################

fmc_ignition_table_model <- lme4::lmer(ignition_delay ~ fmc*spcode +
                                         (1|spcode), data = final_data)

fmc__ignition_anova <- car::Anova(fmc_ignition_table_model, type = 2, 
                            test.statistic = "F")
fmc_ignition_xtable <-  xtable::xtable(fmc__ignition_anova, digits = 3)
fmc_ignition_anova_coefficients <- summary(fmc_ignition_table_model)$coefficients
fmc_ignition_coeff <- xtable::xtable(fmc_ignition_anova_coefficients, digits = 3)


fmc_ignition_kr_model <- afex::mixed(ignition_delay ~ fmc*spcode  +
                                       (1|spcode), data = final_data,
                                     method = "KR", REML = TRUE)


summary(fmc_ignition_kr_model)

##############################################################################
# Without samples those had self ignition
##############################################################################

without_self_ignition <- final_data %>%
  filter(ignition_delay != 0)

dim(without_self_ignition)

fmc_withoutself_ignition_table_model <- lme4::lmer(ignition_delay ~ fmc*spcode +
                                         (1|spcode), data = without_self_ignition)

fmc__withoutself_ignition_anova <- car::Anova(fmc_withoutself_ignition_table_model, type = 2, 
                                  test.statistic = "F")

fmc_withoutself_ignition_xtable <-  xtable::xtable(fmc__withoutself_ignition_anova, digits = 3)
fmc_withoutself_ignition_anova_coefficients <- summary(fmc_withoutself_ignition_table_model)$coefficients
fmc_withoutself_ignition_coeff <- xtable::xtable(fmc_withoutself_ignition_anova_coefficients, digits = 3)


fmc_withoutself_ignition_kr_model <- afex::mixed(ignition_delay ~ fmc*spcode  +
                                       (1|spcode), data = without_self_ignition,
                                     method = "KR", REML = TRUE)


summary(fmc_withoutself_ignition_kr_model)


##################################################################################
# Now fmc vs temperature integration since it had the highest
# loading in PC1 for simples which ignited within 20 seconds!
##################################################################################

filtered_data <- final_data %>%
  filter(ignition_delay <= 20)

fmc_heat_release_table_model <- lme4::lmer(degsec_100 ~ fmc*spcode +
                                         (1|spcode), data = final_data)

fmc__heat_release_anova <- car::Anova(fmc_heat_release_table_model, type = 2, 
                            test.statistic = "F")
fmc_heat_release_xtable <-  xtable::xtable(fmc__heat_release_anova, digits = 3)
fmc_heat_release_anova_coefficients <- summary(fmc_heat_release_table_model)$coefficients
fmc_heat_release_coeff <- xtable::xtable(fmc_heat_release_anova_coefficients, digits = 3)



fmc_heat_release_kr_model <- afex::mixed(degsec_100 ~ fmc*spcode +
                                       (1|spcode), data = filtered_data,
                                     method = "KR", REML = TRUE)



summary(fmc_heat_release_kr_model) # No effect


