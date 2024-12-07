## Stats
## Dr. Dylan Schwilk

library(tidyr)
library(broom)
library(purrr)
library(forcats)
library(lsmeans)

################################################################################
# Initially water potential and leaf fuel moisture and canopy fuel moisture relationship
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
# shoot flammability, initially ignitibility?
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

#short_ignition <- final_data %>%
  #filter(ignition_delay <= 60)

#dim(short_ignition) 

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
# Summarising leaf traits data
#####################################################################################

leaf_sum <- final_data %>%
  group_by(spcode) %>%
  summarize(across(c(lma, ldmc, leaf_length, leaf_area_per_leaflet), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(ldmc = round(ldmc, 2),
         lma = round(lma, 2),
         leaf_length = round(leaf_length, 2),
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

ignition_heat_release_sum <- final_data %>%
  filter(wp >= - 2) %>%
  group_by(spcode, display_name) %>%
  summarize(across(c(ignition_delay, heat_release_j), mean))

ignition_heat_release_sum

ig_heat_release_cor <- cor.test(ignition_heat_release_sum$ignition_delay, ignition_heat_release_sum$heat_release_j,
                                method = "spearman")
ig_heat_release_cor

#######################################################################################
# Combining these three
#######################################################################################

leaf_pv_high_fmc_sum <- left_join(leaf_sum, ignition_heat_release_sum, by = "spcode") %>%
  left_join(pv_sum)

leaf_pv_high_fmc_sum

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

species_sum <- left_join(species_ig_heat_release_sum, leaf_pv_high_fmc_sum)

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

species_sum <- left_join(species_sum, species_wp_cmc)

species_sum

###################################################################################################
# Shoot moisture loss rate
###################################################################################################

lm_fmc <- lm(fmc ~ hours*spcode, data = time_wp)

non_lm_fmc <- lm(log10(fmc) ~ hours*spcode, data = time_wp)

anova(non_lm_fmc)

AIC(lm_fmc, non_lm_fmc)

##################################################################################################
# lfmc loss rate calculation both linear and log
##################################################################################################

dry_down_sensitivity <- time_wp %>%
  nest(data = c(-spcode, -display_name)) %>%
  mutate(fit = map(data, ~ lm(fmc ~ hours, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "hours") %>%
  select(spcode, display_name, dry_down_rate = estimate)

dry_down_sensitivity <- dry_down_sensitivity %>%
  mutate(dry_down_rate = round(dry_down_rate, 3))

species_sum <- left_join(species_sum, dry_down_sensitivity)

dry_down_sensitivity_non_linear <- time_wp %>%
  nest(data = c(-spcode, -display_name)) %>%
  mutate(fit = map(data, ~ lm(log10(fmc) ~ hours, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "hours") %>%
  select(spcode, display_name, dry_down_rate_non_linear = estimate)

dry_down_sensitivity_non_linear <- dry_down_sensitivity_non_linear %>%
  mutate(dry_down_rate_non_linear = round(dry_down_rate_non_linear, 3))

species_sum <- left_join(species_sum, dry_down_sensitivity_non_linear)


######################################################################################################
# Correlation among leaf and hydraulic traits
######################################################################################################

cor_data <- species_sum[, -c(1:2)] %>%
  rename(lfmc_loss_rate = dry_down_rate_non_linear) %>%
  mutate(lfmc_loss_rate = -1*(lfmc_loss_rate)) %>%
  dplyr::select(-ignition_delay,
         - heat_release_j, -wp_ign_sens, -cmc_heat_rlease_sens, - dry_down_rate)

traits_cor <- cor(cor_data, method = "kendall",
                                use = "pairwise")

#traits_cor <- as.data.frame(traits_cor)

############################################################################################################
# Now the models, does leaf traits and pv measurements predict ignition sensitivity
#########################################################################################################

leaf_wp_ig_mod <- lm(wp_ign_sens ~ ldmc*leaf_length*leaf_area_per_leaflet, data = species_sum)

summary(leaf_wp_ig_mod)

anova(leaf_wp_ig_mod)

swc_ig <- lm(wp_ign_sens ~ swc, data = species_sum)

summary(swc_ig)

anova(swc_ig)

pv_wp_ig_mod <- lm(wp_ign_sens ~ capacitance_above_tlp + capacitance_below_tlp, data = species_sum)

summary(pv_wp_ig_mod)

anova(pv_wp_ig_mod)



##############################################################################################################
# Now heat release
#############################################################################################################

leaf_cmc_heat_release_mod <- lm(cmc_heat_rlease_sens ~ ldmc*leaf_length*leaf_area_per_leaflet, data = species_sum)

summary(leaf_cmc_heat_release_mod)

anova(leaf_cmc_heat_release_mod)

swc_heat <- lm(cmc_heat_rlease_sens ~ swc, data=species_sum)

summary(swc_heat)

anova(swc_heat)

pv_cmc_heat_release_mod <- lm(cmc_heat_rlease_sens ~ capacitance_above_tlp + capacitance_below_tlp, data = species_sum)

summary(pv_cmc_heat_release_mod)

anova(pv_cmc_heat_release_mod)


##############################################################################################
# Does shootcapacitance, lfmc loss rate predicts the ignititibility sensitivity?
##############################################################################################

wp_ig_shoot_capacitance <- lm(wp_ign_sens ~ wp_sens, data=species_sum)

summary(wp_ig_shoot_capacitance)

anova(wp_ig_shoot_capacitance)

wp_ig_shoot_capacitance_withoutj <- lm(wp_ign_sens ~ wp_sens, data = filter(species_sum, ! spcode %in% c("JUPI", "JUAS")))

summary(wp_ig_shoot_capacitance_withoutj)

anova(wp_ig_shoot_capacitance_withoutj)

dry_down_ig <- lm(wp_ign_sens ~ dry_down_rate, data=species_sum)

summary(dry_down_ig)

anova(dry_down_ig)

dry_down_ig_withoutj <- lm(wp_ign_sens ~ dry_down_rate, data = filter(species_sum, ! spcode %in% c("JUPI", "JUAS")))

summary(dry_down_ig_withoutj)

anova(dry_down_ig_withoutj)

dry_down_ig_non <- lm(wp_ign_sens ~ dry_down_rate_non_linear, data=species_sum)

summary(dry_down_ig_non)

anova(dry_down_ig_non)

dry_down_ig_withoutj_non <- lm(wp_ign_sens ~ dry_down_rate_non_linear, data = filter(species_sum, ! spcode %in% c("JUPI", "JUAS")))

summary(dry_down_ig_withoutj_non)

anova(dry_down_ig_withoutj_non)

#############################################################################################
# Now heat release
############################################################################################

cmc_heat_release_shoot_capacitance <- lm(cmc_heat_rlease_sens ~ wp_sens, data=species_sum)

summary(cmc_heat_release_shoot_capacitance)

anova(cmc_heat_release_shoot_capacitance)

cmc_heat_release_shoot_capacitance_withoutj <- lm(cmc_heat_rlease_sens ~ wp_sens, 
                                                  data = filter(species_sum, ! spcode %in% c("JUPI", "JUAS")))

summary(cmc_heat_release_shoot_capacitance_withoutj)

anova(cmc_heat_release_shoot_capacitance_withoutj)

dry_down_heat <- lm(cmc_heat_rlease_sens ~ dry_down_rate, data = species_sum)

summary(dry_down_heat)

anova(dry_down_heat)

dry_down_heat_withoutj <- lm(cmc_heat_rlease_sens ~ dry_down_rate, 
                             data = filter(species_sum, ! spcode %in% c("JUPI", "JUAS")))

summary(dry_down_heat_withoutj)

anova(dry_down_heat_withoutj)

dry_down_heat_non <- lm(cmc_heat_rlease_sens ~ dry_down_rate_non_linear, data = species_sum)

summary(dry_down_heat_non)

anova(dry_down_heat_non)

dry_down_heat_withoutj_non <- lm(cmc_heat_rlease_sens ~ dry_down_rate_non_linear, 
                                 data = filter(species_sum, ! spcode %in% c("JUPI", "JUAS")))

summary(dry_down_heat_withoutj_non)

anova(dry_down_heat_withoutj_non)


##############################################################################################
# Models comparison
#############################################################################################

AICc_calculation <- function(model) {
  n <- length(model$residuals)  
  k <- length(coef(model))    
  aic <- AIC(model)
  aicc <- aic + (2 * k * (k + 1)) / (n - k - 1)
  return(aicc)}


model_stats <- function(..., digits = 3) {
  models <- list(...)
  model_names <- sapply(substitute(list(...))[-1], deparse) 
  
  results <- data.frame(
    Model = character(),
    R2_adj = numeric(),
    logLik = numeric(),
    AICc = numeric(),
    stringsAsFactors = FALSE,
    row.names = NULL)
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    R2_adj <- round(summary(model)$adj.r.squared, digits)
    logLik <- round(logLik(model), digits)
    AICc <- round(AICc_calculation(model), digits)
    results <- rbind(results, data.frame(
      Model = model_names[i], 
      R2_adj = R2_adj,
      logLik = logLik,
      AICc = AICc
    ))}
  
  min_aicc <- min(results$AICc)
  results$Delta_AICc <- round(results$AICc - min_aicc, digits)
  weights <- exp(-0.5 * results$Delta_AICc)
  results$Weight <- round(weights / sum(weights), digits)
  rownames(results) <- NULL
  return(results)
}


models_likelihood_and_stuff_for_ignition <- model_stats(wp_species_ignition, lfmc_species_ignition, 
                                                        cmc_species_ignition)

xtable_ignition <- xtable::xtable(models_likelihood_and_stuff_for_ignition)

print(xtable_ignition, type = "html",
      file = "./results/ignition_models_comparisons.html")

models_likelihood_and_stuff_for_heat_release <- model_stats(cmc_species_heat_release, lfmc_species_heat_release,
                                                            wp_species_heat_release)

xtable_heat_release <- xtable::xtable(models_likelihood_and_stuff_for_heat_release)

print(xtable_heat_release, type = "html", 
      file = "./results/heat_release_models_comparisons.html")



