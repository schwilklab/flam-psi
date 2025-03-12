## figures.R

# This R script is used to create figures, anova and coefficient tables

library(xtable)
library(patchwork)

###################################################################################
# From colorful to black
###################################################################################
species_sorted <- species_sum %>% arrange(wp_sens) %>% dplyr::select(display_name)
species_sorted <- unname(unlist(as.vector(species_sorted[,1])))
final_data$display_name <- factor(final_data$display_name, levels = species_sorted)
names(schwilkcolors) <- species_sorted
species_sorted_wp_ig <- species_sum %>% arrange(desc(wp_ign_sens)) %>% dplyr::select(display_name)
species_sorted_wp_ig <- unname(unlist(as.vector(species_sorted_wp_ig[,1])))
final_data$display_name <- factor(final_data$display_name, levels = species_sorted_wp_ig)
schwilkcolors_wp_ig <- schwilkcolors
names(schwilkcolors_wp_ig) <- species_sorted_wp_ig

###########################################################################
# Intially water potential vs cmc plot
##########################################################################

wp_cmc_plot <- ggplot(final_data, aes(wp, cmc, color=display_name)) +
  geom_smooth(method = "lm", se = FALSE) +
  dws_point +
  xlab("Water potential (MPa)") +
  ylab("LFMC (%)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) + 
  pubtheme +
  theme(legend.text = element_text(face="italic"),
        legend.title = element_text(size = textsize, face = "bold"),
        legend.position = c(0.25, 0.85),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

ggsave("./results/figure1.pdf", plot = wp_cmc_plot,
       height = 3.5, width = 3.5, units = "in", dpi = 600)

#######################################################################################
# Now water status vs ignition delay plot
######################################################################################

wp_ig <- ggplot(final_data, aes(wp, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") +
  labs(tag = "(a)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) + 
  pubtheme +
  theme(legend.text = element_text(face = "italic"),
        legend.position = c(0.30, 0.80),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 


cmc_ig <- ggplot(final_data, aes(cmc, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("") +
  labs(tag = "(b)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) + 
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 
 

combined_cmc_ig <- wp_ig | cmc_ig

#######################################################################################
# Now water status vs heat release plot
######################################################################################

wp_heat_release <- ggplot(final_data, aes(wp, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("") +
  ylab("Heat release (kJ)") +
  ylim(0, NA) +
  labs(tag = "(c)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) + 
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

cmc_heat_release <- ggplot(final_data, aes(cmc, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("") +
  ylab("") +
  labs(tag = "(d)") +
  ylim(0, NA) +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) + 
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.02, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

combined_water_status_flam <-   (wp_heat_release | cmc_heat_release) / (combined_cmc_ig)
  
ggsave("./results/figure2.pdf", plot = combined_water_status_flam,
       height = 7.25, width = 7.25, units = "in", dpi = 300)

##########################################################################################
# Species specific ignition and heat release sensitivity to LFMC loss rate
##########################################################################################

species_sum$display_name <- factor(
  species_sum$display_name, 
  levels = species_sum$display_name[order(species_sum$wp_ign_sens,
                                          decreasing = TRUE)])

wp_ign_sens_dry_down <- ggplot(species_sum, aes(abs(dry_down_rate), wp_ign_sens)) +
  geom_point(aes(color= display_name), size=4, alpha=0.9, shape=16) +
  geom_smooth(method="lm", se = FALSE, size = 1.5, color = "black") +
  xlab("Shoot moisture loss rate (%/hr)") +
  ylab(expression(bold(paste("Ignitibility response to ", Psi, " (",s %.% MPa^{-1}, ")")))) +
  labs(tag = "(a)") +
  ylim(0, 8.8) +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) +
  pubtheme +
  theme(legend.text = element_text(face = "italic"),
        legend.title = element_text(size = 7, face = "bold"),
        legend.position = c(0.70, 0.85),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.05, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 

cmc_heat_release_sens_dry_down <- ggplot(species_sum, aes(abs(dry_down_rate), abs(cmc_heat_rlease_sens))) +
  geom_point(aes(color= display_name), size = 4, alpha=0.9, shape=16) +
  geom_smooth(method="lm", se = FALSE, size = 1.5, color = "black") +
  xlab("Shoot moisture loss rate (%/hr)") +
  ylab(expression(bold(paste("Heat release response to LFMC (kJ/%)")))) +
  labs(tag = "(b)") +
  ylim(0, 0.3) +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) +
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.08, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title.x = element_text(size = textsize, face = "bold")) 

combined_dry_down_ig_heat_release <-  wp_ign_sens_dry_down|cmc_heat_release_sens_dry_down

ggsave("./results/figure3.pdf", plot = combined_dry_down_ig_heat_release,
       height = 5, width = 7.25, units = "in", dpi = 600)

############################################################################################
# Drydown plot
############################################################################################

fmc_time_plot <- ggplot(time_wp, aes(hours, fmc, color = display_name)) +
  geom_point(size = 3, alpha = 0.5, shape = 16) +
  geom_smooth(method="lm", se = FALSE, size = 1.5) +
  xlab("Time (hr)") +
  ylab("Shoot moisture content (%)") +
  labs(tag = "(a)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) +
  pubtheme +
  theme(legend.text = element_text(face="italic"),
        legend.title = element_text(size = 7, face = "bold"),
        legend.position = c(0.75, 0.80),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.01, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 

wp_time_plot <- ggplot(time_wp, aes(hours, -1*wp, color = display_name)) +
  geom_point(size=3, alpha = 0.5, shape = 16) +
  geom_smooth(method="lm", se = FALSE, size = 1.5) +
  xlab("Time (hr)") +
  ylab("Water potential (MPa)") +
  labs(tag = "(b)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) +
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"),
        plot.tag.position = c(0.01, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 

combined_fmc_wp_time <- wp_time_plot|fmc_time_plot

ggsave("./results/combined_fmc_wp_time.pdf", plot = combined_fmc_wp_time,
       height = 5, width = 7.25, units = "in", dpi = 600)

###################################################################################################
# Correlation plot
###################################################################################################

traits_cor <- as.data.frame(traits_cor)
p_mat <- ggcorrplot::cor_pmat(traits_cor)

corr_plot <- ggcorrplot::ggcorrplot(traits_cor, type = "lower", lab_size = 3,
                                    lab = TRUE, show.legend = FALSE,
                                    p.mat = p_mat,
                                    sig.level = 0.05, 
                                    insig = "blank",
                                    colors = c(NA, NA),      
                                    hc.order = TRUE) + 
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 10),
        panel.grid = element_blank())

ggsave("./results/corr_plot.pdf", plot =corr_plot,
       height = 6.5, width = 6.5, units = "in", dpi = 600)


################################################################################
# PCA plot, only for shoot flammability
###############################################################################

wp_fmc_pca_axis <- as.data.frame(flam_pca$x)

wp_fmc_variable_loadings <- as.data.frame(flam_pca$rotation)

wp_fmc_variable_loadings <- wp_fmc_variable_loadings %>%
  mutate(PC1 = -1*PC1) 

pca_plot <- ggplot(wp_fmc_pca_axis, aes(x = -1*PC1, y = -1*PC2)) +
  geom_segment(data = wp_fmc_variable_loadings, aes(
    x = 0, y = 0,
    xend = PC1, yend = PC2),
    arrow = arrow(length = unit(2, "mm")),
    color = "black") +
  labs(x = "Principle component 1",
       y = "Principle component 2") +
  pubtheme +
  theme(axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) +
  xlim(-0.1, 0.85) +
  geom_text(x = wp_fmc_variable_loadings[1,1] + 0.05, y = wp_fmc_variable_loadings[1,2] + 0.045,
            label = "Heat release", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[2,1] + 0.05, y = wp_fmc_variable_loadings[2,2] - 0.035,
            label = "Volume burned", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[3,1] + 0.1, y = wp_fmc_variable_loadings[3,2],
            label = "Flame height ", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[4,1] + 0.1, y = wp_fmc_variable_loadings[4,2] + 0.02,
            label = "Flame duration", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[5,1] + 0.15, y = wp_fmc_variable_loadings[5,2] + 0.013,
            label = "Duration over 100 \u00B0C", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[6,1] + 0.15, y = wp_fmc_variable_loadings[6, 2] + 0.02,
            label = "Peak temperature", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[7,1] + 0.17, y = wp_fmc_variable_loadings[7,2]-0.02,
            label = "Temperature integration", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[8,1]+ 0.14, y = wp_fmc_variable_loadings[8,2] - 0.01,
            label = "Ignition delay time", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) 

ggsave("./results/pca_plot.pdf", plot = pca_plot, 
       height = 5.5, width = 5.5, units = "in", dpi = 350) 

#####################################################################################################
# PCA for appendix
####################################################################################################

cor_pca <- prcomp(cor_data, scale=TRUE)

summary(cor_pca)

cor_pca$rotation[, 1:4]

#biplot(cor_pca)

cor_pca_axis <- as.data.frame(cor_pca$x)

cor_pca_variable_loadings <- as.data.frame(cor_pca$rotation)

cor_pca_plot <- ggplot(cor_pca_axis, aes(x = PC1, y = PC2)) +
  geom_segment(data = cor_pca_variable_loadings, aes(
    x = 0, y = 0,
    xend = PC1, yend = PC2),
    arrow = arrow(length = unit(2, "mm")),
    color = "black") +
  labs(x = "Principle component 1",
       y = "Principle component 2") +
  pubtheme +
  theme(axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) +
  xlim(-0.7, 0.70) +
  geom_text(x = cor_pca_variable_loadings[1,1] -0.13, y = cor_pca_variable_loadings[1,2],
            label = "ignitibility sensitivity", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[2,1] + 0.15, y = cor_pca_variable_loadings[2,2],
            label = "heat release sensitivity", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[3,1] - 0.035, y = cor_pca_variable_loadings[3,2],
            label = "LMA", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[4,1], y = cor_pca_variable_loadings[4,2] - 0.015,
            label = "LDMC", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[5,1], y = cor_pca_variable_loadings[5,2] + 0.01,
            label = "leaf length", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[6,1], y = cor_pca_variable_loadings[6, 2] + 0.01,
            label = "leaf area", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[7,1], y = cor_pca_variable_loadings[7,2] + 0.01,
            label = "total dry mass", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[8,1], y = cor_pca_variable_loadings[8,2] - 0.01,
            label = "TLP", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[9,1]-0.04, y = cor_pca_variable_loadings[9,2] + 0.01,
            label = "RWC_TLP", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[10,1] + 0.1, y = cor_pca_variable_loadings[10,2] -0.01,
            label = "leaf capacitance", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[11,1] + 0.15, y = cor_pca_variable_loadings[11,2],
            label = "capacitance below TLP", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[12,1] + 0.08, y = cor_pca_variable_loadings[12,2] + 0.01,
            label = "modulus elasticity", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[13,1] + 0.15, y = cor_pca_variable_loadings[13,2],
            label = "saturated water content", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[14,1]-0.08, y = cor_pca_variable_loadings[14,2] - 0.01,
            label = "osmotic potential", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[15,1] + 0.04, y = cor_pca_variable_loadings[15,2] + 0.01,
            label = "shoot fuel moisture capacitance", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20) +
  geom_text(x = cor_pca_variable_loadings[16,1] + 0.03, y = cor_pca_variable_loadings[16,2] - 0.01,
            label = "LFMC loss rate", size = 3, color = "black", fontface = "plain", family = "sans", alpha = 0.20)

ggsave("./results/supp_pca_plot.pdf", plot = cor_pca_plot, 
       height = 7, width = 7.25, units = "in", dpi = 300) 
  
###############################################################################
# Model comparisons output
###############################################################################

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


models_likelihood_and_stuff_for_ignition <- model_stats(wp_species_ignition, #lfmc_species_ignition, 
                                                        cmc_species_ignition)

print(xtable(models_likelihood_and_stuff_for_ignition), type = "html",
      file = "./results/ignition_models_comparisons.html")

models_likelihood_and_stuff_for_heat_release <- model_stats(cmc_species_heat_release, #lfmc_species_heat_release,
                                                            wp_species_heat_release)

print(xtable(models_likelihood_and_stuff_for_heat_release), type = "html", 
      file = "./results/heat_release_models_comparisons.html")  

######################################################################
# Preliminary dry down from 2023 figures
#####################################################################

fig1 <- drydown_long %>%
  ggplot(aes(time, water_potential, color=plantID)) +
  facet_grid(. ~ species) +
  geom_line() +
  xlab("Bench drying time (hr)") + ylab("Water potential (MPa)") +
  theme_bw() +
  theme(axis.text = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        strip.text = element_text(face = "italic", size = 8),
        legend.position = "none")

ggsave("./results/dry_down.pdf", plot = fig1,
       height = 5.5, width = 5.5, units = "in", dpi = 600)


