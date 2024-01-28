## figures.R

# This R script is used to create figures, anova and coefficient tables

library(xtable)


###########################################################################
# Intially water potential vs fmc plot
##########################################################################

wp_fmc_plot <- ggplot(final_data, aes(wp, fmc, color=display_name)) +
  dws_point +
  xlab("Water potential (MPa)") +
  ylab("Moisture content (%)") +
  scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.28,0.85),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize)) +
  geom_abline(intercept = coef(summary(wp_fmc_kr_model))[1], 
              slope = coef(summary(wp_fmc_kr_model))[2], size = 1.5, color = schwilkcolors[1]) +
  geom_abline(intercept = coef(summary(wp_fmc_kr_model))[1], 
              slope = coef(summary(wp_fmc_kr_model))[3], size = 1.5, color = schwilkcolors[2]) +
  geom_abline(intercept = coef(summary(wp_fmc_kr_model))[1], 
              slope = coef(summary(wp_fmc_kr_model))[4], size = 1.5, color = schwilkcolors[3])



ggsave("./results/wp_fmc.pdf", plot=wp_fmc_plot,
       height = 180, width = 170, units = "mm", dpi = 300)


#######################################################################################
# Now water potential vs ignition delay plot
######################################################################################

wp_ignition_plot <- ggplot(without_self_ignition, aes(wp, ignition_delay, color=display_name)) +
  dws_point +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") +
  scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.28,0.85),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize)) +
  geom_abline(intercept = coef(summary(wp_withoutself_ignition_kr_model))[1], 
              slope = coef(summary(wp_withoutself_ignition_kr_model))[2], size = 1.5, color = schwilkcolors[1]) +
  geom_abline(intercept = coef(summary(wp_withoutself_ignition_kr_model))[1], 
              slope = coef(summary(wp_withoutself_ignition_kr_model))[3], size = 1.5, alpha = 0.8, color = schwilkcolors[2]) +
  geom_abline(intercept = coef(summary(wp_withoutself_ignition_kr_model))[1], 
              slope = coef(summary(wp_withoutself_ignition_kr_model))[4], size = 1.5, alpha = 1.1, color = schwilkcolors[3])


ggsave("./results/wp_ignition.pdf", plot=wp_ignition_plot, 
       height = 180, width = 170, units = "mm", dpi = 300)


##########################################################################################
# Threshold for Juniperus pinchotii and Prosopis glandulosa for ignition delay time
# Initially Juniperus pinchotii
##########################################################################################

predicted_model_data_jp <- data.frame(wp = segmented_jupi_ig$wp, ignition_delay = fitted(jupi_seg))

# plot the fitted model
jupi_threshold <- ggplot(segmented_jupi_ig, aes(x = wp, y = ignition_delay)) + 
  geom_line(data = predicted_model_data_jp, size = 1.2) +
  geom_point(data = segmented_jupi_ig) +
  dws_point +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") + 
  pubtheme +
  geom_vline(xintercept = confint(jupi_seg)[2], linetype = "dashed", size = 1) +
  geom_vline(xintercept = confint(jupi_seg)[3], linetype = "dashed", size = 1) +
  geom_rect(aes(xmin = confint(jupi_seg)[2], xmax = confint(jupi_seg)[3], ymin = -Inf, ymax = Inf),
            fill = "lightsteelblue", alpha = 0.005)

ggsave("./results/jupi_threshold.pdf", plot = jupi_threshold, 
       height = 180, width = 170, units = "mm", dpi = 300) 

###################################################################################################
# Prosopis glandulosa
###################################################################################################

predicted_model_data <- data.frame(wp = segmented_prgl2_ig$wp, ignition_delay = fitted(prgl2_seg))

# plot the fitted model
prgl2_threshold <- ggplot(segmented_prgl2_ig, aes(x = wp, y = ignition_delay)) + 
  geom_line(data = predicted_model_data, size = 1.2) +
  geom_point(data = segmented_prgl2_ig) +
  dws_point +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") + 
  pubtheme +
  geom_vline(xintercept = confint(prgl2_seg)[2], linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_rect(aes(xmin = confint(prgl2_seg)[2], xmax = 0, ymin = -Inf, ymax = Inf),
            fill = "lightsteelblue", alpha = 0.005)

ggsave("./results/mesquite_threshold.pdf", plot = prgl2_threshold, 
       height = 180, width = 170, units = "mm", dpi = 300)  


################################################################################
# PCA plot
###############################################################################

wp_fmc_pca_axis <- as.data.frame(flam_pca$x)

wp_fmc_variable_loadings <- as.data.frame(flam_pca$rotation)

pca_plot <- ggplot(wp_fmc_pca_axis, aes(x = PC1, y = PC2)) +
  geom_segment(data = wp_fmc_variable_loadings, aes(
    x = 0, y = 0,
    xend = PC1, yend = PC2),
    arrow = arrow(length = unit(2, "mm")),
    color = "black") +
  labs(x = "Principle component 1",
       y = "Principle component 2") +
  pubtheme +
  xlim(-0.1, 0.70) +
  geom_text(x = wp_fmc_variable_loadings[1,1] + 0.07, y = wp_fmc_variable_loadings[1,2],
            label = "Heat release", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[2,1] + 0.055, y = wp_fmc_variable_loadings[2,2] - 0.02,
            label = "Volume burned", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[3,1] + 0.06, y = wp_fmc_variable_loadings[3,2] - 0.025,
            label = "Flame height ", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[4,1] + 0.075, y = wp_fmc_variable_loadings[4,2] + 0.01,
            label = "Flame duration", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[5,1] + 0.1, y = wp_fmc_variable_loadings[5,2] + 0.025,
            label = "Duration over 100 \u00B0C", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[6,1] + 0.09, y = wp_fmc_variable_loadings[6, 2],
            label = "Peak temperature", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[7,1] + 0.12, y = wp_fmc_variable_loadings[7,2] - 0.01,
            label = "Temperature integration", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) +
  geom_text(x = wp_fmc_variable_loadings[8,1], y = wp_fmc_variable_loadings[8,2] + 0.03,
            label = "Ignition delay time", size = 4, color = "black", fontface = "plain", family = "sans", alpha = 0.01) 

ggsave("./results/pca_plot.pdf", plot = pca_plot, height = 180,
       width = 170, units = "mm", dpi = 300) 
