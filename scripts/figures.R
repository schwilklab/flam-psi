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
              slope = coef(summary(wp_fmc_kr_model))[4], size = 1.5, color = schwilkcolors[2])



ggsave("./results/wp_fmc.pdf", plot=wp_fmc_plot, width=col1, height=col1, units="cm")


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


ggsave("./results/wp_ignition.pdf", plot=wp_ignition_plot, width=col1, height=col1, units="cm")


##########################################################################################
# Threshold for Prosopis glandulosa for ignition delay time
##########################################################################################

predicted_model_data <- data.frame(wp = segmented_prgl2_ig$wp, ignition_delay = fitted(prgl2_seg))

# plot the fitted model
prgl2_threshold <- ggplot(predicted_model_data, aes(x = wp, y = ignition_delay)) + 
  geom_line() +
  geom_point(data = segmented_prgl2_ig) +
  dws_point +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") + 
  pubtheme +
  geom_vline(xintercept = confint(prgl2_seg)[2], linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_rect(aes(xmin = confint(prgl2_seg)[2], xmax = 0, ymin = -Inf, ymax = Inf),
            fill = "lightsteelblue", alpha = 0.005)

ggsave("./results/mesquite_threshold.pdf", plot = prgl2_threshold, width=col1, height=col1, units="cm")  


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
  theme_bw() +
  xlim(-0.1, 0.70) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_text(x = wp_fmc_variable_loadings[1,1] + 0.1, y = wp_fmc_variable_loadings[1,2],
            label = "Heat release (j)", size = 2, color = "black") +
  geom_text(x = wp_fmc_variable_loadings[2,1] + 0.025, y = wp_fmc_variable_loadings[2,2] - 0.025,
            label = "Volume burned (%)", size = 2, color = "black") +
  geom_text(x = wp_fmc_variable_loadings[3,1] + 0.06, y = wp_fmc_variable_loadings[3,2] - 0.025,
            label = "Flame height (cm)", size = 2, color = "black") +
  geom_text(x = wp_fmc_variable_loadings[4,1] + 0.12, y = wp_fmc_variable_loadings[4,2] + 0.01,
            label = "Flame duration (s)", size = 2, color = "black") +
  geom_text(x = wp_fmc_variable_loadings[5,1] + 0.05, y = wp_fmc_variable_loadings[5,2] + 0.03,
            label = "Duration over 100 \u00B0C (s)", size = 2, color = "black") +
  geom_text(x = wp_fmc_variable_loadings[6,1] + 0.15, y = wp_fmc_variable_loadings[6, 2],
            label = "Peak temperature (\u00B0C)", size = 2, color = "black") +
  geom_text(x = wp_fmc_variable_loadings[7,1] + 0.15, y = wp_fmc_variable_loadings[7,2] - 0.045,
            label = "Temperature integration (\u00B0C.s)", size = 2, color = "black") +
  geom_text(x = wp_fmc_variable_loadings[8,1], y = wp_fmc_variable_loadings[8,2] + 0.03,
            label = "Ignition delay time (s)", size = 2, color = "black") 


ggsave("./results/pca_plot.pdf", plot = pca_plot, width=col1, height=col1, units="cm") 
