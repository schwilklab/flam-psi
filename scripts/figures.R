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
# Now water potential and fmc vs ignition delay plot
######################################################################################

wp_ig <- ggplot(filter(final_data, self_ignition != 1), aes(wp, ignition_delay, color = spcode)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay (s)") +
  scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.28,0.85),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))

ggsave("./results/wp_ignition.pdf", plot=wp_ig, 
       height = 180, width = 170, units = "mm", dpi = 300)


fmc_ig <- ggplot(filter(final_data, self_ignition != 1), aes(fmc, ignition_delay, color = spcode)) +
  dws_point + bestfit +
  xlab("Moisture content (%)") +
  ylab("Ignition delay (s)") +
  scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.20,0.85),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))

ggsave("./results/fmc_ignition.pdf", plot=fmc_ig, 
       height = 180, width = 170, units = "mm", dpi = 300)



#######################################################################################
# Now water potential and fmc vs temperature integration plot
######################################################################################

wp_degsec <- ggplot(filtered_data, aes(wp, degsec_100, color = spcode)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.80,0.90),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))

ggsave("./results/wp_degsec_100.pdf", plot= wp_degsec, 
       height = 180, width = 170, units = "mm", dpi = 300)


fmc_degsec <- ggplot(filtered_data, aes(fmc, degsec_100, color = spcode)) +
  dws_point + bestfit +
  xlab("Moisture content (%)") +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) +
  scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.80,0.90),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))

ggsave("./results/fmc_degsec_100.pdf", plot= fmc_degsec, 
       height = 180, width = 170, units = "mm", dpi = 300)


###############################################################################
# Segmented model plots for fmc for only JUPI
###############################################################################

predicted_model_data_jp_fmc <- data.frame(fmc = segmented_jupi_fmcig$fmc, ignition_delay = fitted(jupi_seg_fmc))

jupi_threshold_fmcig <- ggplot(segmented_jupi_fmcig, aes(x = fmc, y = ignition_delay)) + 
  geom_line(data = predicted_model_data_jp_fmc, size = 1.2) +
  geom_point(data = segmented_jupi_fmcig) +
  dws_point +
  xlab("Moisture content (%)") +
  ylab("Ignition delay time (s)") + 
  pubtheme +
  geom_vline(xintercept = confint(jupi_seg_fmc)[2], linetype = "dashed", size = 1) +
  geom_vline(xintercept = confint(jupi_seg_fmc)[3], linetype = "dashed", size = 1) +
  geom_rect(aes(xmin = confint(jupi_seg_fmc)[2], xmax = confint(jupi_seg_fmc)[3], ymin = -Inf, ymax = Inf),
            fill = "lightsteelblue", alpha = 0.005)

ggsave("./results/jupi_threshold_fmcig.pdf", plot = jupi_threshold_fmcig, 
       height = 180, width = 170, units = "mm", dpi = 300) 

predicted_model_data_jp_fmc_degsec <- data.frame(fmc = segmented_jupi_fmc_degsec$fmc,  
                                                 degsec_100 = fitted(jupi_fmc_degsec_segmented))

jupi_threshold_fmc_degsec <- ggplot(segmented_jupi_fmc_degsec, aes(x = fmc, y = degsec_100)) + 
  geom_line(data = predicted_model_data_jp_fmc_degsec, size = 1.2) +
  geom_point(data = segmented_jupi_fmc_degsec) +
  dws_point +
  xlab("Moisture content (%)") +
  ylab(expression(Temperature ~ integration ~ (degree~C %.% s ) )) + 
  pubtheme +
  geom_vline(xintercept = confint(jupi_fmc_degsec_segmented)[2], linetype = "dashed", size = 1) +
  geom_vline(xintercept = confint(jupi_fmc_degsec_segmented)[3], linetype = "dashed", size = 1) +
  geom_rect(aes(xmin = confint(jupi_fmc_degsec_segmented)[2], xmax = confint(jupi_fmc_degsec_segmented)[3], ymin = -Inf, ymax = Inf),
            fill = "lightsteelblue", alpha = 0.005)

ggsave("./results/jupi_threshold_fmc_degsec.pdf", plot = jupi_threshold_fmc_degsec, 
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
