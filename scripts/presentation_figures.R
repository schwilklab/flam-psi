## figures.R

# This R script is used to create figures, anova and coefficient tables

library(xtable)
library(patchwork)

# Two dataset, one is final_data which is ajb_flam_psi paper and another
# dataset named puja_flam. Since there is 21 species, generating the
# figures separately might be better. We can also make a single data set if needed.
# For now, keeping them separately.

###################################################################################
# From colorful to black, this is for Azaj's ajb paper's figures
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

####################################################################################
# This one for Puja
####################################################################################

###################################################################################
# From colorful to black, this is for Azaj's ajb paper's figures
###################################################################################

puja_species_sorted <- puja_species_wp_cmc %>% arrange(wp_sens) %>% dplyr::select(display_name)
puja_species_sorted <- unname(unlist(as.vector(puja_species_sorted[,1])))
puja_flam$display_name <- factor(puja_flam$display_name, levels = puja_species_sorted)
names(pujacolors) <- puja_species_sorted
puja_species_sorted_wp_ig <- puja_species_wp_ign_sensitivity %>% arrange(desc(wp_ign_sens)) %>% dplyr::select(display_name)
puja_species_sorted_wp_ig <- unname(unlist(as.vector(puja_species_sorted_wp_ig[,1])))
puja_flam$display_name <- factor(puja_flam$display_name, levels = puja_species_sorted_wp_ig)
pujacolors_wp_ig <- pujacolors
names(pujacolors_wp_ig) <- puja_species_sorted_wp_ig

####################################################################################
# LFMC vs ignition delay
####################################################################################

yaxis_range_ig <- range(c(final_data$ignition_delay, puja_flam$ig_delay), na.rm = TRUE)

lfmc_ig_azaj <- ggplot(final_data, aes(cmc, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("Ignition delay time (s)") +
  scale_y_continuous(limits = yaxis_range_ig) +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) + 
  prestheme +
  theme(legend.position = c(0.20, 0.80),
        legend.text = element_text(face = "italic"),
        plot.margin = unit(c(4, 12, 4, 12), "pt"),  
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold")) 

lfmc_ig_puja <- ggplot(puja_flam, aes(cmc, ig_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("") +
  scale_y_continuous(limits = yaxis_range_ig) +
  scale_color_manual(name = "", values = pujacolors_wp_ig) + 
  prestheme +
  theme(legend.position = c(0.20, 0.80),
        legend.text = element_text(face = "italic"),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold")) 


combined_lfmc_ig_puja_azaj <- lfmc_ig_azaj | lfmc_ig_puja


ggsave("./results/presentation_figures/combined_lfmc_ig_puja_azaj.pdf", plot = combined_lfmc_ig_puja_azaj,
       width = 13.333, height = 7.5, units = "in", dpi = 300)


####################################################################################
# LFMC vs heat release
####################################################################################

yaxis_range_heat <- range(c(final_data$heat_release_j, puja_flam$heat_release_j), na.rm = TRUE) / 1000

heat_release_azaj <- ggplot(final_data, aes(cmc, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("Heat release (kJ)") +
  scale_y_continuous(limits = yaxis_range_heat) +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) + 
  prestheme +
  theme(legend.position = c(0.75, 0.80),
        legend.text = element_text(face = "italic"),
        plot.margin = unit(c(4, 12, 4, 12), "pt"),  
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

heat_release_puja <- ggplot(puja_flam, aes(cmc, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("") +
  scale_y_continuous(limits = yaxis_range_heat) +
  scale_color_manual(name = "", values =  pujacolors_wp_ig) + 
  prestheme +
  theme(legend.position = c(0.75, 0.80),
        legend.text = element_text(face = "italic"),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

combined_heat_puja_azaj <- heat_release_azaj | heat_release_puja

ggsave("./results/presentation_figures/combined_heat_puja_azaj.pdf", plot = combined_heat_puja_azaj,
       width = 13.333, height = 7.5, units = "in", dpi = 300)


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
  ylab(expression(bold("Ignitibility response to LFMC (s" ~ . ~ "%"^{-1} ~ ")"))) + # Though this is ignitibility response to wp
  # but labelling it as ignitibility response to LFMC, only for presentation.
  ylim(0, 8.8) +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) +
  prestheme +
  theme(legend.text = element_text(face = "italic"),
        legend.position = c(0.80, 0.80),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold")) 

cmc_heat_release_sens_dry_down <- ggplot(species_sum, aes(abs(dry_down_rate), abs(cmc_heat_rlease_sens))) +
  geom_point(aes(color= display_name), size = 4, alpha=0.9, shape=16) +
  geom_smooth(method="lm", se = FALSE, size = 1.5, color = "black") +
  xlab("Shoot moisture loss rate (%/hr)") +
  ylab(expression(bold(paste("Heat release response to LFMC (kJ/%)")))) +
  ylim(0, 0.3) +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) +
  prestheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        axis.text = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) 

combined_dry_down_ig_heat_release <-  wp_ign_sens_dry_down|cmc_heat_release_sens_dry_down

ggsave("./results/presentation_figures/dry_down_sensitivity.pdf", plot = combined_dry_down_ig_heat_release,
       width = 13.333, height = 7.5, units = "in", dpi = 300)

############################################################################################
# Drydown plot, only for Azaj
############################################################################################

pres_fmc_time_plot_azaj <- ggplot(time_wp, aes(hours, fmc, color = display_name)) +
  geom_point(size = 3, alpha = 0.5, shape = 16) +
  geom_smooth(method="lm", se = FALSE, size = 1.5) +
  xlab("Time (hr)") +
  ylab("LFMC (%)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) +
  prestheme +
  theme(legend.text = element_text(face="italic"),
        legend.position = c(0.75, 0.80),
        plot.margin = unit(c(4, 10, 4, 10), "pt"),  
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold")) 

pres_wp_time_plot_azaj <- ggplot(time_wp, aes(hours, -1*wp, color = display_name)) +
  geom_point(size=3, alpha = 0.5, shape = 16) +
  geom_smooth(method="lm", se = FALSE, size = 1.5) +
  xlab("Time (hr)") +
  ylab("Water potential (MPa)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig) +
  prestheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 10, 4, 10), "pt"),  
        axis.text = element_text(face = "bold"),
        axis.title = element_text(face = "bold")) 

pres_combined_fmc_wp_time <- pres_wp_time_plot_azaj|pres_fmc_time_plot_azaj

ggsave("./results/presentation_figures/bench_dry_down_azaj.pdf", plot = pres_combined_fmc_wp_time,
       width = 13.333, height = 7.5, units = "in", dpi = 300)

