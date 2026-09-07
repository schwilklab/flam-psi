## figures.R

# This R script is used to create figures, anova and coefficient tables

library(xtable)
library(patchwork)
source("scripts/ggplot_themes.R")

# Two dataset, one is final_data which is ajb_flam_psi paper and another
# dataset named puja_flam. Since there is 21 species, generating the
# figures separately might be better. We can also make a single data set if needed.
# For now, keeping them separately.

###################################################################################
# From colorful to black, this is for Azaj's ajb paper's figures
###################################################################################

## DWS: I do not understand these comments.


####################################################################################
# LFMC vs ignition delay
####################################################################################

## DWS: This should be done much earlier, but attempting to reconcile data here for now:

flam_data_azaj <- select(final_data, sample_id, spcode, display_name, ecosystem,
                         cmc, ignition_delay, heat_release_j, wp)

flam_data_puja <- select(puja_flam, sample_id, spcode, display_name, ecosystem,
                         cmc, ignition_delay, heat_release_j, wp)

flam_data <- bind_rows(flam_data_azaj, flam_data_puja)

##  wp sens?  capacitance?  I'm ignoring this and making colors based only on name.

wpsens <- select(species_sum, spcode, display_name, wp_sens)
wpsens <- bind_rows(wpsens, select(puja_species_wp_cmc, spcode, display_name, wp_sens)) %>%
  arrange(wp_sens)




## species_sorted <- species_sum %>% arrange(wp_sens) %>% dplyr::select(display_name)
## species_sorted <- unname(unlist(as.vector(species_sorted[,1])))
## final_data$display_name <- factor(final_data$display_name, levels = species_sorted)
## names(schwilkcolors) <- species_sorted
## species_sorted_wp_ig <- species_sum %>% arrange(desc(wp_ign_sens)) %>% dplyr::select(display_name)
## species_sorted_wp_ig <- unname(unlist(as.vector(species_sorted_wp_ig[,1])))
## final_data$display_name <- factor(final_data$display_name, levels = species_sorted_wp_ig)
## schwilkcolors_wp_ig <- schwilkcolors
## names(schwilkcolors_wp_ig) <- species_sorted_wp_ig

## ## ####################################################################################
## ## # This one for Puja
## ## ####################################################################################

## ## ###################################################################################
## ## # From colorful to black, this is for Azaj's ajb paper's figures
## ## ###################################################################################

## puja_species_sorted <- puja_species_wp_cmc %>% arrange(wp_sens) %>% dplyr::select(display_name)
## puja_species_sorted <- unname(unlist(as.vector(puja_species_sorted[,1])))
## puja_flam$display_name <- factor(puja_flam$display_name, levels = puja_species_sorted)
## names(pujacolors) <- puja_species_sorted
## puja_species_sorted_wp_ig <- puja_species_wp_ign_sensitivity %>% arrange(desc(wp_ign_sens)) %>% dplyr::select(display_name)
## puja_species_sorted_wp_ig <- unname(unlist(as.vector(puja_species_sorted_wp_ig[,1])))
## puja_flam$display_name <- factor(puja_flam$display_name, levels = puja_species_sorted_wp_ig)
## pujacolors_wp_ig <- schwilkcolors
## names(pujacolors_wp_ig) <- puja_species_sorted_wp_ig




yaxis_range_ig <- range(flam_data$ignition_delay, na.rm = TRUE)

lfmc_ig <- ggplot(flam_data, aes(cmc, ignition_delay, color = display_name)) +
  facet_grid( . ~ ecosystem) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("Ignition delay time (s)") +
  scale_y_continuous(limits = yaxis_range_ig) +
  scale_x_continuous(limits = c(20,160)) +
#  scale_color_manual(name = "", values = pujacolors) + 
  prestheme.nogridlines +
  theme(legend.position = c(0.20, 0.80),
        legend.text = element_text(size = smsize, face = "italic"),
        plot.margin = margin(t=8,r=0,b=8,l=5))

ctx_data <- filter(flam_data, ecosystem=="CTX")
sac_data <- filter(flam_data, ecosystem %in% c("SAC", "DES"))

lfmc_ig_ctx <- ggplot(ctx_data,
                     aes(cmc, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("Ignition delay time (s)") +
  scale_y_continuous(limits = yaxis_range_ig) +
  scale_x_continuous(limits = c(20,160)) +
  scale_color_manual(name = "", values = schwilkcolors_extended) + 
  prestheme.nogridlines +
  theme(legend.position = c(0.20, 0.75),
        legend.text = element_text(size = smsize, face = "italic"),
        plot.margin = margin(t=8,r=0,b=8,l=5))

lfmc_ig_sac <- ggplot(sac_data,
                      aes(cmc, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("") +
  scale_y_continuous(limits = yaxis_range_ig) +
  scale_x_continuous(limits = c(20,160)) +
  scale_color_manual(name = "", values = schwilkcolors) + 
  prestheme.nogridlines +
  theme(legend.position = c(0.20, 0.75),
        legend.text = element_text(size = smsize, face = "italic"),
        axis.text.y = element_blank(),
        plot.margin = margin(t=8,r=5,b=8,l=0))

lfmc_ig_sac

combined_lfmc_ig <- lfmc_ig_ctx | lfmc_ig_sac
#plot_grid(lfmc_ig_azaj, lfmc_ig_puja)
combined_lfmc_ig

ggsave("./results/presentation_figures/combined_lfmc_ig.pdf",
       device = cairo_pdf,
       plot = combined_lfmc_ig,
       width = beamer_width*2, height = beamer_height*1.5, units = "cm")


## ggsave("./results/presentation_figures/lfmc_ig_nm.pdf",
##        device = cairo_pdf,
##        plot = lfmc_ig_puja,
##        width = beamer_width, height = beamer_height, units = "cm")



## Same, but with wp:

wp_ig_ctx <- ggplot(ctx_data,
                     aes(wp, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") +
  scale_y_continuous(limits = yaxis_range_ig) +
#  scale_x_continuous(limits = c(20,160)) +
  scale_color_manual(name = "", values = schwilkcolors_extended) + 
  prestheme.nogridlines +
  theme(legend.position = c(0.20, 0.75),
        legend.text = element_text(size = smsize, face = "italic"),
        plot.margin = margin(t=8,r=0,b=8,l=5))

wp_ig_sac <- ggplot(sac_data,
                      aes(wp, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("") +
  scale_y_continuous(limits = yaxis_range_ig) +
 # scale_x_continuous(limits = c(20,160)) +
  scale_color_manual(name = "", values = schwilkcolors) + 
  prestheme.nogridlines +
  theme(legend.position = c(0.20, 0.75),
        legend.text = element_text(size = smsize, face = "italic"),
        axis.text.y = element_blank(),
        plot.margin = margin(t=8,r=5,b=8,l=0))

wp_ig_sac

combined_wp_ig <- wp_ig_ctx | wp_ig_sac
combined_wp_ig

ggsave("./results/presentation_figures/combined_wp_ig.pdf",
       device = cairo_pdf,
       plot = combined_wp_ig,
       width = beamer_width*2, height = beamer_height*1.5, units = "cm")




####################################################################################
# LFMC vs heat release
####################################################################################

#yaxis_range_heat <- range(flam_data$heat_release_j)

heat_release_ctx <- ggplot(ctx_data,
                           aes(cmc, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("Heat release (kJ)") +
#  scale_y_continuous(limits = yaxis_range_heat) +
   scale_x_continuous(limits = c(20,160)) +
  scale_color_manual(name = "", values = schwilkcolors_extended) + 
  prestheme.nogridlines +
  theme(legend.position = c(0.77, 0.75),
        legend.text = element_text(size = smsize, face = "italic"),
        plot.margin = margin(t=8,r=0,b=8,l=5))

heat_release_sac <- ggplot(sac_data,
                            aes(cmc, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("LFMC (%)") +
  ylab("") +
 # scale_y_continuous(limits = yaxis_range_heat) +
   scale_x_continuous(limits = c(20,160)) +
  scale_color_manual(name = "", values = schwilkcolors) + 
  prestheme.nogridlines +
  theme(legend.position = c(0.75, 0.80),
        legend.text = element_text(size = smsize, face = "italic"),
        axis.text.y = element_blank(),
        plot.margin = margin(t=8,r=5,b=8,l=0))

combined_lfmc_heat<- heat_release_ctx | heat_release_sac

ggsave("./results/presentation_figures/combined_lfmc_heat.pdf",
       device = cairo_pdf,
       plot = combined_ldmc_heat,
       width = beamer_width*2, height = beamer_height*1.5, units = "cm")


## Same as above for wp

wp_heat_release_ctx <- ggplot(ctx_data,
                           aes(wp, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Heat release (kJ)") +
#  scale_y_continuous(limits = yaxis_range_heat) +
#   scale_x_continuous(limits = c(20,160)) +
  scale_color_manual(name = "", values = schwilkcolors_extended) + 
  prestheme.nogridlines +
  theme(legend.position = c(0.77, 0.75),
        legend.text = element_text(size = smsize, face = "italic"),
        plot.margin = margin(t=8,r=0,b=8,l=5))

wp_heat_release_sac <- ggplot(sac_data,
                            aes(wp, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("") +
 # scale_y_continuous(limits = yaxis_range_heat) +
 #  scale_x_continuous(limits = c(20,160)) +
  scale_color_manual(name = "", values = schwilkcolors) + 
  prestheme.nogridlines +
  theme(legend.position = c(0.75, 0.80),
        legend.text = element_text(size = smsize, face = "italic"),
        axis.text.y = element_blank(),
        plot.margin = margin(t=8,r=5,b=8,l=0))

wp_combined_heat<- wp_heat_release_ctx | wp_heat_release_sac

ggsave("./results/presentation_figures/wp_combined_heat.pdf",
       device = cairo_pdf,
       plot = wp_combined_heat,
       width = beamer_width*2, height = beamer_height*1.5, units = "cm")



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
  scale_color_manual(name = "", values = schwilkcolors_extended) +
  prestheme.nogridlines +
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
  scale_color_manual(name = "", values = schwilkcolors_extended) +
  prestheme.nogridlines +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        axis.text = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")) 

combined_dry_down_ig_heat_release <-  wp_ign_sens_dry_down|cmc_heat_release_sens_dry_down

ggsave("./results/presentation_figures/dry_down_sensitivity.pdf",
       plot = combined_dry_down_ig_heat_release,
       width = beamer_width*2, height = beamer_height*1.5,
       units = "cm")


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

