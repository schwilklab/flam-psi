## requires read-data and read-hobos.R
## TODO: organize scripts to minimize dependencies and clarify order.

library(ggplot2)
library(corrplot)

source("./scripts/ggplot_theme.R")

###############################################################################################
# 2024
##############################################################################################

short_ignite_2024 <- filter(alldata_2024, ignition_delay <= 20)

fig1_2024 <- ggplot(alldata_2024, aes(wp, lfmc, color = spcode)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Leaf moisture content (%)") +
  pubtheme +
  theme(legend.position = c(0.15,0.75),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))

ggsave("./results/fig1_2024.pdf", plot=fig1_2024, width=col1, height=col1, units="cm")


fig2_2024 <- ggplot(alldata_2024, aes(wp, cmc, color = spcode)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Canopy moisture content (%)") +
  pubtheme +
  theme(legend.position = c(0.15,0.75),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))

ggsave("./results/fig2_2024.pdf", plot=fig2_2024, width=col1, height=col1, units="cm")


fig3_2024 <- ggplot(short_ignite_2024, aes(wp, heat_release_j, color = spcode)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Heat release (J)") +
  pubtheme +
  theme(legend.position = c(0.90,0.85),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))

fig4_2024 <- ggplot(alldata_2024, aes(wp, ignition_delay, color = spcode)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") +
  pubtheme +
  ylim(0, 190) +
  theme(legend.position = c(0.1,0.85),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))

############################################################################################
# Supplementary figures
############################################################################################
drydown <- read_csv("./data/2023/shrub_drydown_test.csv")

drydown_long <- drydown %>% 
  dplyr::select(-note) %>%
  pivot_longer(cols=starts_with("hr_"), names_to="time",
               values_to="water_potential",
               names_pattern="hr_([0-9\\.]+)") %>%
  mutate(time = as.numeric(time), water_potential = -1*water_potential)


drydown_fig <- drydown_long %>%
  ggplot(aes(time, water_potential, color=plantID)) +
  facet_grid(. ~ species) +
  geom_line() +
  xlab("Bench drying time (hr)") + ylab("Water potential (MPa)") +
  pubtheme +
  theme(legend.position = "none",
        strip.text = element_text(face="italic"),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

ggsave("./results/drydowns.pdf", plot = drydown_fig, width = 7.25, height = 6, units="in")

###############################################################################
# corrplot
##############################################################################

pdf("./results/cor_plot.pdf", height = 7.5, width = 7.25) 
corrplot(traits_cor, method = "number", type = "upper")
dev.off()

###############################################################################
# figures including Juniperus pinchotii female
###############################################################################
alldata_jp <- alldata %>%
  filter(sample_id != "FT17") %>%
  filter(sample_id != "FT15") %>%
  filter(sample_id != "SXT19") %>%
  filter(year == 2024) %>%
  filter(wp != -8.24) %>%
  mutate(display_name = ifelse(spcode == "JUPIF", "J. pinchotii (F)", display_name))

unique(alldata_jp$display_name)

dim(alldata_jp)

species_wp_ign_sensitivity_all <- alldata_jp %>%
  nest(data = c(-spcode, -display_name)) %>%
  mutate(fit = map(data, ~ lm(ignition_delay ~ wp, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "wp") %>%
  dplyr::select(spcode, display_name, wp_ign_sens_all = estimate)

species_wp_ign_sensitivity_all

schwilkcolors_all <- c("#B22222", "#EC4E15", "#EE722E", "#D68D18", "#AF5F42", "#E3C477", "#A9B678", "#8F7955", "#4A4C4F", "#2C2A1D")

species_sorted_wp_ig_all <- species_wp_ign_sensitivity_all %>% arrange(desc(wp_ign_sens_all)) %>% dplyr::select(display_name)
species_sorted_wp_ig_all <- unname(unlist(as.vector(species_sorted_wp_ig_all[,1])))
alldata_jp$display_name <- factor(alldata_jp$display_name, levels = species_sorted_wp_ig_all)
schwilkcolors_wp_ig_all <- schwilkcolors_all
names(schwilkcolors_wp_ig_all) <- species_sorted_wp_ig_all

wp_ig_all <- ggplot(alldata_jp, aes(wp, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") +
  labs(tag = "(a)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig_all) + 
  pubtheme +
  theme(legend.text = element_text(face = "italic"),
        legend.position = c(0.30, 0.80),
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.06, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 

lfmc_ig_all <- ggplot(alldata_jp, aes(lfmc, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("Leaf moisture content (%)") +
  ylab("") +
  labs(tag = "(b)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig_all) + 
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.06, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 

cmc_ig_all <- ggplot(alldata_jp, aes(cmc, ignition_delay, color = display_name)) +
  dws_point + bestfit +
  xlab("Shoot moisture content (%)") +
  ylab("") +
  labs(tag = "(c)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig_all) + 
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.06, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 


combined_lfmc_cmc_ig_all <- wp_ig_all | lfmc_ig_all | cmc_ig_all

############################################################################
# Now heat release
############################################################################


cmc_heat_release_all <- ggplot(alldata_jp, aes(cmc, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("") +
  ylab("") +
  labs(tag = "(f)") +
  ylim(0, NA) +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig_all) + 
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.06, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

wp_heat_release_all <- ggplot(alldata_jp, aes(wp, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("") +
  ylab("Heat release (kJ)") +
  ylim(0, NA) +
  labs(tag = "(d)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig_all) + 
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.06, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

lfmc_heat_release_all <- ggplot(alldata_jp, aes(lfmc, heat_release_j/1000, color = display_name)) +
  dws_point + bestfit +
  xlab("") +
  ylab("") +
  ylim(0, NA) +
  labs(tag = "(e)") +
  scale_color_manual(name = "", values = schwilkcolors_wp_ig_all) + 
  pubtheme +
  theme(legend.position = "none",
        plot.margin = unit(c(4, 4, 4, 4), "pt"),  
        plot.tag = element_text(size = 12, face = "bold"), 
        plot.tag.position = c(0.08, 1),
        axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold"))

combined_water_status_flam_all <-   (wp_heat_release_all | lfmc_heat_release_all | cmc_heat_release_all) / (combined_lfmc_cmc_ig_all)


ggsave("./results/combined_water_status_ignition_heat_release_all.pdf", plot = combined_water_status_flam_all,
       height = 6, width = 7.25, units = "in", dpi = 300)



##########################################################################################
# Threshold for RHVI3
##########################################################################################

predicted_model_data_rhvi3 <- data.frame(wp = rhvi3$wp, ignition_delay = fitted(rhvi3_seg))

rhvi3_wp_ig_threshold <- ggplot(rhvi3, aes(x = wp, y = ignition_delay)) + 
  geom_line(data = predicted_model_data_rhvi3, size = 1.2) +
  geom_point(data = rhvi3_seg,  position = "jitter", alpha = 0.5) +
  dws_point +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") + 
  geom_vline(xintercept = confint(rhvi3_seg)[2], linetype = "dashed", size = 1) +
  geom_vline(xintercept = confint(rhvi3_seg)[3], linetype = "dashed", size = 1) +
  geom_rect(aes(xmin = confint(rhvi3_seg)[2], xmax = confint(rhvi3_seg)[3], ymin = -Inf, ymax = Inf),
            fill = "lightsteelblue", alpha = 0.005) +
  pubtheme +
  theme(axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 

ggsave("./results/rhvi3_wp_ig_threshold.pdf", plot = rhvi3_wp_ig_threshold, 
       height = 4, width = 3.5, units = "in", dpi = 600) 

predicted_model_data_rhvi3_heat <- data.frame(wp = rhvi3$wp, heat_release_j = fitted(rhvi3_heat_release_segmented))

rhvi3_wp_heat_threshold <- ggplot(rhvi3, aes(x = wp, y = heat_release_j)) + 
  geom_line(data = predicted_model_data_rhvi3_heat, size = 1.2) +
  geom_point(data = rhvi3_heat_release_segmented,  position = "jitter", alpha = 0.5) +
  dws_point +
  xlab("Water potential (MPa)") +
  ylab("Heat release (J)") + 
  geom_vline(xintercept = confint(rhvi3_heat_release_segmented)[2], linetype = "dashed", size = 1) +
  geom_vline(xintercept = confint(rhvi3_heat_release_segmented)[3], linetype = "dashed", size = 1) +
  geom_rect(aes(xmin = confint(rhvi3_heat_release_segmented)[2],
                xmax = confint(rhvi3_heat_release_segmented)[3], ymin = -Inf, ymax = Inf),
            fill = "lightsteelblue", alpha = 0.005) +
  pubtheme +
  theme(axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 

ggsave("./results/rhvi3_wp_heat_threshold.pdf", plot = rhvi3_wp_heat_threshold, 
       height = 4, width = 3.5, units = "in", dpi = 600) 

##########################################################################################
# Threshold for SOSE3
##########################################################################################

predicted_model_data_sose3 <- data.frame(wp = sose3$wp, ignition_delay = fitted(sose3_seg))

sose3_wp_ig_threshold <- ggplot(sose3, aes(x = wp, y = ignition_delay)) + 
  geom_line(data = predicted_model_data_sose3, size = 1.2) +
  geom_point(data = sose3_seg,  position = "jitter", alpha = 0.5) +
  dws_point +
  xlab("Water potential (MPa)") +
  ylab("Ignition delay time (s)") + 
  geom_vline(xintercept = confint(sose3_seg)[2], linetype = "dashed", size = 1) +
  geom_vline(xintercept = confint(sose3_seg)[3], linetype = "dashed", size = 1) +
  geom_rect(aes(xmin = confint(sose3_seg)[2], xmax = confint(sose3_seg)[3], ymin = -Inf, ymax = Inf),
            fill = "lightsteelblue", alpha = 0.005) +
  pubtheme +
  theme(axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 

ggsave("./results/sose3_wp_ig_threshold.pdf", plot = sose3_wp_ig_threshold, 
       height = 4, width = 3.5, units = "in", dpi = 600) 

predicted_model_data_sose3_heat <- data.frame(wp = sose3$wp, heat_release_j = fitted(sose3_heat_release_segmented))

sose3_wp_heat_threshold <- ggplot(sose3, aes(x = wp, y = heat_release_j)) + 
  geom_line(data = predicted_model_data_sose3_heat, size = 1.2) +
  geom_point(data = sose3_heat_release_segmented,  position = "jitter", alpha = 0.5) +
  dws_point +
  xlab("Water potential (MPa)") +
  ylab("Heat release (J)") + 
  geom_vline(xintercept = confint(sose3_heat_release_segmented)[2], linetype = "dashed", size = 1) +
  geom_vline(xintercept = confint(sose3_heat_release_segmented)[3], linetype = "dashed", size = 1) +
  geom_rect(aes(xmin = confint(sose3_heat_release_segmented)[2],
                xmax = confint(sose3_heat_release_segmented)[3], ymin = -Inf, ymax = Inf),
            fill = "lightsteelblue", alpha = 0.005) +
  pubtheme +
  theme(axis.text = element_text(size = axissz, face = "bold"),
        axis.title = element_text(size = textsize, face = "bold")) 

ggsave("./results/sose3_wp_heat_threshold.pdf", plot = sose3_wp_heat_threshold, 
       height = 4, width = 3.5, units = "in", dpi = 600) 

