
## Figures and stats for ESA 2024 presentation
## D. Schwilk

library(tidyr)
library(broom)
library(purrr)

ESA_data <-  final_data_2024 %>%
  filter(wp >= -2 & sex !="female")

# Flammability 2 dimensional and heat release dominate one in these data. But
# heat release and ignition

high_fmc_sum <- final_data_2024 %>%
  filter(wp >= -2 & sex !="female") %>%
  group_by(spcode, display_name) %>%
  summarize(across(c(ignition_delay,heat_release_j), mean))

high_fmc_sum

ESA_ign_heat <- ggplot(high_fmc_sum, aes(ignition_delay, heat_release_j/1000)) +
  #estfit +
  geom_point(aes(color=display_name), size=2, alpha=0.9, shape=16) +
  xlab("Ignition delay (s)") +
  ylab("Heat release (kJ)") +
  scale_color_brewer(palette = "Paired") + #scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.74,0.45),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize-4))
ggsave("./results/ESA_ign_heat.pdf", plot=ESA_ign_heat, width=col1*1.5, height=col1, units="cm")


## Individual level figures

## Sensitivity of ignition to WP varies by species
# 2023:

# WP-FM relationship varies across species. Some show slow change in FMC with WP
ESA_wp_fm1 <- ggplot(alldata, aes(wp, fmc, color=display_name)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Fuel moisture content (%)") +
  scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.28,0.85),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))
ggsave("./results/ESA_wp_fm1.pdf", plot=ESA_wp_fm1, width=col1*1.5, height=col1, units="cm")




## 2024
ESA_wp_ign <- ggplot(filter(final_data_2024, ignition_delay <= 60 & wp > -8 & spcode !="JUPI"),
               aes(wp, ignition_delay, color=display_name)) +
  geom_point(size=2, alpha=0.9, shape=16, position=position_jitter(height=0.2)) +
  geom_smooth(method="lm",se = FALSE, linewidth=1) +
  xlab("Water potential (Mpa)") +
  ylab("Ignition delay (s)") +
 scale_color_brewer(palette = "Paired") +
  pubtheme +
  theme(legend.position = c(0.2,0.76),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = textsize-3))
ESA_wp_ign
ggsave("./results/ESA_wp_ign.pdf", plot=ESA_wp_ign, width=col1*1.2, height=col1, units="cm")


## fig6_2024 <- ggplot(filter(final_data_2024, ignition_delay <= 60 & wp > -8 & spcode !="JUPI"),
##                aes(lfmc, ignition_delay, color=display_name)) +
##   geom_point(size=2, alpha=0.9, shape=16) + #, position=position_jitter(height=0.2)) +
##     geom_smooth(method="lm",se = FALSE, linewidth=1) +
##   xlab("Fuel moisture content (%)") +
##   ylab("Ignition delay (s)") +
##  # scale_color_manual(values = schwilkcolors) +
##   pubtheme +
##   theme(legend.position = c(0.2,0.76),
##         legend.text = element_text(face="italic"),
##         legend.title = element_blank(),
##         axis.text = element_text(size = textsize-3))
## fig6_2024
## ggsave("./results/fig6_2024.pdf", plot=fig6_2024, width=col1, height=col1, units="cm")




## Species level summaries

names(final_data_2024)

## attempt for one

temp <- final_data_2024 %>% filter(sex != "female") %>%
  filter(ignition_delay <= 20) %>%
  filter(spcode=="JUPI")

lm(ignition_delay ~ wp, data = temp)

esa_data <- final_data_2024 %>%
  filter(sex != "female" & spcode != "JUPI") %>%
  filter(ignition_delay <= 20)

## Sensitivity to fuel moisture parameters
species_ign_sensitivity <- esa_data %>%
  nest(data = c(-spcode, -display_name)) %>%
  mutate(fit = map(data, ~ lm(ignition_delay ~ wp, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term=="wp") %>%
  dplyr::select(spcode, display_name, ign_sens = estimate)

species_ign_sensitivity
## Now add in other trait summaries
## trait_sum <- esa_data %>%
##   group_by(spcode, display_name) %>%
##   summarize(across(c()), mean)

pv_sum <- read_csv("./data/2024/pv_curve_summary.csv") %>%
  group_by(spcode) %>%
  mutate(mc_tlp = saturated_moisture_content * rwc_tlp * 0.01) %>%
  summarize(across(c(tlp, rwc_tlp, mc_tlp, capacitance_above_tlp, capacitance_below_tlp, modulus_elasticity), mean))

pv_sum

species_sum <- left_join(species_ign_sensitivity, pv_sum)
species_sum

ggplot(filter(species_sum, spcode!="JUAS"), aes(capacitance_above_tlp, ign_sens)) +
  geom_point()

ggplot(species_sum, aes(capacitance_below_tlp, ign_sens)) +
  geom_point()


ggplot(species_sum, aes(modulus_elasticity, ign_sens)) +
  geom_point()

ggplot(filter(species_sum, spcode!="JUAS"),
       aes(mc_tlp, ign_sens)) +
  geom_point()


ggplot(filter(species_sum, spcode!="JUAS"), aes(rwc_tlp, ign_sens,color=spcode)) +
  geom_point()



## Using all data as a measure of shoot "capacitance"

## Sensitivity of fuel moisture to wp
species_wp_fmc <- esa_data %>%
  nest(data = -spcode) %>%
  mutate(fit = map(data, ~ lm(lfmc ~ wp, data = .x)),
         tidied = map(fit, tidy)) %>%
  unnest(tidied) %>%
  filter(term=="wp") %>%
  dplyr::select(spcode, wp_sens = estimate)

species_sum <- left_join(species_sum, species_wp_fmc)

ESA_shoot_capac <- ggplot(species_sum, aes(1/wp_sens, ign_sens, color=display_name)) +
   geom_point(aes(color=display_name), size=2, alpha=0.9, shape=16) +
  xlab(expression(paste("Shoot capacitance ", MPa^{-1}))) +
  ylab(expression(paste("Ignitibility response to ", Psi, " (",s %.% MPa^{-1}, ")"))) +
  scale_color_brewer(palette = "Paired") +
  pubtheme +
  theme(legend.position = c(0.2,0.76),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = textsize-3))
ggsave("./results/ESA_shoot_capac.pdf", plot=ESA_shoot_capac, width=col1*1.2, height=col1, units="cm")


