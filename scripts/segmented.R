library(segmented)

#############################################################################
# This script is for segmented analysis
# will consider only those 
# segmented model which is better (lower AIC value) than their 
# corresponding linear model
#############################################################################

# Source of the codes: https://rpubs.com/MarkusLoew/12164

#############################################################################
# Initially we two subsets of final_data for each species, one is for ignition delay
# and another one is for temperature integration of samples which ignited withing 20 seconds
# Initially Juniperus!
#############################################################################

segmented_jupi_ig <- final_data %>%
  filter(ignition_delay != 0) %>%
  filter(spcode == "JUPI")

dim(segmented_jupi_ig) # 63


segmented_jupi_degsec <- final_data %>%
  filter(ignition_delay <= 20) %>%
  filter(spcode == "JUPI")

dim(segmented_jupi_degsec) # 28

range(segmented_jupi_degsec$ignition_delay) # Making sure the range of ignition delay is okay

segmented_jupi_ig$ignition_delay <- log(segmented_jupi_ig$ignition_delay + 1)

lm_jupi <- lm(ignition_delay ~ wp, data = segmented_jupi_ig)

jupi_seg <- segmented::segmented(lm_jupi, seg.Z = ~ wp)


AIC(lm_jupi, jupi_seg) 

####################################################################################
# Now the wp vs temperature integration for Juniperus
###################################################################################

lm_degsec_jupi <- lm(degsec_100 ~ wp, data = segmented_jupi_degsec)

jupi_degsec_segmented <- segmented::segmented(lm_degsec_jupi, seg.Z = ~ wp)

AIC(lm_degsec_jupi, jupi_degsec_segmented) # lm is better than segmented

###################################################################################
# Prosopis glandulosa, initially ignition delay
###################################################################################

segmented_prgl2_ig <- final_data %>%
  filter(ignition_delay != 0) %>%
  filter(spcode == "PRGL2")

dim(segmented_prgl2_ig) # 35


segmented_prgl2_degsec <- final_data %>%
  filter(ignition_delay <= 20) %>%
  filter(spcode == "PRGL2")

dim(segmented_prgl2_degsec) # 35

range(segmented_prgl2_degsec$ignition_delay)

segmented_prgl2_ig$ignition_delay <- log(segmented_prgl2_ig$ignition_delay + 1)


lm_prgl2 <- lm(ignition_delay ~ wp, data = segmented_prgl2_ig)

prgl2_seg <- segmented::segmented(lm_prgl2, seg.Z = ~ wp)

summary(prgl2_seg)


AIC(lm_prgl2, prgl2_seg) 

####################################################################################
# Now the wp vs temerature integration for Prosopis glandulosa
###################################################################################

lm_degsec_prgl2 <- lm(degsec_100 ~ wp, data = segmented_prgl2_degsec) 

prgl2_degsec_seg <- segmented::segmented(lm_degsec_prgl2, seg.Z = ~ wp)


AIC(lm_degsec_prgl2, prgl2_degsec_seg) # lm is better than seg


###########################################################################
# Rhus trilobata, initially ignition delay
###########################################################################

segmented_rhtr_ig <- final_data %>%
  filter(ignition_delay != 0) %>%
  filter(spcode == "RHTR")

dim(segmented_rhtr_ig)


segmented_rhtr_degsec <- final_data %>%
  filter(ignition_delay <= 20) %>%
  filter(spcode == "RHTR")

dim(segmented_rhtr_degsec)

range(segmented_rhtr_degsec$ignition_delay)

segmented_rhtr_ig$ignition_delay <- log(segmented_rhtr_ig$ignition_delay + 1)


lm_ig_rhtr <- lm(ignition_delay ~ wp, data = segmented_rhtr_ig)

rhtr_ig_seg <- segmented::segmented(lm_ig_rhtr, seg.Z = ~ wp)

AIC(lm_ig_rhtr, rhtr_ig_seg) # lm is better than seg


####################################################################################
# Now the wp vs temerature integration for Rhus trilobata
###################################################################################

lm_degsec_rhtr <- lm(degsec_100 ~ wp, data = segmented_rhtr_degsec)

rhtr_degsec_seg <- segmented::segmented(lm_degsec_rhtr, seg.Z = ~ wp)

AIC(lm_degsec_rhtr, rhtr_degsec_seg) # lm is better than seg



#######################################################################################
# Now fmc
######################################################################################

segmented_jupi_fmcig <- final_data %>%
  filter(spcode == "JUPI") %>%
  filter(self_ignition != 1)

dim(segmented_jupi_fmcig)


segmented_jupi_fmc_degsec <- filtered_data %>%
  filter(spcode == "JUPI")

dim(segmented_jupi_fmc_degsec)


lm_jupi_fmc <- lm(ignition_delay ~ fmc, data = segmented_jupi_fmcig)

jupi_seg_fmc <- segmented::segmented(lm_jupi_fmc, seg.Z = ~ fmc)


AIC(lm_jupi_fmc, jupi_seg_fmc) 

####################################################################################
# Now the  temperature integration 
###################################################################################

lm_fmc_degsec_jupi <- lm(degsec_100 ~ fmc, data = segmented_jupi_fmc_degsec)

jupi_fmc_degsec_segmented <- segmented::segmented(lm_fmc_degsec_jupi, seg.Z = ~ fmc)

AIC(lm_fmc_degsec_jupi, jupi_fmc_degsec_segmented) 



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

##########################################################################################
# Cleaning the environment
#########################################################################################

rm("lm_degsec_rhtr", "rhtr_degsec_seg", "lm_ig_rhtr", "rhtr_ig_seg",
   "segmented_rhtr_degsec", "segmented_rhtr_ig", "lm_degsec_prgl2", "prgl2_degsec_seg",
   "segmented_prgl2_degsec",  "segmented_jupi_degsec", "lm_degsec_jupi", "jupi_degsec_segmented")
