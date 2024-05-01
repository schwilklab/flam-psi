library(segmented)

#############################################################################
# This script is for segmented analysis
# will consider only those 
# segmented model which is better (lower AIC value) than their 
# corresponding linear model
#############################################################################

# Source of the codes: https://rpubs.com/MarkusLoew/12164

#############################################################################
# Intially JUPI
#############################################################################

segmented_jupi_ig <- final_data %>%
  filter(spcode == "JUPI") %>%
  filter(self_ignition != 1)

dim(segmented_jupi_ig) 

segmented_jupi_degsec <- filtered_data %>%
  filter(spcode == "JUPI") %>%
  filter(self_ignition != 1)

dim(segmented_jupi_degsec)

lm_jupi <- lm(ignition_delay ~ wp, data = segmented_jupi_ig)

jupi_seg <- segmented::segmented(lm_jupi, seg.Z = ~ wp)

AIC(lm_jupi, jupi_seg) 

summary(jupi_seg) # segmented is better

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
  filter(spcode == "PRGL2")

dim(segmented_prgl2_ig) # 35

lm_prgl2 <- lm(ignition_delay ~ wp, data = segmented_prgl2_ig)

prgl2_seg <- segmented::segmented(lm_prgl2, seg.Z = ~ wp)

summary(prgl2_seg)

AIC(lm_prgl2, prgl2_seg) # segmented is better

####################################################################################
# Now the wp vs temerature integration for Prosopis glandulosa
###################################################################################

lm_degsec_prgl2_wp <- lm(degsec_100 ~ wp, data = segmented_prgl2_ig) 

prgl2_degsec_seg_wp <- segmented::segmented(lm_degsec_prgl2_wp, seg.Z = ~ wp)

AIC(lm_degsec_prgl2_wp, prgl2_degsec_seg_wp) # lm is better

###########################################################################
# Rhus trilobata, initially ignition delay
###########################################################################

segmented_rhtr_ig <- final_data %>%
  filter(spcode == "RHTR")

dim(segmented_rhtr_ig)

lm_ig_rhtr <- lm(ignition_delay ~ wp, data = segmented_rhtr_ig)

rhtr_ig_seg <- segmented::segmented(lm_ig_rhtr, seg.Z = ~ wp)

AIC(lm_ig_rhtr, rhtr_ig_seg) # lm is better than seg

####################################################################################
# Now the wp vs temerature integration for Rhus trilobata
###################################################################################

lm_degsec_rhtr_wp <- lm(degsec_100 ~ wp, data = segmented_rhtr_ig)

rhtr_degsec_seg_wp <- segmented::segmented(lm_degsec_rhtr_wp, seg.Z = ~ wp)

AIC(lm_degsec_rhtr_wp, rhtr_degsec_seg_wp) # lm is better than seg

