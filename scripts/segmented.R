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


lm_jupi <- lm(ignition_delay ~ wp, data = segmented_jupi_ig)

jupi_seg <- segmented::segmented(lm_jupi, seg.Z = ~ wp)


AIC(lm_jupi, jupi_seg) # lm 522.9, segmented 523.9 (Linear model is better than segmented)

####################################################################################
# Now the wp vs temperature integration for Juniperus
###################################################################################

lm_degsec_jupi <- lm(degsec_100 ~ wp, data = segmented_jupi_degsec)

jupi_degsec_segmented <- segmented::segmented(lm_degsec_jupi, seg.Z = ~ wp)

AIC(lm_degsec_jupi, jupi_degsec_segmented) # linear model is better than segmented model
# Linear model is 677.7 and segmented is 681.3

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


lm_prgl2 <- lm(ignition_delay ~ wp, data = segmented_prgl2_ig)
prgl2_seg <- segmented::segmented(lm_prgl2, seg.Z = ~ wp)

summary(prgl2_seg)


AIC(lm_prgl2, prgl2_seg) # linear 162.8, segmented 157.2 (segmeneted model is better)


####################################################################################
# Now the wp vs temerature integration for Prosopis glandulosa
###################################################################################

lm_degsec_prgl2 <- lm(degsec_100 ~ wp, data = segmented_prgl2_degsec) 

prgl2_degsec_seg <- segmented::segmented(lm_degsec_prgl2, seg.Z = ~ wp)


AIC(lm_degsec_prgl2, prgl2_degsec_seg) # Linear 786.4, segmented 788.0 (linear is better)


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


lm_ig_rhtr <- lm(ignition_delay ~ wp, data = segmented_rhtr_ig)

rhtr_ig_seg <- segmented::segmented(lm_ig_rhtr, seg.Z = ~ wp)

AIC(lm_ig_rhtr, rhtr_ig_seg) # linear model 209.9, segmented 212.6 , linear model is better


####################################################################################
# Now the wp vs temerature integration for Rhus trilobata
###################################################################################

lm_degsec_rhtr <- lm(degsec_100 ~ wp, data = segmented_rhtr_degsec)

rhtr_degsec_seg <- segmented::segmented(lm_degsec_rhtr, seg.Z = ~ wp)

AIC(lm_degsec_rhtr, rhtr_degsec_seg) # Linear 700.6, segmented 702.7 (Linear model is better)


##########################################################################################
# Cleaning the environment
#########################################################################################

rm("lm_degsec_rhtr", "rhtr_degsec_seg", "lm_ig_rhtr", "rhtr_ig_seg",
   "segmented_rhtr_degsec", "segmented_rhtr_ig", "lm_degsec_prgl2", "prgl2_degsec_seg",
   "segmented_prgl2_degsec", "segmented_jupi_ig",  "segmented_jupi_degsec",
   "lm_jupi", "jupi_seg", "lm_degsec_jupi", "jupi_degsec_segmented")
