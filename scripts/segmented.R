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

jupi <- final_data %>%
  filter(spcode == "JUPI")
  
dim(jupi) 

lm_jupi <- lm(ignition_delay ~ wp, data = jupi)

jupi_seg <- segmented::segmented(lm_jupi, seg.Z = ~ wp)

AIC(lm_jupi, jupi_seg) 

summary(jupi_seg) 

### Now fmc

lm_jupi_ig_cmc <- lm(ignition_delay ~ cmc, data = jupi)

jupi_seg_ig_cmc <- segmented::segmented(lm_jupi_ig_cmc, seg.Z = ~ cmc)

AIC(lm_jupi_ig_cmc, jupi_seg_ig_cmc)

lm_jupi_ig_lfmc <- lm(ignition_delay ~ lfmc, data = jupi)

jupi_seg_ig_lfmc <- segmented::segmented(lm_jupi_ig_lfmc, seg.Z = ~ lfmc)

AIC(lm_jupi_ig_lfmc, jupi_seg_ig_lfmc)

####################################################################################
# Now the wp vs heat release
###################################################################################

lm_heat_release_jupi <- lm(heat_release_j ~ wp, data = jupi)

jupi_heat_release_segmented <- segmented::segmented(lm_heat_release_jupi, seg.Z = ~ wp)

AIC(lm_heat_release_jupi, jupi_heat_release_segmented) 

# Now FMC

lm_heat_release_jupi_cmc <- lm(heat_release_j ~ cmc, data = jupi)

jupi_heat_release_segmented_cmc <- segmented::segmented(lm_heat_release_jupi_cmc, seg.Z = ~ cmc)

AIC(lm_heat_release_jupi_cmc, jupi_heat_release_segmented_cmc) 

lm_heat_release_jupi_lfmc <- lm(heat_release_j ~ lfmc, data = jupi)

jupi_heat_release_segmented_lfmc <- segmented::segmented(lm_heat_release_jupi_lfmc, seg.Z = ~ lfmc)

AIC(lm_heat_release_jupi_lfmc, jupi_heat_release_segmented_lfmc) 

###################################################################################
# JUAS
###################################################################################

juas <- final_data %>%
  filter(spcode == "JUAS")

dim(juas) 

lm_juas <- lm(ignition_delay ~ wp, data = juas)

juas_seg <- segmented::segmented(lm_juas, seg.Z = ~ wp)

AIC(lm_juas, juas_seg) 

### Now FMC

lm_juas_ig_cmc <- lm(ignition_delay ~ cmc, data = juas)

juas_seg_ig_cmc <- segmented::segmented(lm_juas_ig_cmc, seg.Z = ~ cmc)

AIC(lm_juas_ig_cmc, juas_seg_ig_cmc)

lm_juas_ig_lfmc <- lm(ignition_delay ~ lfmc, data = juas)

juas_seg_ig_lfmc <- segmented::segmented(lm_juas_ig_lfmc, seg.Z = ~ lfmc)

AIC(lm_juas_ig_lfmc, juas_seg_ig_lfmc)

####################################################################################
# Now the wp vs heat release
###################################################################################

lm_heat_release_juas <- lm(heat_release_j ~ wp, data = juas)

juas_heat_release_segmented <- segmented::segmented(lm_heat_release_juas, seg.Z = ~ wp)

AIC(lm_heat_release_juas, juas_heat_release_segmented) 

# Now FMC

lm_heat_release_juas_cmc <- lm(heat_release_j ~ cmc, data = juas)

juas_heat_release_segmented_cmc <- segmented::segmented(lm_heat_release_juas_cmc, seg.Z = ~ cmc)

AIC(lm_heat_release_juas_cmc, juas_heat_release_segmented_cmc) 

lm_heat_release_juas_lfmc <- lm(heat_release_j ~ lfmc, data = juas)

juas_heat_release_segmented_lfmc <- segmented::segmented(lm_heat_release_juas_lfmc, seg.Z = ~ lfmc)

AIC(lm_heat_release_juas_lfmc, juas_heat_release_segmented_lfmc) 

####################################################################################
# RHVI3
###################################################################################

rhvi3 <- final_data %>%
  filter(spcode == "RHVI3")

dim(rhvi3) 

lm_rhvi3 <- lm(ignition_delay ~ wp, data = rhvi3)

rhvi3_seg <- segmented::segmented(lm_rhvi3, seg.Z = ~ wp)

AIC(lm_rhvi3, rhvi3_seg) 

summary(rhvi3_seg) 

### Now cmc

lm_rhvi3_ig_cmc <- lm(ignition_delay ~ cmc, data = rhvi3)

rhvi3_seg_ig_cmc <- segmented::segmented(lm_rhvi3_ig_cmc, seg.Z = ~ cmc)

AIC(lm_rhvi3_ig_cmc, rhvi3_seg_ig_cmc)

lm_rhvi3_ig_lfmc <- lm(ignition_delay ~ lfmc, data = rhvi3)

rhvi3_seg_ig_lfmc <- segmented::segmented(lm_rhvi3_ig_cmc, seg.Z = ~ lfmc)

AIC(lm_rhvi3_ig_lfmc, rhvi3_seg_ig_lfmc)


####################################################################################
# Now the wp vs heat release
###################################################################################

lm_heat_release_rhvi3 <- lm(heat_release_j ~ wp, data = rhvi3)

rhvi3_heat_release_segmented <- segmented::segmented(lm_heat_release_rhvi3, seg.Z = ~ wp)

AIC(lm_heat_release_rhvi3, rhvi3_heat_release_segmented) 

# Now FMC

lm_heat_release_rhvi3_cmc <- lm(heat_release_j ~ cmc, data = rhvi3)

rhvi3_heat_release_segmented_cmc <- segmented::segmented(lm_heat_release_rhvi3_cmc, seg.Z = ~ cmc)

AIC(lm_heat_release_rhvi3_cmc, rhvi3_heat_release_segmented_cmc) 

lm_heat_release_rhvi3_lfmc <- lm(heat_release_j ~ lfmc, data = rhvi3)

rhvi3_heat_release_segmented_lfmc <- segmented::segmented(lm_heat_release_rhvi3_lfmc, seg.Z = ~ lfmc)

AIC(lm_heat_release_rhvi3_lfmc, rhvi3_heat_release_segmented_lfmc) 


####################################################################################
# SOSE3
###################################################################################

sose3 <- final_data %>%
  filter(spcode == "SOSE3")

dim(sose3) 

lm_sose3 <- lm(ignition_delay ~ wp, data = sose3)

sose3_seg <- segmented::segmented(lm_sose3, seg.Z = ~ wp)

AIC(lm_sose3, sose3_seg) 

summary(sose3_seg) 

### Now cmc

lm_sose3_ig_cmc <- lm(ignition_delay ~ cmc, data = sose3)

sose3_seg_ig_cmc <- segmented::segmented(lm_sose3_ig_cmc, seg.Z = ~ cmc)

AIC(lm_sose3_ig_cmc, sose3_seg_ig_cmc)

lm_sose3_ig_lfmc <- lm(ignition_delay ~ lfmc, data = sose3)

sose3_seg_ig_lfmc <- segmented::segmented(lm_sose3_ig_cmc, seg.Z = ~ lfmc)

AIC(lm_sose3_ig_lfmc, sose3_seg_ig_lfmc)

####################################################################################
# Now the wp vs heat release
###################################################################################

lm_heat_release_sose3 <- lm(heat_release_j ~ wp, data = sose3)

sose3_heat_release_segmented <- segmented::segmented(lm_heat_release_sose3, seg.Z = ~ wp)

AIC(lm_heat_release_sose3, sose3_heat_release_segmented) 

# Now FMC

lm_heat_release_sose3_cmc <- lm(heat_release_j ~ cmc, data = sose3)

sose3_heat_release_segmented_cmc <- segmented::segmented(lm_heat_release_sose3_cmc, seg.Z = ~ cmc)

AIC(lm_heat_release_sose3_cmc, sose3_heat_release_segmented_cmc) 

lm_heat_release_sose3_lfmc <- lm(heat_release_j ~ lfmc, data = sose3)

sose3_heat_release_segmented_lfmc <- segmented::segmented(lm_heat_release_sose3_lfmc, seg.Z = ~ lfmc)

AIC(lm_heat_release_sose3_lfmc, sose3_heat_release_segmented_lfmc) 

####################################################################################
# MATR3, only for heat release since its not sensitive on its ignition delay
###################################################################################

matr3 <- final_data %>%
  filter(spcode == "MATR3")

lm_heat_release_matr3 <- lm(heat_release_j ~ wp, data = matr3)

matr3_heat_release_segmented <- segmented::segmented(lm_heat_release_matr3, seg.Z = ~ wp)

AIC(lm_heat_release_matr3, matr3_heat_release_segmented) 

# Now FMC

lm_heat_release_matr3_cmc <- lm(heat_release_j ~ cmc, data = matr3)

matr3_heat_release_segmented_cmc <- segmented::segmented(lm_heat_release_matr3_cmc, seg.Z = ~ cmc)

AIC(lm_heat_release_matr3_cmc, matr3_heat_release_segmented_cmc) 

lm_heat_release_matr3_lfmc <- lm(heat_release_j ~ lfmc, data = matr3)

matr3_heat_release_segmented_lfmc <- segmented::segmented(lm_heat_release_matr3_cmc, seg.Z = ~ lfmc)

AIC(lm_heat_release_matr3_lfmc, matr3_heat_release_segmented_lfmc) 

###################################################################################
# PRGL2
###################################################################################
prgl2 <- final_data %>%
  filter(spcode == "PRGL2")

lm_heat_release_prgl2 <- lm(heat_release_j ~ wp, data = prgl2)

prgl2_heat_release_segmented <- segmented::segmented(lm_heat_release_prgl2, seg.Z = ~ wp)

AIC(lm_heat_release_prgl2, prgl2_heat_release_segmented) 

# Now FMC

lm_heat_release_prgl2_cmc <- lm(heat_release_j ~ cmc, data = prgl2)

prgl2_heat_release_segmented_cmc <- segmented::segmented(lm_heat_release_prgl2_cmc, seg.Z = ~ cmc)

AIC(lm_heat_release_prgl2_cmc, prgl2_heat_release_segmented_cmc) 

lm_heat_release_prgl2_lfmc <- lm(heat_release_j ~ lfmc, data = prgl2)

prgl2_heat_release_segmented_lfmc <- segmented::segmented(lm_heat_release_prgl2_lfmc, seg.Z = ~ lfmc)

AIC(lm_heat_release_prgl2_lfmc, prgl2_heat_release_segmented_lfmc) 



