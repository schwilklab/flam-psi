# flam_pca.R

# Shrub flammability project using summer 2022 data. PCA analysis uses prcomp()


###############################################################################
# AM: Merging the hobos and all data 
# PCA analysis. Don;t use massloss because it is covaries too strongly with wp
# (more mass to lose)_
###############################################################################

alldata <- left_join(alldata, hobos_wider)

pca_data <- alldata %>%
  dplyr::select(sample_id, heat_release_j, ##, massloss,
         vol_burned, flame_height, flame_duration, dur_100,
         peak_temp, degsec_100, ignition_delay) %>%
  filter(complete.cases(.))


dim(pca_data)

any(is.na(pca_data)) 

flam_pca <- prcomp(pca_data[,-1], scale=TRUE)

summary(flam_pca)

flam_loadings <- flam_pca$rotation[ ,1:2] 

flam_loadings

biplot(flam_pca)


###############################################################################
## Assigning PC1 and PC2 to pca data and  then merging with alldata
## to get the final data which will be used for rest of the 
# the statistical part and figures
###############################################################################

pca_data$PC1 <- flam_pca$x[ ,1]
pca_data$PC2 <- flam_pca$x[ ,2]

pca_data <- pca_data %>%
  dplyr::select(sample_id, PC1, PC2)


final_data <- left_join(alldata, pca_data) %>%
  filter(sample_id %in% pca_data$sample_id)

dim(final_data)

filtered_data <- final_data %>%
  filter(self_ignition != 1) %>%
  filter(sample_id %in% filtered_sample_id)
  
dim(filtered_data)



###############################################################################
# 2024
###############################################################################

alldata_2024 <- left_join(alldata_2024, hobos_wider_2024)

pca_data_2024 <- alldata_2024 %>%
  dplyr::select(sample_id, heat_release_j, ##, massloss,
                vol_burned, flame_height, flame_duration, dur_100,
                peak_temp, degsec_100, ignition_delay) %>%
  filter(complete.cases(.))


dim(pca_data_2024)

any(is.na(pca_data_2024)) 

flam_pca_2024 <- prcomp(pca_data_2024[,-1], scale=TRUE)

summary(flam_pca_2024)

flam_loadings_2024 <- flam_pca_2024$rotation[ ,1:2] 

biplot(flam_pca_2024)


pca_data_2024$PC1 <- flam_pca_2024$x[ ,1]
pca_data_2024$PC2 <- flam_pca_2024$x[ ,2]

pca_data_2024 <- pca_data_2024 %>%
  dplyr::select(sample_id, PC1, PC2)


final_data_2024 <- left_join(alldata_2024, pca_data_2024) %>%
  filter(sample_id %in% pca_data_2024$sample_id) %>%
  filter(sample_id != "FRT19")

dim(final_data_2024)


###############################################################################
# Cleaning the environment, leaving the alldata for exploratory figures
###############################################################################

rm("burn_trials","burn_trials_2024", "flam_loadings", "flam_loadings_2024", "hobos_wider",
   "pca_data", "hobos_wider_2024", "pca_data_2024", "filtered_sample_id")

