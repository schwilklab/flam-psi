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


###############################################################################
# Cleaning the environment, leaving the alldatas for exploratory figures
###############################################################################

rm("burn_trials", "flam_loadings", "hobos_wider",
   "pca_data")

