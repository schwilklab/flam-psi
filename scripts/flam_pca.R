# flam_pca.R

# Shrub flammability project using summer 2022 data. PCA analysis uses prcomp()
# for PCA and factoextra for figures

library(factoextra)

###############################################################################
# PCA analysis. Don;t use massloss because it is covaries too strongly with wp
# (more mass to lose)_
###############################################################################


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

##############################################################################
## Scree plot, eigenvalue and variables info
###############################################################################

eig_val <- get_eigenvalue(flam_pca) 
eig_val
variables_info <- get_pca_var(flam_pca) # Variables information
variables_info$coord[ ,1:2] # Coordinates of variables
head(variables_info$contrib) # Contributions of variables

###############################################################################
# Assigning PC1 to pca_data_2022 and then merging with alldata_2022
# data set for doing rest of the analysis.
###############################################################################

pca_data$PC1 <- flam_pca$x[ ,1]
pca_data$PC2 <- flam_pca$x[ ,2]



###############################################################################
# Cleaning the environment
###############################################################################

## rm( "flam_pca_2022", "flam_loadings",
##    "eig_val", "variables_info" ,"contributor_pc1_2022",
##    "contributor_pc2_2022", 
##    "var_contr_by_cos2")

