## Check with 30 DEM based Factors and Others

library(sf)
library(tidyverse)
library(randomForest)

library(caret)
library(pROC)
library(e1071)
library(iml)

# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
glimpse(Non_landslides)
library(tidyverse)
library(dplyr)
landslides <- landslides %>% select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V,
                                    TRI_ASTER, RO_ASTER, TPI_ASTER,
                                    Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA,  
                                    TRI_EDNA, RO_EDNA, TPI_EDNA,
                                    Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, 
                                    TRI_SRTM, RO_SRTM, TPI_SRTM,
                                    Imprevious , LULC_chang, Geology, Rain, NDVI, Landuse)
Non_landslides <- Non_landslides %>% select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V,
                                            TRI_ASTER, RO_ASTER, TPI_ASTER,
                                            Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA,  
                                            TRI_EDNA, RO_EDNA, TPI_EDNA,
                                            Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, 
                                            TRI_SRTM, RO_SRTM, TPI_SRTM,
                                            Imprevious , LULC_chang, Geology, Rain,  NDVI, Landuse)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))

# Scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
glimpse(Data_m)
Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
Data_m$V <- as.factor(Data_m$V)

#Scale the Variables
# Load necessary library
library(dplyr)

# Function to scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

Data_m <- Data_m %>%
  mutate(across(.cols = -c(V      ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
#Data_m$Soil       <- as.factor(Data_m$Soil        )
Data_m$Geology <- as.factor(Data_m$Geology )
Data_m$Landuse <- as.factor(Data_m$Landuse )
Data_m$LULC_chang <- as.factor(Data_m$LULC_chang )
Data_m$V <- as.factor(Data_m$V)
# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

 
# Summarize the model
print(rf_model_36_All )

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_36_All , newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_36_All , newdata = testData, type = "response")

# Evaluate the model
confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
print(confusion_matrix_rf)

# Calculate ROC curve and AUC
roc_curve_rf <- roc(testData$V, rf_pred_prob)
auc_value_rf <- auc(roc_curve_rf)
print(paste("AUC for Random Forest:", auc_value_rf))
auc_1m <- as.data.frame(auc_value_rf )
# Plot ROC curve using ggplot2
roc_data_rf <- data.frame(
  specificity = rev(roc_curve_rf$specificities),
  sensitivity = rev(roc_curve_rf$sensitivities)
)

ggplot(roc_data_rf, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Random Forest", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

# Get variable importance
importance_rf <- importance(rf_model_36_All  )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1

## Map Generation

# Train a Random Forest model
rf_model_36_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)

library(raster)
library(sp)
library(caret)  # Assuming you used caret for model training

## SRTM

Slop_SRTM <- raster("D:/DEM/Slope_SRTM.tif")
Elev_SRTM <- raster("D:/DEM/SRTM.tif")
Asp_SRTM  <- raster("D:/DEM/Aspect_SRTM.tif")
PL_SRTM   <- raster("D:/DEM/Plan_SRTM.tif")
PR_SRTM   <- raster("D:/DEM/Profile_SRTM.tif")
TRI_SRTM  <- raster("D:/DEM/TRI_SRTM.tif")
RO_SRTM   <- raster("D:/DEM/Roughness_SRTM.tif")
TPI_SRTM  <- raster("D:/DEM/TPI_SRTM.tif")
PL_SRTM <- projectRaster(PL_SRTM , crs = crs(Slop_SRTM))
PR_SRTM <- projectRaster(PR_SRTM , crs = crs(Slop_SRTM))

## ASPECT

Slop_ASTER <- raster("D:/DEM/Slope_ASTER.tif")
Elev_ASTER <- raster("D:/DEM/ASTER_30m.tif")
Asp_ASTER  <- raster("D:/DEM/Aspect_ASTER.tif")
PL_ASTER   <- raster("D:/DEM/Plan_ASTER.tif")
PR_ASTER   <- raster("D:/DEM/Profile_ASTER.tif")
TRI_ASTER  <- raster("D:/DEM/TRI_ASTER.tif")
RO_ASTER   <- raster("D:/DEM/Roughness_ASTER.tif")
TPI_ASTER  <- raster("D:/DEM/TPI_ASTER.tif")
PL_ASTER <- projectRaster(PL_ASTER , crs = crs(Slop_SRTM))
PR_ASTER <- projectRaster(PR_ASTER, crs = crs(Slop_SRTM))

## EDNA

Slop_EDNA <- raster("D:/DEM/Slope_EDNA.tif")
Elev_EDNA <- raster("D:/DEM/EDNA.tif")
Asp_EDNA  <- raster("D:/DEM/Aspect_EDNA.tif")
PL_EDNA   <- raster("D:/DEM/Plan_EDNA.tif")
PR_EDNA  <- raster("D:/DEM/Profile_EDNA.tif")
TRI_EDNA  <- raster("D:/DEM/TRI_EDNA.tif")
RO_EDNA   <- raster("D:/DEM/Roughness_EDNA.tif")
TPI_EDNA  <- raster("D:/DEM/TPI_EDNA.tif")
PL_EDNA <- projectRaster(PL_EDNA , crs = crs(Slop_SRTM))
PR_EDNA <- projectRaster(PR_EDNA, crs = crs(Slop_SRTM))
list.files("D:/DEM/Other Factors/")
Landuse <- raster("D:/DEM/Other Factors/Landuse.tif")

Geology= raster("D:/Cheeky Scientist/Factors/Geologic Formation.tif")
Imprevious= raster("D:/Cheeky Scientist/Factors/Imprevious.tif")
LULC_chang= raster("D:/Cheeky Scientist/Factors/LULC_change.tif")
NDVI= raster("D:/Cheeky Scientist/Factors/NDVI.tif")
Rain= raster("D:/Cheeky Scientist/Factors/Rain.tif")
plot(Rain)
crs_Slop_SRTM <- crs(Slop_SRTM)

Landuse <- projectRaster(Landuse, crs = crs_Slop_SRTM)
Geology <- projectRaster(Geology, crs = crs_Slop_SRTM)
Imprevious <- projectRaster(Imprevious, crs = crs_Slop_SRTM)
LULC_chang <- projectRaster(LULC_chang, crs = crs_Slop_SRTM)
NDVI <- projectRaster(NDVI, crs = crs_Slop_SRTM)
Rain <- projectRaster(Rain, crs = crs_Slop_SRTM)
## Resampling

Landuse=resample(Landuse,Slop_SRTM, resample='bilinear')
Geology=resample(Geology,Slop_SRTM, resample='bilinear')
Imprevious=resample(Imprevious,Slop_SRTM, resample='bilinear')
LULC_chang=resample(LULC_chang,Slop_SRTM, resample='bilinear')
Rain=resample(Rain,Slop_SRTM, resample='bilinear')
NDVI=resample(NDVI,Slop_SRTM, resample='bilinear')
## SRTM

Slop_SRTM=resample(Slop_SRTM,Slop_SRTM, resample='bilinear')
Elev_SRTM=resample(Elev_SRTM,Slop_SRTM, resample='bilinear')
Asp_SRTM=resample(Asp_SRTM,Slop_SRTM, resample='bilinear')
PL_SRTM=resample(PL_SRTM,Slop_SRTM, resample='bilinear')
PR_SRTM=resample(PR_SRTM,Slop_SRTM, resample='bilinear')
TRI_SRTM=resample(TRI_SRTM,Slop_SRTM, resample='bilinear')
RO_SRTM=resample(RO_SRTM,Slop_SRTM, resample='bilinear')
TPI_SRTM=resample(TPI_SRTM,Slop_SRTM, resample='bilinear')

## ASTER
Slop_ASTER=resample(Slop_ASTER,Slop_SRTM, resample='bilinear')
Elev_ASTER=resample(Elev_ASTER,Slop_SRTM, resample='bilinear')
Asp_ASTER=resample(Asp_ASTER,Slop_SRTM, resample='bilinear')
PL_ASTER=resample(PR_ASTER,Slop_SRTM, resample='bilinear')
PR_ASTER=resample(PR_ASTER,Slop_SRTM, resample='bilinear')
TRI_ASTER=resample(TRI_ASTER,Slop_SRTM, resample='bilinear')
RO_ASTER=resample(RO_ASTER,Slop_SRTM, resample='bilinear')
TPI_ASTER=resample(TPI_ASTER,Slop_SRTM, resample='bilinear')

## EDNA

Slop_EDNA=resample(Slop_EDNA,Slop_SRTM, resample='bilinear')
Elev_EDNA=resample(Elev_EDNA,Slop_SRTM, resample='bilinear')
Asp_EDNA=resample(Asp_EDNA,Slop_SRTM, resample='bilinear')
PL_EDNA=resample(PL_EDNA,Slop_SRTM, resample='bilinear')
PR_EDNA=resample(PR_EDNA,Slop_SRTM, resample='bilinear')
TRI_EDNA=resample(TRI_EDNA,Slop_SRTM, resample='bilinear')
RO_EDNA=resample(RO_EDNA,Slop_SRTM, resample='bilinear')
TPI_EDNA=resample(TPI_EDNA,Slop_SRTM, resample='bilinear')

## Extent 

Landuse<- crop(Landuse, extent(Slop_SRTM))
Geology <- crop(Geology, extent(Slop_SRTM))
Imprevious <- crop(Imprevious, extent(Slop_SRTM))
LULC_chang <- crop(LULC_chang, extent(Slop_SRTM))
Rain <- crop(Rain, extent(Slop_SRTM))
NDVI <- crop(NDVI, extent(Slop_SRTM))

Slop_SRTM <- crop(Slop_SRTM, extent(Slop_SRTM))
Elev_SRTM <- crop(Elev_SRTM, extent(Slop_SRTM))
Asp_SRTM  <- crop(Asp_SRTM, extent(Slop_SRTM))
PL_SRTM   <- crop(PL_SRTM, extent(Slop_SRTM))
PR_SRTM   <- crop(PR_SRTM, extent(Slop_SRTM))
TRI_SRTM  <- crop(TRI_SRTM, extent(Slop_SRTM))
RO_SRTM   <- crop(RO_SRTM, extent(Slop_SRTM))
TPI_SRTM  <- crop(TPI_SRTM, extent(Slop_SRTM))
## ASTER
Slop_ASTER <- crop(Slop_ASTER, extent(Slop_SRTM))
Elev_ASTER <- crop(Elev_ASTER, extent(Slop_SRTM))
Asp_ASTER  <- crop(Asp_ASTER, extent(Slop_SRTM))
PL_ASTER  <- crop(PL_ASTER, extent(Slop_SRTM))
PR_ASTER  <- crop(PR_ASTER, extent(Slop_SRTM))
TRI_ASTER  <- crop(TRI_ASTER, extent(Slop_SRTM))
RO_ASTER  <- crop(RO_ASTER, extent(Slop_SRTM))
TPI_ASTER <- crop(TPI_ASTER, extent(Slop_SRTM))


## EDNA
Slop_EDNA <- crop(Slop_EDNA, extent(Slop_SRTM))
Elev_EDNA <- crop(Elev_EDNA, extent(Slop_SRTM))
Asp_EDNA  <- crop(Asp_EDNA, extent(Slop_SRTM))
PL_EDNA  <- crop(PL_EDNA, extent(Slop_SRTM))
PR_EDNA  <- crop(PR_EDNA, extent(Slop_SRTM))
TRI_EDNA  <- crop(TRI_EDNA, extent(Slop_SRTM))
RO_EDNA  <- crop(RO_EDNA, extent(Slop_SRTM))
TPI_EDNA<- crop(TPI_EDNA, extent(Slop_SRTM))



# Stack all predictor rasters
predictor_stack <- stack(Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, TRI_SRTM, RO_SRTM, TPI_SRTM,
                         Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, TRI_ASTER, RO_ASTER, TPI_ASTER,
                         Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, TRI_EDNA, RO_EDNA, TPI_EDNA, Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)
names(predictor_stack) <- c("Slop_SRTM", "Elev_SRTM", "Asp_SRTM", "PL_SRTM", "PR_SRTM", "TRI_SRTM", "RO_SRTM", "TPI_SRTM",
                            "Slop_ASTER", "Elev_ASTER", "Asp_ASTER", "PL_ASTER", "PR_ASTER", "TRI_ASTER", "RO_ASTER", "TPI_ASTER",
                            "Slop_EDNA", "Elev_EDNA", "Asp_EDNA", "PL_EDNA", "PR_EDNA", "TRI_EDNA", "RO_EDNA", "TPI_EDNA", "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology")


# Train a Random Forest model (final model)
rf_model_36_All_Final<- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)




# Train a Random Forest model (final model)
rf_model_36_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 2, importance = TRUE)

# Make predictions using the trained Random Forest model
predicted_raster <- predict(predictor_stack, rf_model_36_All_Final, type = "response")

# For probability of landslide
probability_raster <- predict(predictor_stack, rf_model_36_All_Final, type = "prob", index = 2)

# Save the predicted raster
writeRaster(predicted_raster, filename = "D:/DEM/predicted_32_All_Final.tif", format = "GTiff", overwrite = TRUE)

# Save the probability raster
writeRaster(probability_raster, filename = "D:/DEM/probability_32_All_Final.tif", format = "GTiff", overwrite = TRUE)




# # Make predictions using the trained Random Forest model
# predictions <- predict(rf_model_36_All_Final, predictor_df, type = "response")
# probability_predictions <- predict(rf_model_36_All_Final, predictor_df, type = "prob")[,2]
# 
# # Add predictions to the dataframe
# predictor_df$Prediction <- predictions
# predictor_df$Probability <- probability_predictions
# 
# 
# # Create a raster for the predictions
# prediction_raster <- rasterFromXYZ(predictor_df[, c("x", "y", "Prediction")])
# probability_raster <- rasterFromXYZ(predictor_df[, c("x", "y", "Probability")])
# 
# # Assign the correct projection to the raster
# proj4string(prediction_raster) <- CRS(projection(Slop_SRTM))
# proj4string(probability_raster) <- CRS(projection(Slop_SRTM))
# 
# # Save the predicted raster
# writeRaster(prediction_raster, filename = "D:/DEM/predicted_RF_32_All.tif", format = "GTiff", overwrite = TRUE)
# writeRaster(probability_raster, filename = "D:/DEM/probability_RF_32_All.tif", format = "GTiff", overwrite = TRUE)
# 
# # Plot the probability raster
# plot(probability_raster, main = "Landslide Probability Map")
# 
# 
# # Make predictions using the trained Random Forest model
# predicted_raster <- predict(predictor_stack, rf_model_36_All_Final, type = "response")
# 
# # For probability of landslide
# probability_raster <- predict(predictor_stack, rf_model_36_All_Final, type = "prob", index = 2)
# 
# # Save the predicted raster
# writeRaster(predicted_raster, filename = "D:/DEM/predicted_RF_32_All.tif", format = "GTiff", overwrite = TRUE)
# 
# # Save the probability raster
# writeRaster(probability_raster, filename = "D:/DEM/probability_RF_32_All.tif", format = "GTiff", overwrite = TRUE)
# 
# plot(probability_raster)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ## EXPort
# # # Create new folder in WD using manually or in R studio (lower right pan)
# # 
# # ## SRTM
# # "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology"
# #
# writeRaster(Landuse,filename="D:/DEM/resampled_all/Landuse.tif", format="GTiff", overwrite=TRUE)
# writeRaster(LULC_chang,filename="D:/DEM/resampled_all/LULC_chang.tif", format="GTiff", overwrite=TRUE)
# writeRaster(Imprevious,filename="D:/DEM/resampled_all/Imprevious.tif", format="GTiff", overwrite=TRUE)
# writeRaster(Rain,filename="D:/DEM/resampled_all/Rain.tif", format="GTiff", overwrite=TRUE)
# writeRaster(NDVI,filename="D:/DEM/resampled_all/NDVI.tif", format="GTiff", overwrite=TRUE)
# writeRaster(Geology,filename="D:/DEM/resampled_all/Geology.tif", format="GTiff", overwrite=TRUE)
# writeRaster(Slop_SRTM,filename="D:/DEM/resampled_all/Slop_SRTM.tif", format="GTiff", overwrite=TRUE)
# writeRaster(Elev_SRTM,filename="D:/DEM/resampled_all/Elev_SRTM.tif", format="GTiff", overwrite=TRUE)
# writeRaster(Asp_SRTM,filename="D:/DEM/resampled_all/Asp_SRTM.tif", format="GTiff", overwrite=TRUE)
# writeRaster(PL_SRTM,filename="D:/DEM/resampled_all/PL_SRTM.tif", format="GTiff", overwrite=TRUE)
# writeRaster(PR_SRTM,filename="D:/DEM/resampled_all/PR_SRTM.tif", format="GTiff", overwrite=TRUE)
# #writeRaster(TWI_SRTM,filename="D:/DEM/resampled_all/TWI_SRTM.tif", format="GTiff", overwrite=TRUE)
# #writeRaster(SPI_SRTM,filename="D:/DEM/resampled_all/SPI_SRTM.tif", format="GTiff", overwrite=TRUE)
# writeRaster(TRI_SRTM,filename="D:/DEM/resampled_all/TRI_SRTM.tif", format="GTiff", overwrite=TRUE)
# writeRaster(RO_SRTM,filename="D:/DEM/resampled_all/RO_SRTM.tif", format="GTiff", overwrite=TRUE)
# writeRaster(TPI_SRTM,filename="D:/DEM/resampled_all/TPI_SRTM.tif", format="GTiff", overwrite=TRUE)
# 
# 
# ##ASTER
# 
# writeRaster(Slop_ASTER,filename="D:/DEM/resampled_all/Slop_ASTER.tif", format="GTiff", overwrite=TRUE)
# writeRaster(Elev_ASTER,filename="D:/DEM/resampled_all/Elev_ASTER.tif", format="GTiff", overwrite=TRUE)
# writeRaster(Asp_ASTER,filename="D:/DEM/resampled_all/Asp_ASTER.tif", format="GTiff", overwrite=TRUE)
# writeRaster(PL_ASTER,filename="D:/DEM/resampled_all/PL_ASTER.tif", format="GTiff", overwrite=TRUE)
# writeRaster(PR_ASTER,filename="D:/DEM/resampled_all/PR_ASTER.tif", format="GTiff", overwrite=TRUE)
# #writeRaster(TWI_ASTER,filename="D:/DEM/resampled_all/TWI_ASTER.tif", format="GTiff", overwrite=TRUE)
# #writeRaster(SPI_ASTER,filename="D:/DEM/resampled_all/SPI_ASTER.tif", format="GTiff", overwrite=TRUE)
# writeRaster(TRI_ASTER,filename="D:/DEM/resampled_all/TRI_ASTER.tif", format="GTiff", overwrite=TRUE)
# writeRaster(RO_ASTER,filename="D:/DEM/resampled_all/RO_ASTER.tif", format="GTiff", overwrite=TRUE)
# writeRaster(TPI_ASTER,filename="D:/DEM/resampled_all/TPI_ASTER.tif", format="GTiff", overwrite=TRUE)
# 
# ## EDNA
# 
# writeRaster(Slop_EDNA,filename="D:/DEM/resampled_all/Slop_EDNA.tif", format="GTiff", overwrite=TRUE)
# writeRaster(Elev_EDNA,filename="D:/DEM/resampled_all/Elev_EDNA.tif", format="GTiff", overwrite=TRUE)
# writeRaster(Asp_EDNA,filename="D:/DEM/resampled_all/Asp_EDNA.tif", format="GTiff", overwrite=TRUE)
# writeRaster(PL_EDNA,filename="D:/DEM/resampled_all/PL_EDNA.tif", format="GTiff", overwrite=TRUE)
# writeRaster(PR_EDNA,filename="D:/DEM/resampled_all/PR_EDNA.tif", format="GTiff", overwrite=TRUE)
# #writeRaster(TWI_EDNA,filename="D:/DEM/resampled_all/TWI_EDNA.tif", format="GTiff", overwrite=TRUE)
# #writeRaster(SPI_EDNA,filename="D:/DEM/resampled_all/SPI_EDNA.tif", format="GTiff", overwrite=TRUE)
# writeRaster(TRI_EDNA,filename="D:/DEM/resampled_all/TRI_EDNA.tif", format="GTiff", overwrite=TRUE)
# writeRaster(RO_EDNA,filename="D:/DEM/resampled_all/RO_EDNA.tif", format="GTiff", overwrite=TRUE)
# writeRaster(TPI_EDNA,filename="D:/DEM/resampled_all/TPI_EDNA.tif", format="GTiff", overwrite=TRUE)

#
## stack multiple raster files
Stack_List= list.files(path = "D:/DEM/resampled_all",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)

#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)
# 
# Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y
# # Convert categorical variables in new data to factors with the same levels as training data
# categorical_vars <- c("Landuse", "Geology", "LULC_chang")
# 
# for (var in categorical_vars) {
#   Rasters.df_N[[var]] <- factor(Rasters.df_N[[var]], levels = levels(Data_m[[var]]))
# }
# 
# # Now make predictions using the trained Random Forest model
# p3 <- as.data.frame(predict(rf_model_36_All_Final, Rasters.df_N, type = "prob"))
# ##PRODUCE PROBABILITY MAP
# p3<-as.data.frame(predict(rf_model_36_All_Final, Rasters.df_N, type = "prob"))
# summary(p3)
# colnames(p3) <- c("no", "yes")
# Rasters.df$Levels_yes<-p3$yes
# Rasters.df$Levels_no<-p3$no
# 
# x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
# r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
# proj4string(r_ave_yes)=CRS(projection(Slop_SRTM))
# # Plot Maps
# spplot(r_ave_yes, main="Landslides SM using RF")
# 
# writeRaster(r_ave_yes,filename="D:/DEM/resampled_all/RF_32All_Factors", format="GTiff", overwrite=TRUE)
# 
# 
# 
# 
# 
# library(raster)
# library(sp)
# library(randomForest)
# 
# # Step 1: Stack multiple raster files
# Stack_List <- list.files(path = "D:/DEM/resampled_all", pattern = "tif$", full.names = TRUE)
# Rasters <- stack(Stack_List)
# 
# # Step 2: Convert raster stack to a data frame with longitude and latitude
# Rasters.df <- as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
# 
# # Step 3: Handle categorical variables in the new data to match the training data
# categorical_vars <- c("Landuse", "Geology", "LULC_chang")
# 
# for (var in categorical_vars) {
#   Rasters.df[[var]] <- factor(Rasters.df[[var]], levels = levels(Data_m[[var]]))
# }
# 
# # Step 4: Remove x and y columns for prediction
# Rasters.df_N <- Rasters.df[, !(names(Rasters.df) %in% c("x", "y"))]
# 
# # Step 5: Make predictions using the trained Random Forest model
# p3 <- as.data.frame(predict(rf_model_36_All_Final, Rasters.df_N, type = "prob"))
# 
# # Step 6: Add prediction results back to the original data frame
# colnames(p3) <- c("no", "yes")
# Rasters.df$Levels_yes <- p3$yes
# Rasters.df$Levels_no <- p3$no
# 
# # Step 7: Convert the data frame back to spatial format
# coordinates(Rasters.df) <- ~x+y
# proj4string(Rasters.df) <- proj4string(Slop_SRTM)
# 
# # Step 8: Create raster from the prediction results
# r_ave_yes <- rasterFromXYZ(Rasters.df[, c("x", "y", "Levels_yes")])
# proj4string(r_ave_yes) <- CRS(projection(Slop_SRTM))
# 
# # Step 9: Plot the results
# plot(r_ave_yes, main = "Landslide Susceptibility Map using RF")
# 
# # Step 10: Save the predicted raster
# writeRaster(r_ave_yes, filename = "D:/DEM/resampled_all/RF_32All_Factors.tif", format = "GTiff", overwrite = TRUE)
# 


## SRTM



## Check with 30 DEM based Factors and Others

library(sf)
library(tidyverse)
library(randomForest)

library(caret)
library(pROC)
library(e1071)
library(iml)

# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
glimpse(Non_landslides)
library(tidyverse)
library(dplyr)
landslides <- landslides %>% select(V,
  Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, 
  TRI_SRTM, TPI_SRTM,
  Imprevious , LULC_chang, Geology, Rain, NDVI, Landuse)
Non_landslides <- Non_landslides %>% select(V,
  Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, 
  TRI_SRTM, TPI_SRTM,
  Imprevious , LULC_chang, Geology, Rain,  NDVI, Landuse)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))

# Scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
glimpse(Data_m)
Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
Data_m$V <- as.factor(Data_m$V)

#Scale the Variables
# Load necessary library
library(dplyr)

# Function to scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

Data_m <- Data_m %>%
  mutate(across(.cols = -c(V      ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
#Data_m$Soil       <- as.factor(Data_m$Soil        )
Data_m$Geology <- as.factor(Data_m$Geology )
Data_m$Landuse <- as.factor(Data_m$Landuse )
Data_m$LULC_chang <- as.factor(Data_m$LULC_chang )
Data_m$V <- as.factor(Data_m$V)
# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m[trainIndex, ]
testData <- Data_m[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)
# Train a Random Forest model
rf_model_SRTM_All <- randomForest(V ~ ., data = trainData, ntree = 1000, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_SRTM_All )

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_SRTM_All , newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_SRTM_All , newdata = testData, type = "response")

# Evaluate the model
confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
print(confusion_matrix_rf)

# Correct ROC curve data for ggplot2 (flip specificity and sensitivity)
roc_data_rf <- data.frame(
  fpr = rev(1 - roc_curve_rf$specificities),  # False positive rate = 1 - specificity
  tpr = rev(roc_curve_rf$sensitivities)       # True positive rate = sensitivity
)

# Plot ROC curve with corrected axes
roc_rf_SRTM <- ggplot(roc_data_rf, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Random Forest", x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

# Display the plot
print(roc_rf_SRTM)
# Get variable importance
importance_rf <- importance(rf_model_SRTM_All  )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1

## Map Generation

# Train a Random Forest model
rf_model_SRTM_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)

library(raster)
library(sp)
library(caret)  # Assuming you used caret for model training

## SRTM

Slop_SRTM <- raster("D:/DEM/Slope_SRTM.tif")
Elev_SRTM <- raster("D:/DEM/SRTM.tif")
Asp_SRTM  <- raster("D:/DEM/Aspect_SRTM.tif")
PL_SRTM   <- raster("D:/DEM/Plan_SRTM.tif")
PR_SRTM   <- raster("D:/DEM/Profile_SRTM.tif")
TRI_SRTM  <- raster("D:/DEM/TRI_SRTM.tif")
RO_SRTM   <- raster("D:/DEM/Roughness_SRTM.tif")
TPI_SRTM  <- raster("D:/DEM/TPI_SRTM.tif")
PL_SRTM <- projectRaster(PL_SRTM , crs = crs(Slop_SRTM))
PR_SRTM <- projectRaster(PR_SRTM , crs = crs(Slop_SRTM))


list.files("D:/DEM/Other Factors/")
Landuse <- raster("D:/DEM/Other Factors/Landuse.tif")

Geology= raster("D:/Cheeky Scientist/Factors/Geologic Formation.tif")
Imprevious= raster("D:/Cheeky Scientist/Factors/Imprevious.tif")
LULC_chang= raster("D:/Cheeky Scientist/Factors/LULC_change.tif")
NDVI= raster("D:/Cheeky Scientist/Factors/NDVI.tif")
Rain= raster("D:/Cheeky Scientist/Factors/Rain.tif")
plot(Rain)
crs_Slop_SRTM <- crs(Slop_SRTM)

Landuse <- projectRaster(Landuse, crs = crs_Slop_SRTM)
Geology <- projectRaster(Geology, crs = crs_Slop_SRTM)
Imprevious <- projectRaster(Imprevious, crs = crs_Slop_SRTM)
LULC_chang <- projectRaster(LULC_chang, crs = crs_Slop_SRTM)
NDVI <- projectRaster(NDVI, crs = crs_Slop_SRTM)
Rain <- projectRaster(Rain, crs = crs_Slop_SRTM)
## Resampling

Landuse=resample(Landuse,Slop_SRTM, resample='bilinear')
Geology=resample(Geology,Slop_SRTM, resample='bilinear')
Imprevious=resample(Imprevious,Slop_SRTM, resample='bilinear')
LULC_chang=resample(LULC_chang,Slop_SRTM, resample='bilinear')
Rain=resample(Rain,Slop_SRTM, resample='bilinear')
NDVI=resample(NDVI,Slop_SRTM, resample='bilinear')
## SRTM

Slop_SRTM=resample(Slop_SRTM,Slop_SRTM, resample='bilinear')
Elev_SRTM=resample(Elev_SRTM,Slop_SRTM, resample='bilinear')
Asp_SRTM=resample(Asp_SRTM,Slop_SRTM, resample='bilinear')
PL_SRTM=resample(PL_SRTM,Slop_SRTM, resample='bilinear')
PR_SRTM=resample(PR_SRTM,Slop_SRTM, resample='bilinear')
TRI_SRTM=resample(TRI_SRTM,Slop_SRTM, resample='bilinear')
RO_SRTM=resample(RO_SRTM,Slop_SRTM, resample='bilinear')
TPI_SRTM=resample(TPI_SRTM,Slop_SRTM, resample='bilinear')


## Extent 

Landuse<- crop(Landuse, extent(Slop_SRTM))
Geology <- crop(Geology, extent(Slop_SRTM))
Imprevious <- crop(Imprevious, extent(Slop_SRTM))
LULC_chang <- crop(LULC_chang, extent(Slop_SRTM))
Rain <- crop(Rain, extent(Slop_SRTM))
NDVI <- crop(NDVI, extent(Slop_SRTM))

Slop_SRTM <- crop(Slop_SRTM, extent(Slop_SRTM))
Elev_SRTM <- crop(Elev_SRTM, extent(Slop_SRTM))
Asp_SRTM  <- crop(Asp_SRTM, extent(Slop_SRTM))
PL_SRTM   <- crop(PL_SRTM, extent(Slop_SRTM))
PR_SRTM   <- crop(PR_SRTM, extent(Slop_SRTM))
TRI_SRTM  <- crop(TRI_SRTM, extent(Slop_SRTM))
RO_SRTM   <- crop(RO_SRTM, extent(Slop_SRTM))
TPI_SRTM  <- crop(TPI_SRTM, extent(Slop_SRTM))



# Stack all predictor rasters
predictor_stack <- stack(Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, TRI_SRTM, RO_SRTM, TPI_SRTM,
                         Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)
names(predictor_stack) <- c("Slop_SRTM", "Elev_SRTM", "Asp_SRTM", "PL_SRTM", "PR_SRTM", "TRI_SRTM", "RO_SRTM", "TPI_SRTM",
                             "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology")


# Train a Random Forest model (final model)
rf_model_SRTM_All_Final<- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)

# Convert the raster stack to a data frame including the x and y coordinates
predictor_df <- as.data.frame(predictor_stack, xy = TRUE, na.rm = TRUE)

# Display the first few rows of the data frame
head(predictor_df)




# Make predictions using the trained Random Forest model
predictions <- predict(rf_model_SRTM_All_Final, predictor_df, type = "response")
probability_predictions <- predict(rf_model_SRTM_All_Final, predictor_df, type = "prob")[,2]



# Make predictions using the trained Random Forest model
predicted_raster <- predict(predictor_stack, rf_model_SRTM_All_Final, type = "response")

# For probability of landslide
probability_raster <- predict(predictor_stack, rf_model_SRTM_All_Final, type = "prob", index = 2)

# Save the predicted raster
writeRaster(predicted_raster, filename = "D:/DEM/predicted_RF_SRTM_All.tif", format = "GTiff", overwrite = TRUE)

# Save the probability raster
writeRaster(probability_raster, filename = "D:/DEM/probability_RF_SRTM_All.tif", format = "GTiff", overwrite = TRUE)

plot(probability_raster)


## EDNA

library(sf)
library(tidyverse)
library(randomForest)

library(caret)
library(pROC)
library(e1071)
library(iml)

# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
glimpse(Non_landslides)
library(tidyverse)
library(dplyr)
landslides <- landslides %>% select( V,
                                     
                                     Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA,  
                                     TRI_EDNA, TPI_EDNA,
                                     
                                     Imprevious , LULC_chang, Geology, Rain, NDVI, Landuse)
Non_landslides <- Non_landslides %>% select( V,
                                             
                                             Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA,  
                                             TRI_EDNA, TPI_EDNA,
                                             
                                             Imprevious , LULC_chang, Geology, Rain,  NDVI, Landuse)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))

# Scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
glimpse(Data_m)
Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
Data_m$V <- as.factor(Data_m$V)

#Scale the Variables
# Load necessary library
library(dplyr)

# Function to scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

Data_m <- Data_m %>%
  mutate(across(.cols = -c(V      ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
#Data_m$Soil       <- as.factor(Data_m$Soil        )
Data_m$Geology <- as.factor(Data_m$Geology )
Data_m$Landuse <- as.factor(Data_m$Landuse )
Data_m$LULC_chang <- as.factor(Data_m$LULC_chang )
Data_m$V <- as.factor(Data_m$V)
# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m[trainIndex, ]
testData <- Data_m[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)
# Train a Random Forest model
rf_model_EDNA_All <- randomForest(V ~ ., data = trainData, ntree = 1000, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_EDNA_All )

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_EDNA_All , newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_EDNA_All , newdata = testData, type = "response")

# Evaluate the model
confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
print(confusion_matrix_rf)

# Calculate ROC curve and AUC
roc_curve_rf <- roc(testData$V, rf_pred_prob)
auc_value_rf <- auc(roc_curve_rf)
print(paste("AUC for Random Forest:", auc_value_rf))
auc_1m <- as.data.frame(auc_value_rf )
# Plot ROC curve with corrected axes
roc_rf_EDNA <- ggplot(roc_data_rf, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Random Forest", x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  theme_minimal()

# Display the plot
print(roc_rf_EDNA)

# Get variable importance
importance_rf <- importance(rf_model_EDNA_All )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1

## Map Generation

# Train a Random Forest model
rf_model_EDNA_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)

library(raster)
library(sp)
library(caret)  # Assuming you used caret for model training



## EDNA

Slop_EDNA <- raster("D:/DEM/Slope_EDNA.tif")
Elev_EDNA <- raster("D:/DEM/EDNA.tif")
Asp_EDNA  <- raster("D:/DEM/Aspect_EDNA.tif")
PL_EDNA   <- raster("D:/DEM/Plan_EDNA.tif")
PR_EDNA  <- raster("D:/DEM/Profile_EDNA.tif")
TRI_EDNA  <- raster("D:/DEM/TRI_EDNA.tif")
RO_EDNA   <- raster("D:/DEM/Roughness_EDNA.tif")
TPI_EDNA  <- raster("D:/DEM/TPI_EDNA.tif")
PL_EDNA <- projectRaster(PL_EDNA , crs = crs(Slop_SRTM))
PR_EDNA <- projectRaster(PR_EDNA, crs = crs(Slop_SRTM))
list.files("D:/DEM/Other Factors/")
Landuse <- raster("D:/DEM/Other Factors/Landuse.tif")

Geology= raster("D:/Cheeky Scientist/Factors/Geologic Formation.tif")
Imprevious= raster("D:/Cheeky Scientist/Factors/Imprevious.tif")
LULC_chang= raster("D:/Cheeky Scientist/Factors/LULC_change.tif")
NDVI= raster("D:/Cheeky Scientist/Factors/NDVI.tif")
Rain= raster("D:/Cheeky Scientist/Factors/Rain.tif")
plot(Rain)
crs_Slop_SRTM <- crs(Slop_SRTM)

Landuse <- projectRaster(Landuse, crs = crs_Slop_SRTM)
Geology <- projectRaster(Geology, crs = crs_Slop_SRTM)
Imprevious <- projectRaster(Imprevious, crs = crs_Slop_SRTM)
LULC_chang <- projectRaster(LULC_chang, crs = crs_Slop_SRTM)
NDVI <- projectRaster(NDVI, crs = crs_Slop_SRTM)
Rain <- projectRaster(Rain, crs = crs_Slop_SRTM)
## Resampling

Landuse=resample(Landuse,Slop_SRTM, resample='bilinear')
Geology=resample(Geology,Slop_SRTM, resample='bilinear')
Imprevious=resample(Imprevious,Slop_SRTM, resample='bilinear')
LULC_chang=resample(LULC_chang,Slop_SRTM, resample='bilinear')
Rain=resample(Rain,Slop_SRTM, resample='bilinear')
NDVI=resample(NDVI,Slop_SRTM, resample='bilinear')


## EDNA

Slop_EDNA=resample(Slop_EDNA,Slop_SRTM, resample='bilinear')
Elev_EDNA=resample(Elev_EDNA,Slop_SRTM, resample='bilinear')
Asp_EDNA=resample(Asp_EDNA,Slop_SRTM, resample='bilinear')
PL_EDNA=resample(PL_EDNA,Slop_SRTM, resample='bilinear')
PR_EDNA=resample(PR_EDNA,Slop_SRTM, resample='bilinear')
TRI_EDNA=resample(TRI_EDNA,Slop_SRTM, resample='bilinear')
RO_EDNA=resample(RO_EDNA,Slop_SRTM, resample='bilinear')
TPI_EDNA=resample(TPI_EDNA,Slop_SRTM, resample='bilinear')

## Extent 

Landuse<- crop(Landuse, extent(Slop_SRTM))
Geology <- crop(Geology, extent(Slop_SRTM))
Imprevious <- crop(Imprevious, extent(Slop_SRTM))
LULC_chang <- crop(LULC_chang, extent(Slop_SRTM))
Rain <- crop(Rain, extent(Slop_SRTM))
NDVI <- crop(NDVI, extent(Slop_SRTM))



## EDNA
Slop_EDNA <- crop(Slop_EDNA, extent(Slop_SRTM))
Elev_EDNA <- crop(Elev_EDNA, extent(Slop_SRTM))
Asp_EDNA  <- crop(Asp_EDNA, extent(Slop_SRTM))
PL_EDNA  <- crop(PL_EDNA, extent(Slop_SRTM))
PR_EDNA  <- crop(PR_EDNA, extent(Slop_SRTM))
TRI_EDNA  <- crop(TRI_EDNA, extent(Slop_SRTM))
RO_EDNA  <- crop(RO_EDNA, extent(Slop_SRTM))
TPI_EDNA<- crop(TPI_EDNA, extent(Slop_SRTM))



# Stack all predictor rasters
predictor_stack <- stack(
  Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, TRI_EDNA, RO_EDNA, TPI_EDNA, Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)
names(predictor_stack) <- c(
  "Slop_EDNA", "Elev_EDNA", "Asp_EDNA", "PL_EDNA", "PR_EDNA", "TRI_EDNA", "RO_EDNA", "TPI_EDNA", "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology")


# Train a Random Forest model (final model)
rf_model_EDNA_All_Final<- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)

# Train a Random Forest model (final model)
rf_model_EDNA_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 20, importance = TRUE)

# Make predictions using the trained Random Forest model
predicted_raster <- predict(predictor_stack, rf_model_EDNA_All_Final, type = "response")

# For probability of landslide
probability_raster <- predict(predictor_stack, rf_model_EDNA_All_Final, type = "prob", index = 2)

# Save the predicted raster
writeRaster(predicted_raster, filename = "D:/DEM/predicted_RF_EDNA_All.tif", format = "GTiff", overwrite = TRUE)

# Save the probability raster
writeRaster(probability_raster, filename = "D:/DEM/probability_RF_EDNA_All.tif", format = "GTiff", overwrite = TRUE)

# Plot the probability raster
plot(probability_raster, main = "Landslide Probability Map")


## ASTER

## Check with 30 DEM based Factors and Others

## Check with 30 DEM based Factors and Others

library(sf)
library(tidyverse)
library(randomForest)

library(caret)
library(pROC)
library(e1071)
library(iml)

# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
glimpse(Non_landslides)
library(tidyverse)
library(dplyr)
landslides <- landslides %>% select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V,
                                    TRI_ASTER, RO_ASTER, TPI_ASTER,
                                    
                                    Imprevious , LULC_chang, Geology, Rain, NDVI, Landuse)
Non_landslides <- Non_landslides %>% select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V,
                                            TRI_ASTER, RO_ASTER, TPI_ASTER,
                                            
                                            Imprevious , LULC_chang, Geology, Rain,  NDVI, Landuse)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))


Data_m$V <- as.factor(Data_m$V)


# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m[trainIndex, ]
testData <- Data_m[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)
# Train a Random Forest model
rf_model_ASTER_All <- randomForest(V ~ ., data = trainData, ntree = 1000, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_ASTER_All )

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_ASTER_All , newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_ASTER_All , newdata = testData, type = "response")

# Evaluate the model
confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
print(confusion_matrix_rf)

# Calculate ROC curve and AUC
roc_curve_rf <- roc(testData$V, rf_pred_prob)
auc_value_rf <- auc(roc_curve_rf)
print(paste("AUC for Random Forest:", auc_value_rf))
auc_1m <- as.data.frame(auc_value_rf )
# Plot ROC curve using ggplot2
roc_data_rf <- data.frame(
  specificity = rev(roc_curve_rf$specificities),
  sensitivity = rev(roc_curve_rf$sensitivities)
)

ggplot(roc_data_rf, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Random Forest", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

# Get variable importance
importance_rf <- importance(rf_model_ASTER_All )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1

## Map Generation

# Train a Random Forest model
rf_model_ASTER_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)

library(raster)
library(sp)
library(caret)  # Assuming you used caret for model training



Slop_ASTER <- raster("D:/DEM/Slope_ASTER.tif")
Elev_ASTER <- raster("D:/DEM/ASTER_30m.tif")
Asp_ASTER  <- raster("D:/DEM/Aspect_ASTER.tif")
PL_ASTER   <- raster("D:/DEM/Plan_ASTER.tif")
PR_ASTER   <- raster("D:/DEM/Profile_ASTER.tif")
TRI_ASTER  <- raster("D:/DEM/TRI_ASTER.tif")
RO_ASTER   <- raster("D:/DEM/Roughness_ASTER.tif")
TPI_ASTER  <- raster("D:/DEM/TPI_ASTER.tif")
PL_ASTER <- projectRaster(PL_ASTER , crs = crs(Slop_SRTM))
PR_ASTER <- projectRaster(PR_ASTER, crs = crs(Slop_SRTM))


list.files("D:/DEM/Other Factors/")
Landuse <- raster("D:/DEM/Other Factors/Landuse.tif")

Geology= raster("D:/Cheeky Scientist/Factors/Geologic Formation.tif")
Imprevious= raster("D:/Cheeky Scientist/Factors/Imprevious.tif")
LULC_chang= raster("D:/Cheeky Scientist/Factors/LULC_change.tif")
NDVI= raster("D:/Cheeky Scientist/Factors/NDVI.tif")
Rain= raster("D:/Cheeky Scientist/Factors/Rain.tif")
plot(Rain)
crs_Slop_SRTM <- crs(Slop_SRTM)

Landuse <- projectRaster(Landuse, crs = crs_Slop_SRTM)
Geology <- projectRaster(Geology, crs = crs_Slop_SRTM)
Imprevious <- projectRaster(Imprevious, crs = crs_Slop_SRTM)
LULC_chang <- projectRaster(LULC_chang, crs = crs_Slop_SRTM)
NDVI <- projectRaster(NDVI, crs = crs_Slop_SRTM)
Rain <- projectRaster(Rain, crs = crs_Slop_SRTM)
## Resampling

Landuse=resample(Landuse,Slop_SRTM, resample='bilinear')
Geology=resample(Geology,Slop_SRTM, resample='bilinear')
Imprevious=resample(Imprevious,Slop_SRTM, resample='bilinear')
LULC_chang=resample(LULC_chang,Slop_SRTM, resample='bilinear')
Rain=resample(Rain,Slop_SRTM, resample='bilinear')
NDVI=resample(NDVI,Slop_SRTM, resample='bilinear')

## ASTER
Slop_ASTER=resample(Slop_ASTER,Slop_SRTM, resample='bilinear')
Elev_ASTER=resample(Elev_ASTER,Slop_SRTM, resample='bilinear')
Asp_ASTER=resample(Asp_ASTER,Slop_SRTM, resample='bilinear')
PL_ASTER=resample(PR_ASTER,Slop_SRTM, resample='bilinear')
PR_ASTER=resample(PR_ASTER,Slop_SRTM, resample='bilinear')
TRI_ASTER=resample(TRI_ASTER,Slop_SRTM, resample='bilinear')
RO_ASTER=resample(RO_ASTER,Slop_SRTM, resample='bilinear')
TPI_ASTER=resample(TPI_ASTER,Slop_SRTM, resample='bilinear')



## Extent 

Landuse<- crop(Landuse, extent(Slop_SRTM))
Geology <- crop(Geology, extent(Slop_SRTM))
Imprevious <- crop(Imprevious, extent(Slop_SRTM))
LULC_chang <- crop(LULC_chang, extent(Slop_SRTM))
Rain <- crop(Rain, extent(Slop_SRTM))
NDVI <- crop(NDVI, extent(Slop_SRTM))


## ASTER
Slop_ASTER <- crop(Slop_ASTER, extent(Slop_SRTM))
Elev_ASTER <- crop(Elev_ASTER, extent(Slop_SRTM))
Asp_ASTER  <- crop(Asp_ASTER, extent(Slop_SRTM))
PL_ASTER  <- crop(PL_ASTER, extent(Slop_SRTM))
PR_ASTER  <- crop(PR_ASTER, extent(Slop_SRTM))
TRI_ASTER  <- crop(TRI_ASTER, extent(Slop_SRTM))
RO_ASTER  <- crop(RO_ASTER, extent(Slop_SRTM))
TPI_ASTER <- crop(TPI_ASTER, extent(Slop_SRTM))






# Stack all predictor rasters
predictor_stack <- stack(
  Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, TRI_ASTER, RO_ASTER, TPI_ASTER,
  Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)
names(predictor_stack) <- c(
  "Slop_ASTER", "Elev_ASTER", "Asp_ASTER", "PL_ASTER", "PR_ASTER", "TRI_ASTER", "RO_ASTER", "TPI_ASTER",
  "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology")


# Train a Random Forest model (final model)
rf_model_ASTER_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)

# Make predictions using the trained Random Forest model
predicted_raster <- predict(predictor_stack, rf_model_ASTER_All_Final, type = "response")

# For probability of landslide
probability_raster <- predict(predictor_stack, rf_model_ASTER_All_Final, type = "prob", index = 2)

# Save the predicted raster
writeRaster(predicted_raster, filename = "D:/DEM/predicted_RF_ASTER_All.tif", format = "GTiff", overwrite = TRUE)

# Save the probability raster
writeRaster(probability_raster, filename = "D:/DEM/probability_RF_ASTER_All.tif", format = "GTiff", overwrite = TRUE)

# Plot the probability raster
plot(probability_raster, main = "Landslide Probability Map")



## DEM
## DEM

## DEM
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
landslides <- landslides %>% select(Slop_DEM, Elev_DEM, Asp_DEM, PL_DEM, PR_DEM, V, 
                                    TRI_DEM, RO_DEM, TPI_DEM, Imprevious , LULC_chang, Geology, Rain, NDVI, Landuse)
Non_landslides <- Non_landslides %>% select(Slop_DEM, Elev_DEM, Asp_DEM, PL_DEM, PR_DEM, V,  
                                            TRI_DEM, RO_DEM, TPI_DEM, Imprevious , LULC_chang, Geology, Rain, NDVI, Landuse)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))


Data_m$V <- as.factor(Data_m$V)


# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m[trainIndex, ]
testData <- Data_m[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)

# Train a Random Forest model
rf_model_DEM_All <- randomForest(V ~ ., data = trainData, ntree = 1000, mtry = 2, importance = TRUE)

# Summarize the model
print(rf_model_DEM_All )

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_DEM_All  , newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_DEM_All  , newdata = testData, type = "response")

# Evaluate the model
confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
print(confusion_matrix_rf)

# Calculate ROC curve and AUC
roc_curve_rf <- roc(testData$V, rf_pred_prob)
auc_value_rf <- auc(roc_curve_rf)
print(paste("AUC for Random Forest:", auc_value_rf))
auc_1m <- as.data.frame(auc_value_rf )
# Plot ROC curve using ggplot2
roc_data_rf <- data.frame(
  specificity = rev(roc_curve_rf$specificities),
  sensitivity = rev(roc_curve_rf$sensitivities)
)

ggplot(roc_data_rf, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Random Forest", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

# Get variable importance
importance_rf <- importance(rf_model_DEM_All )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## Mapping

rf_model_DEM_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 2, importance = TRUE)


library(raster)
library(sp)
library(caret)  # Assuming you used caret for model training


## ASTER

Slop_DEM <- raster("D:/DEM/Slope_DEM.tif")
Elev_DEM <- raster("D:/DEM/DEM_1m to 30m.tif")
Asp_DEM  <- raster("D:/DEM/Aspect_DEM.tif")
PL_DEM   <- raster("D:/DEM/Plan_DEM.tif")
PR_DEM   <- raster("D:/DEM/Profile_DEM.tif")
TRI_DEM  <- raster("D:/DEM/TRI_DEM.tif")
RO_DEM   <- raster("D:/DEM/Roughness_DEM.tif")
TPI_DEM <- raster("D:/DEM/TPI_DEM.tif")
PL_DEM <- projectRaster(PL_DEM , crs = crs(Slop_SRTM))
PR_DEM <- projectRaster(PR_DEM, crs = crs(Slop_SRTM))

list.files("D:/DEM/Other Factors/")
Landuse <- raster("D:/DEM/Other Factors/Landuse.tif")

Geology= raster("D:/Cheeky Scientist/Factors/Geologic Formation.tif")
Imprevious= raster("D:/Cheeky Scientist/Factors/Imprevious.tif")
LULC_chang= raster("D:/Cheeky Scientist/Factors/LULC_change.tif")
NDVI= raster("D:/Cheeky Scientist/Factors/NDVI.tif")
Rain= raster("D:/Cheeky Scientist/Factors/Rain.tif")
plot(Rain)
crs_Slop_SRTM <- crs(Slop_SRTM)

Landuse <- projectRaster(Landuse, crs = crs_Slop_SRTM)
Geology <- projectRaster(Geology, crs = crs_Slop_SRTM)
Imprevious <- projectRaster(Imprevious, crs = crs_Slop_SRTM)
LULC_chang <- projectRaster(LULC_chang, crs = crs_Slop_SRTM)
NDVI <- projectRaster(NDVI, crs = crs_Slop_SRTM)
Rain <- projectRaster(Rain, crs = crs_Slop_SRTM)
## ASTER
Slop_DEM=resample(Slop_DEM,Slop_SRTM, resample='bilinear')
Elev_DEM=resample(Elev_DEM,Slop_SRTM, resample='bilinear')
Asp_DEM=resample(Asp_DEM,Slop_SRTM, resample='bilinear')
PL_DEM=resample(PR_DEM,Slop_SRTM, resample='bilinear')
PR_DEM=resample(PR_DEM,Slop_SRTM, resample='bilinear')
TRI_DEM=resample(TRI_DEM,Slop_SRTM, resample='bilinear')
RO_DEM=resample(RO_DEM,Slop_SRTM, resample='bilinear')
TPI_DEM=resample(TPI_DEM,Slop_SRTM, resample='bilinear')

Landuse=resample(Landuse,Slop_SRTM, resample='bilinear')
Geology=resample(Geology,Slop_SRTM, resample='bilinear')
Imprevious=resample(Imprevious,Slop_SRTM, resample='bilinear')
LULC_chang=resample(LULC_chang,Slop_SRTM, resample='bilinear')
Rain=resample(Rain,Slop_SRTM, resample='bilinear')
NDVI=resample(NDVI,Slop_SRTM, resample='bilinear')

## Extent 

Landuse<- crop(Landuse, extent(Slop_SRTM))
Geology <- crop(Geology, extent(Slop_SRTM))
Imprevious <- crop(Imprevious, extent(Slop_SRTM))
LULC_chang <- crop(LULC_chang, extent(Slop_SRTM))
Rain <- crop(Rain, extent(Slop_SRTM))
NDVI <- crop(NDVI, extent(Slop_SRTM))
## ASTER
Slop_DEM <- crop(Slop_DEM, extent(Slop_SRTM))
Elev_DEM <- crop(Elev_DEM, extent(Slop_SRTM))
Asp_DEM  <- crop(Asp_DEM, extent(Slop_SRTM))
PL_DEM  <- crop(PL_DEM, extent(Slop_SRTM))
PR_DEM  <- crop(PR_DEM, extent(Slop_SRTM))
TRI_DEM  <- crop(TRI_DEM, extent(Slop_SRTM))
RO_DEM  <- crop(RO_DEM, extent(Slop_SRTM))
TPI_DEM <- crop(TPI_DEM, extent(Slop_SRTM))


# Stack all predictor rasters
predictor_stack <- stack(Slop_DEM, Elev_DEM, Asp_DEM, PL_DEM, PR_DEM, TRI_DEM, RO_DEM, TPI_DEM, 
                         Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)
names(predictor_stack) <- c("Slop_DEM", "Elev_DEM", "Asp_DEM", "PL_DEM", "PR_DEM", "TRI_DEM", "RO_DEM", "TPI_DEM",
                            "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology")

# Train a Random Forest model (final model)
rf_model_DEM_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 2, importance = TRUE)

# Make predictions using the trained Random Forest model
predicted_raster <- predict(predictor_stack, rf_model_DEM_All_Final, type = "response")

# For probability of landslide
probability_raster <- predict(predictor_stack, rf_model_DEM_All_Final, type = "prob", index = 2)

# Save the predicted raster
writeRaster(predicted_raster, filename = "D:/DEM/predicted_RF_DEM_All.tif", format = "GTiff", overwrite = TRUE)

# Save the probability raster
writeRaster(probability_raster, filename = "D:/DEM/probability_RF_DEM_All.tif", format = "GTiff", overwrite = TRUE)

## Combined (Accuracy Based)

## Combine based on Accuracy

## New Model (Based on Accuracy)

library(sf)
library(tidyverse)
library(randomForest)

library(caret)
library(pROC)
library(e1071)


# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)

library(tidyverse)
library(dplyr)
landslides <- landslides %>% select (Slop_New, Elev_New, Asp_New, PL_New, PR_New, V, 
                                     TRI_New, RO_New, TPI_New, Imprevious , LULC_chang, Geology, Rain, NDVI, Landuse)

Non_landslides <- Non_landslides %>% select (Slop_New, Elev_New, Asp_New, PL_New, PR_New, V,  
                                             TRI_New, RO_New, TPI_New, Imprevious , LULC_chang, Geology, Rain, NDVI, Landuse)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))


Data_m$V <- as.factor(Data_m$V)


# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m[trainIndex, ]
testData <- Data_m[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)


# Train a Random Forest model
rf_model__All_New <- randomForest(V ~ ., data = trainData, ntree = 1000, mtry = 2, importance = TRUE)

# Summarize the model
print(rf_model__All_New)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model__All_New, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model__All_New, newdata = testData, type = "response")

# Evaluate the model
confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
print(confusion_matrix_rf)

# Calculate ROC curve and AUC
roc_curve_rf <- roc(testData$V, rf_pred_prob)
auc_value_rf <- auc(roc_curve_rf)
print(paste("AUC for Random Forest:", auc_value_rf))
auc_1m <- as.data.frame(auc_value_rf )
# Plot ROC curve using ggplot2
roc_data_rf <- data.frame(
  specificity = rev(roc_curve_rf$specificities),
  sensitivity = rev(roc_curve_rf$sensitivities)
)

ggplot(roc_data_rf, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Random Forest", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

# Get variable importance
importance_rf <- importance(rf_model__All_New )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1

## Mapping

rf_model__All_New_Final <- randomForest(V ~ ., data = Data_m , ntree = 1000, mtry = 2, importance = TRUE)

library(raster)
library(sp)
library(caret)  # Assuming you used caret for model training


## New

Slop_New <- raster("D:/DEM/Slope_New.tif")
Elev_New <- raster("D:/DEM/New_DEM.tif")
Asp_New  <- raster("D:/DEM/Aspect_New.tif")
PL_New   <- raster("D:/DEM/plan_New.tif")
PR_New   <- raster("D:/DEM/profile_New.tif")
TRI_New  <- raster("D:/DEM/TRI_New.tif")
RO_New   <- raster("D:/DEM/Roughness_New.tif")
TPI_New <- raster("D:/DEM/TPI_New.tif")
PL_New <- projectRaster(PL_New , crs = crs(Slop_SRTM))
PR_New <- projectRaster(PR_New, crs = crs(Slop_SRTM))

list.files("D:/DEM/Other Factors/")
Landuse <- raster("D:/DEM/Other Factors/Landuse.tif")

Geology= raster("D:/Cheeky Scientist/Factors/Geologic Formation.tif")
Imprevious= raster("D:/Cheeky Scientist/Factors/Imprevious.tif")
LULC_chang= raster("D:/Cheeky Scientist/Factors/LULC_change.tif")
NDVI= raster("D:/Cheeky Scientist/Factors/NDVI.tif")
Rain= raster("D:/Cheeky Scientist/Factors/Rain.tif")
plot(Rain)
crs_Slop_SRTM <- crs(Slop_SRTM)

Landuse <- projectRaster(Landuse, crs = crs_Slop_SRTM)
Geology <- projectRaster(Geology, crs = crs_Slop_SRTM)
Imprevious <- projectRaster(Imprevious, crs = crs_Slop_SRTM)
LULC_chang <- projectRaster(LULC_chang, crs = crs_Slop_SRTM)
NDVI <- projectRaster(NDVI, crs = crs_Slop_SRTM)
Rain <- projectRaster(Rain, crs = crs_Slop_SRTM)
## ASTER
Slop_New=resample(Slop_New,Slop_SRTM, resample='bilinear')
Elev_New=resample(Elev_New,Slop_SRTM, resample='bilinear')
Asp_New=resample(Asp_New,Slop_SRTM, resample='bilinear')
PL_New=resample(PR_New,Slop_SRTM, resample='bilinear')
PR_New=resample(PR_New,Slop_SRTM, resample='bilinear')
TRI_New=resample(TRI_New,Slop_SRTM, resample='bilinear')
RO_New=resample(RO_New,Slop_SRTM, resample='bilinear')
TPI_New=resample(TPI_New,Slop_SRTM, resample='bilinear')


Landuse=resample(Landuse,Slop_SRTM, resample='bilinear')
Geology=resample(Geology,Slop_SRTM, resample='bilinear')
Imprevious=resample(Imprevious,Slop_SRTM, resample='bilinear')
LULC_chang=resample(LULC_chang,Slop_SRTM, resample='bilinear')
Rain=resample(Rain,Slop_SRTM, resample='bilinear')
NDVI=resample(NDVI,Slop_SRTM, resample='bilinear')
## Extent 


## ASTER
Slop_New <- crop(Slop_New, extent(Slop_SRTM))
Elev_New <- crop(Elev_New, extent(Slop_SRTM))
Asp_New  <- crop(Asp_New, extent(Slop_SRTM))
PL_New  <- crop(PL_New, extent(Slop_SRTM))
PR_New  <- crop(PR_New, extent(Slop_SRTM))
TRI_New  <- crop(TRI_New, extent(Slop_SRTM))
RO_New  <- crop(RO_New, extent(Slop_SRTM))
TPI_New <- crop(TPI_New, extent(Slop_SRTM))

Landuse<- crop(Landuse, extent(Slop_SRTM))
Geology <- crop(Geology, extent(Slop_SRTM))
Imprevious <- crop(Imprevious, extent(Slop_SRTM))
LULC_chang <- crop(LULC_chang, extent(Slop_SRTM))
Rain <- crop(Rain, extent(Slop_SRTM))
NDVI <- crop(NDVI, extent(Slop_SRTM))
# Stack all predictor rasters
predictor_stack <- stack(Slop_New, Elev_New, Asp_New, PL_New, PR_New, TRI_New, RO_New, TPI_New, 
                         Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)
names(predictor_stack) <- c("Slop_New", "Elev_New", "Asp_New", "PL_New", "PR_New", "TRI_New", "RO_New", "TPI_New",
                            "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology" )

# Train a Random Forest model (final model)
rf_model__All_New_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 2, importance = TRUE)

# Make predictions using the trained Random Forest model
predicted_raster <- predict(predictor_stack, rf_model__All_New_Final, type = "response")

# For probability of landslide
probability_raster <- predict(predictor_stack, rf_model__All_New_Final, type = "prob", index = 2)

# Save the predicted raster
writeRaster(predicted_raster, filename = "D:/DEM/predicted_RF_All_New.tif", format = "GTiff", overwrite = TRUE)

# Save the probability raster
writeRaster(probability_raster, filename = "D:/DEM/probability_RF_All_New.tif", format = "GTiff", overwrite = TRUE)

## PC All Final


# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)

library(tidyverse)
library(dplyr)
landslides <- landslides %>% select(Slop_PC1,  Elev_PC1, Asp_PC1,  PL_PC1,  PR_PC1,
                                    V,  
                                    TRI_PC1, RO_PC1, TPI_PC1,  Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI
)
Non_landslides <- Non_landslides %>% select(Slop_PC1,  Elev_PC1, Asp_PC1,  PL_PC1,  PR_PC1,
                                            V,   
                                            TRI_PC1, RO_PC1, TPI_PC1,  Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))


Data_m$V <- as.factor(Data_m$V)
# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m[trainIndex, ]
testData <- Data_m[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)


# Train a Random Forest model
rf_model_PC_All<- randomForest(V ~ ., data = trainData, ntree = 1000, mtry = 2, importance = TRUE)

# Summarize the model
print(rf_model_PC_All)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_PC_All, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_PC_All, newdata = testData, type = "response")

# Evaluate the model
confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
print(confusion_matrix_rf)

# Calculate ROC curve and AUC
roc_curve_rf <- roc(testData$V, rf_pred_prob)
auc_value_rf <- auc(roc_curve_rf)
print(paste("AUC for Random Forest:", auc_value_rf))
auc_1m <- as.data.frame(auc_value_rf )
# Plot ROC curve using ggplot2
roc_data_rf <- data.frame(
  specificity = rev(roc_curve_rf$specificities),
  sensitivity = rev(roc_curve_rf$sensitivities)
)

ggplot(roc_data_rf, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Random Forest", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

# Get variable importance
importance_rf <- importance(rf_model_PC_All )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## Mapping

rf_model_PC_All_Final <- randomForest(V ~ ., data = Data_m , ntree = 1000, mtry = 2, importance = TRUE)

library(raster)
library(sp)
library(caret)  # Assuming you used caret for model training


## New
list.files("D:/DEM/Other Factors/")
Landuse <- raster("D:/DEM/Other Factors/Landuse.tif")

Geology= raster("D:/Cheeky Scientist/Factors/Geologic Formation.tif")
Imprevious= raster("D:/Cheeky Scientist/Factors/Imprevious.tif")
LULC_chang= raster("D:/Cheeky Scientist/Factors/LULC_change.tif")
NDVI= raster("D:/Cheeky Scientist/Factors/NDVI.tif")
Rain= raster("D:/Cheeky Scientist/Factors/Rain.tif")
plot(Rain)
crs_Slop_SRTM <- crs(Slop_SRTM)

Landuse <- projectRaster(Landuse, crs = crs_Slop_SRTM)
Geology <- projectRaster(Geology, crs = crs_Slop_SRTM)
Imprevious <- projectRaster(Imprevious, crs = crs_Slop_SRTM)
LULC_chang <- projectRaster(LULC_chang, crs = crs_Slop_SRTM)
NDVI <- projectRaster(NDVI, crs = crs_Slop_SRTM)
Rain <- projectRaster(Rain, crs = crs_Slop_SRTM)
Slop_PC1 <- raster("D:/DEM/PC/Slope_PC1.tif")
Elev_PC1 <- raster("D:/DEM/PC/Elevation_PC1.tif")
Asp_PC1  <- raster("D:/DEM/PC/Aspect_PC1.tif")
PL_PC1   <- raster("D:/DEM/PC/plan_PC1.tif")
PR_PC1   <- raster("D:/DEM/PC/profile_PC1.tif")
TRI_PC1  <- raster("D:/DEM/PC/TRI_PC1.tif")
RO_PC1   <- raster("D:/DEM/PC/Roughness_PC1.tif")
TPI_PC1 <- raster("D:/DEM/PC/TPI_PC1.tif")
PL_PC1 <- projectRaster(PL_PC1 , crs = crs(Slop_SRTM))
PR_PC1 <- projectRaster(PR_PC1, crs = crs(Slop_SRTM))


## ASTER
Slop_PC1=resample(Slop_PC1,Slop_SRTM, resample='bilinear')
Elev_PC1=resample(Elev_PC1,Slop_SRTM, resample='bilinear')
Asp_PC1=resample(Asp_PC1,Slop_SRTM, resample='bilinear')
PL_PC1=resample(PR_PC1,Slop_SRTM, resample='bilinear')
PR_PC1=resample(PR_PC1,Slop_SRTM, resample='bilinear')
TRI_PC1=resample(TRI_PC1,Slop_SRTM, resample='bilinear')
RO_PC1=resample(RO_PC1,Slop_SRTM, resample='bilinear')
TPI_PC1=resample(TPI_PC1,Slop_SRTM, resample='bilinear')

Landuse=resample(Landuse,Slop_SRTM, resample='bilinear')
Geology=resample(Geology,Slop_SRTM, resample='bilinear')
Imprevious=resample(Imprevious,Slop_SRTM, resample='bilinear')
LULC_chang=resample(LULC_chang,Slop_SRTM, resample='bilinear')
Rain=resample(Rain,Slop_SRTM, resample='bilinear')
NDVI=resample(NDVI,Slop_SRTM, resample='bilinear')

## Extent 


## ASTER
Slop_PC1 <- crop(Slop_PC1, extent(Slop_SRTM))
Elev_PC1 <- crop(Elev_PC1, extent(Slop_SRTM))
Asp_PC1  <- crop(Asp_PC1, extent(Slop_SRTM))
PL_PC1  <- crop(PL_PC1, extent(Slop_SRTM))
PR_PC1  <- crop(PR_PC1, extent(Slop_SRTM))
TRI_PC1  <- crop(TRI_PC1, extent(Slop_SRTM))
RO_PC1  <- crop(RO_PC1, extent(Slop_SRTM))
TPI_PC1 <- crop(TPI_PC1, extent(Slop_SRTM))

Landuse<- crop(Landuse, extent(Slop_SRTM))
Geology <- crop(Geology, extent(Slop_SRTM))
Imprevious <- crop(Imprevious, extent(Slop_SRTM))
LULC_chang <- crop(LULC_chang, extent(Slop_SRTM))
Rain <- crop(Rain, extent(Slop_SRTM))
NDVI <- crop(NDVI, extent(Slop_SRTM))

# Stack all predictor rasters
predictor_stack <- stack(Slop_PC1, Elev_PC1, Asp_PC1, PL_PC1, PR_PC1, TRI_PC1, RO_PC1, TPI_PC1,
                         Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)
names(predictor_stack) <- c("Slop_PC1", "Elev_PC1", "Asp_PC1", "PL_PC1", "PR_PC1", "TRI_PC1", "RO_PC1", "TPI_PC1",
                            "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology")

# Train a Random Forest model (final model)
rf_model_PC_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 2, importance = TRUE)

# Make predictions using the trained Random Forest model
predicted_raster <- predict(predictor_stack, rf_model_PC_All_Final, type = "response")

# For probability of landslide
probability_raster <- predict(predictor_stack, rf_model_PC_All_Final, type = "prob", index = 2)

# Save the predicted raster
writeRaster(predicted_raster, filename = "D:/DEM/predicted_PC_All_Final.tif", format = "GTiff", overwrite = TRUE)

# Save the probability raster
writeRaster(probability_raster, filename = "D:/DEM/probability_PC_All_Final.tif", format = "GTiff", overwrite = TRUE)



## PCA (DEM Based)


## PCA (DEM Based)
## PCA Based
## Final New

library(sf)
library(tidyverse)
library(randomForest)

library(caret)
library(pROC)
library(e1071)
library(iml)

# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)

library(tidyverse)
library(dplyr)
landslides <- landslides %>% select(Slop_PC1,  Elev_PC1, Asp_PC1,  PL_PC1,  PR_PC1,
                                    V,  
                                    TRI_PC1, RO_PC1, TPI_PC1,  Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI
)
Non_landslides <- Non_landslides %>% select(Slop_PC1,  Elev_PC1, Asp_PC1,  PL_PC1,  PR_PC1,
                                            V,   
                                            TRI_PC1, RO_PC1, TPI_PC1,  Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))


Data_m$V <- as.factor(Data_m$V)


# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m[trainIndex, ]
testData <- Data_m[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)


## Training Accuracy

# Train a Random Forest model
rf_model_All_PC1 <- randomForest(V ~ ., data = trainData, ntree = 1000, mtry = 2, importance = TRUE)

# Summarize the model
print(rf_model_All_PC1)


## Test Accuracy



# Make predictions on the test set
rf_pred_prob <- predict(rf_model_All_PC1, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_All_PC1, newdata = testData, type = "response")

# Evaluate the model
confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
print(confusion_matrix_rf)


# create new confusion matrix for Actual + Predicted labels
con_matrx_test_SRTM <- table(Actual = testData$V, Predicted =  rf_pred_class)

# convert new confusion matrix to data frame
hm <- as.data.frame(as.table(con_matrx_test_SRTM ))

# create confusion matrix with ggplot2
ggplot(hm, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  guides(fill = FALSE) +
  geom_text(aes(label = Freq), color = "black", size = 10) +
  # following lines only increase text size (optional)
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))
# Calculate ROC curve and AUC
roc_curve_rf <- roc(testData$V, rf_pred_prob)
auc_value_rf <- auc(roc_curve_rf)
print(paste("AUC for Random Forest:", auc_value_rf))
auc_1m <- as.data.frame(auc_value_rf )
# Plot ROC curve using ggplot2
roc_data_rf <- data.frame(
  specificity = rev(roc_curve_rf$specificities),
  sensitivity = rev(roc_curve_rf$sensitivities)
)

ggplot(roc_data_rf, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Random Forest", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

# Get variable importance
importance_rf <- importance(rf_model_All_PC1)
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## Mapping
# Train a Random Forest model
rf_model_All_PC1_Final <- randomForest(V ~ ., data = trainData, ntree = 1000, mtry = 2, importance = TRUE)
library(raster)
library(sp)
library(caret)  # Assuming you used caret for model training
# variables


list.files("D:/DEM/Other Factors/")
Landuse <- raster("D:/DEM/Other Factors/Landuse.tif")

Geology= raster("D:/Cheeky Scientist/Factors/Geologic Formation.tif")
Imprevious= raster("D:/Cheeky Scientist/Factors/Imprevious.tif")
LULC_chang= raster("D:/Cheeky Scientist/Factors/LULC_change.tif")
NDVI= raster("D:/Cheeky Scientist/Factors/NDVI.tif")
Rain= raster("D:/Cheeky Scientist/Factors/Rain.tif")
plot(Rain)
crs_Slop_SRTM <- crs(Slop_SRTM)

Landuse <- projectRaster(Landuse, crs = crs_Slop_SRTM)
Geology <- projectRaster(Geology, crs = crs_Slop_SRTM)
Imprevious <- projectRaster(Imprevious, crs = crs_Slop_SRTM)
LULC_chang <- projectRaster(LULC_chang, crs = crs_Slop_SRTM)
NDVI <- projectRaster(NDVI, crs = crs_Slop_SRTM)
Rain <- projectRaster(Rain, crs = crs_Slop_SRTM)
Slop_PC1 <- raster("D:/DEM/PC/Slope_PC1.tif")
Elev_PC1 <- raster("D:/DEM/PC/Elevation_PC1.tif")
Asp_PC1  <- raster("D:/DEM/PC/Aspect_PC1.tif")
PL_PC1   <- raster("D:/DEM/PC/Plan_PC1.tif")
PR_PC1   <- raster("D:/DEM/PC/Profile_PC1.tif")
#TWI_PC1  <- raster("D:/DEM/PC/New Folder/TWI_PC1.tif")
#SPI_PC1  <- raster("D:/DEM/PC/New Folder/TWI_PC1.tif")
TRI_PC1  <- raster("D:/DEM/PC/TRI_PC1.tif")
RO_PC1   <- raster("D:/DEM/PC/Roughness_PC1.tif")
TPI_PC1  <- raster("D:/DEM/PC/TPI_PC1.tif")
PL_PC1 <- projectRaster(PL_PC1 , crs = crs(Slop_SRTM))
PR_PC1 <- projectRaster(PR_PC1 , crs = crs(Slop_SRTM))
## Resampling
Landuse=resample(Landuse,Slop_SRTM, resample='bilinear')
Geology=resample(Geology,Slop_SRTM, resample='bilinear')
Imprevious=resample(Imprevious,Slop_SRTM, resample='bilinear')
LULC_chang=resample(LULC_chang,Slop_SRTM, resample='bilinear')
Rain=resample(Rain,Slop_SRTM, resample='bilinear')
NDVI=resample(NDVI,Slop_SRTM, resample='bilinear')
Slop_PC1=resample(Slop_PC1,Slop_SRTM, resample='bilinear')
Elev_PC1=resample(Elev_PC1,Slop_SRTM, resample='bilinear')
Asp_PC1=resample(Asp_PC1,Slop_SRTM, resample='bilinear')
PL_PC1=resample(PL_PC1,Slop_SRTM, resample='bilinear')
PR_PC1=resample(PR_PC1,Slop_SRTM, resample='bilinear')
#TWI_SRTM=resample(TWI_SRTM,Slop_SRTM, resample='bilinear')
#SPI_SRTM=resample(SPI_SRTM,Slop_SRTM, resample='bilinear')
TRI_PC1=resample(TRI_PC1,Slop_SRTM, resample='bilinear')
RO_PC1=resample(RO_PC1,Slop_SRTM, resample='bilinear')
TPI_PC1=resample(TPI_PC1,Slop_SRTM, resample='bilinear')

#PL_SRTM <- projectRaster(PL_SRTM , crs = crs(Elev_ASTER))
#PR_SRTM <- projectRaster(PR_SRTM , crs = crs(Elev_ASTER))
# Crop all SRTM rasters to the reference extent

Landuse<- crop(Landuse, extent(Slop_SRTM))
Geology <- crop(Geology, extent(Slop_SRTM))
Imprevious <- crop(Imprevious, extent(Slop_SRTM))
LULC_chang <- crop(LULC_chang, extent(Slop_SRTM))
Rain <- crop(Rain, extent(Slop_SRTM))
NDVI <- crop(NDVI, extent(Slop_SRTM))
Slop_PC1 <- crop(Slop_PC1, extent(Slop_SRTM))
Elev_PC1 <- crop(Elev_PC1, extent(Slop_SRTM))
Asp_PC1  <- crop(Asp_PC1, extent(Slop_SRTM))
PL_PC1   <- crop(PL_PC1, extent(Slop_SRTM))
PR_PC1   <- crop(PR_PC1, extent(Slop_SRTM))
#TWI_SRTM  <- crop(TWI_SRTM, extent(Slop_SRTM))
#SPI_SRTM  <- crop(SPI_SRTM, extent(Slop_SRTM))
TRI_PC1  <- crop(TRI_PC1, extent(Slop_SRTM))
RO_PC1   <- crop(RO_PC1, extent(Slop_SRTM))
TPI_PC1  <- crop(TPI_PC1, extent(Slop_SRTM))

# Stack all predictor rasters
predictor_stack <- stack(Slop_PC1, Elev_PC1, Asp_PC1, PL_PC1, PR_PC1, TRI_PC1, RO_PC1, TPI_PC1,
                         Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)
names(predictor_stack) <- c("Slop_PC1", "Elev_PC1", "Asp_PC1", "PL_PC1", "PR_PC1", "TRI_PC1", "RO_PC1", "TPI_PC1",
                            "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology")

# Train a Random Forest model (final model)
rf_model_PC1_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 2, importance = TRUE)

# Make predictions using the trained Random Forest model
predicted_raster <- predict(predictor_stack, rf_model_PC1_All_Final, type = "response")

# For probability of landslide
probability_raster <- predict(predictor_stack, rf_model_PC1_All_Final, type = "prob", index = 2)

# Save the predicted raster
writeRaster(predicted_raster, filename = "D:/DEM/predicted_PC1_All_Final.tif", format = "GTiff", overwrite = TRUE)

# Save the probability raster
writeRaster(probability_raster, filename = "D:/DEM/probability_PC1_All_Final.tif", format = "GTiff", overwrite = TRUE)



## PCA (DEM Based)
## PCA Based
## Final New

library(sf)
library(tidyverse)
library(randomForest)

library(caret)
library(pROC)
library(e1071)
library(iml)

# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)

library(tidyverse)
library(dplyr)
landslides <- landslides %>% select(Slop_PCA,  Elev_PCA, Asp_PCA,  PL_PCA,  PR_PCA,
                                    V,  
                                    TRI_PCA, RO_PCA, TPI_PCA,  Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI
)
Non_landslides <- Non_landslides %>% select(Slop_PCA,  Elev_PCA, Asp_PCA,  PL_PCA,  PR_PCA,
                                            V,   
                                            TRI_PCA, RO_PCA, TPI_PCA,  Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))


Data_m$V <- as.factor(Data_m$V)


# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m[trainIndex, ]
testData <- Data_m[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)


## Training Accuracy

# Train a Random Forest model
rf_model_All_PCA <- randomForest(V ~ ., data = trainData, ntree = 1000, mtry = 2, importance = TRUE)

# Summarize the model
print(rf_model_All_PCA)


## Test Accuracy



# Make predictions on the test set
rf_pred_prob <- predict(rf_model_All_PCA, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_All_PCA, newdata = testData, type = "response")

# Evaluate the model
confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
print(confusion_matrix_rf)


# create new confusion matrix for Actual + Predicted labels
con_matrx_test_SRTM <- table(Actual = testData$V, Predicted =  rf_pred_class)

# convert new confusion matrix to data frame
hm <- as.data.frame(as.table(con_matrx_test_SRTM ))

# create confusion matrix with ggplot2
ggplot(hm, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  guides(fill = FALSE) +
  geom_text(aes(label = Freq), color = "black", size = 10) +
  # following lines only increase text size (optional)
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30))
# Calculate ROC curve and AUC
roc_curve_rf <- roc(testData$V, rf_pred_prob)
auc_value_rf <- auc(roc_curve_rf)
print(paste("AUC for Random Forest:", auc_value_rf))
auc_1m <- as.data.frame(auc_value_rf )
# Plot ROC curve using ggplot2
roc_data_rf <- data.frame(
  specificity = rev(roc_curve_rf$specificities),
  sensitivity = rev(roc_curve_rf$sensitivities)
)

ggplot(roc_data_rf, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Random Forest", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

# Get variable importance
importance_rf <- importance(rf_model_All_PCA )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## Mapping
# Train a Random Forest model
rf_model_All_PCA_Final <- randomForest(V ~ ., data = trainData, ntree = 1000, mtry = 2, importance = TRUE)
library(raster)
library(sp)
library(caret)  # Assuming you used caret for model training
# variables


list.files("D:/DEM/Other Factors/")
Landuse <- raster("D:/DEM/Other Factors/Landuse.tif")

Geology= raster("D:/Cheeky Scientist/Factors/Geologic Formation.tif")
Imprevious= raster("D:/Cheeky Scientist/Factors/Imprevious.tif")
LULC_chang= raster("D:/Cheeky Scientist/Factors/LULC_change.tif")
NDVI= raster("D:/Cheeky Scientist/Factors/NDVI.tif")
Rain= raster("D:/Cheeky Scientist/Factors/Rain.tif")
plot(Rain)
crs_Slop_SRTM <- crs(Slop_SRTM)

Landuse <- projectRaster(Landuse, crs = crs_Slop_SRTM)
Geology <- projectRaster(Geology, crs = crs_Slop_SRTM)
Imprevious <- projectRaster(Imprevious, crs = crs_Slop_SRTM)
LULC_chang <- projectRaster(LULC_chang, crs = crs_Slop_SRTM)
NDVI <- projectRaster(NDVI, crs = crs_Slop_SRTM)
Rain <- projectRaster(Rain, crs = crs_Slop_SRTM)
Slop_PCA <- raster("D:/DEM/Slope_PCA.tif")
Elev_PCA <- raster("D:/DEM/reconstructed_elevation.tif")
Asp_PCA  <- raster("D:/DEM/Aspect_PCA.tif")
PL_PCA   <- raster("D:/DEM/Plan_PCA.tif")
PR_PCA   <- raster("D:/DEM/Profile_PCA.tif")
#TWI_PCA  <- raster("D:/DEM/New Folder/TWI_PCA.tif")
#SPI_PCA  <- raster("D:/DEM/New Folder/TWI_PCA.tif")
TRI_PCA  <- raster("D:/DEM/TRI_PCA.tif")
RO_PCA   <- raster("D:/DEM/Roughness_PCA.tif")
TPI_PCA  <- raster("D:/DEM/TPI_PCA.tif")
PL_PCA <- projectRaster(PL_PCA , crs = crs(Slop_PCA))
PR_PCA <- projectRaster(PR_PCA , crs = crs(Slop_PCA))
## Resampling
Landuse=resample(Landuse,Slop_SRTM, resample='bilinear')
Geology=resample(Geology,Slop_SRTM, resample='bilinear')
Imprevious=resample(Imprevious,Slop_SRTM, resample='bilinear')
LULC_chang=resample(LULC_chang,Slop_SRTM, resample='bilinear')
Rain=resample(Rain,Slop_SRTM, resample='bilinear')
NDVI=resample(NDVI,Slop_SRTM, resample='bilinear')
Slop_PCA=resample(Slop_PCA,Slop_SRTM, resample='bilinear')
Elev_PCA=resample(Elev_PCA,Slop_SRTM, resample='bilinear')
Asp_PCA=resample(Asp_PCA,Slop_SRTM, resample='bilinear')
PL_PCA=resample(PL_PCA,Slop_SRTM, resample='bilinear')
PR_PCA=resample(PR_PCA,Slop_SRTM, resample='bilinear')
#TWI_SRTM=resample(TWI_SRTM,Slop_SRTM, resample='bilinear')
#SPI_SRTM=resample(SPI_SRTM,Slop_SRTM, resample='bilinear')
TRI_PCA=resample(TRI_PCA,Slop_SRTM, resample='bilinear')
RO_PCA=resample(RO_PCA,Slop_SRTM, resample='bilinear')
TPI_PCA=resample(TPI_PCA,Slop_SRTM, resample='bilinear')

#PL_SRTM <- projectRaster(PL_SRTM , crs = crs(Elev_ASTER))
#PR_SRTM <- projectRaster(PR_SRTM , crs = crs(Elev_ASTER))
# Crop all SRTM rasters to the reference extent

Landuse<- crop(Landuse, extent(Slop_SRTM))
Geology <- crop(Geology, extent(Slop_SRTM))
Imprevious <- crop(Imprevious, extent(Slop_SRTM))
LULC_chang <- crop(LULC_chang, extent(Slop_SRTM))
Rain <- crop(Rain, extent(Slop_SRTM))
NDVI <- crop(NDVI, extent(Slop_SRTM))
Slop_PCA <- crop(Slop_PCA, extent(Slop_SRTM))
Elev_PCA <- crop(Elev_PCA, extent(Slop_SRTM))
Asp_PCA  <- crop(Asp_PCA, extent(Slop_SRTM))
PL_PCA   <- crop(PL_PCA, extent(Slop_SRTM))
PR_PCA   <- crop(PR_PCA, extent(Slop_SRTM))
#TWI_SRTM  <- crop(TWI_SRTM, extent(Slop_SRTM))
#SPI_SRTM  <- crop(SPI_SRTM, extent(Slop_SRTM))
TRI_PCA  <- crop(TRI_PCA, extent(Slop_SRTM))
RO_PCA   <- crop(RO_PCA, extent(Slop_SRTM))
TPI_PCA  <- crop(TPI_PCA, extent(Slop_SRTM))

# Stack all predictor rasters
predictor_stack <- stack(Slop_PCA, Elev_PCA, Asp_PCA, PL_PCA, PR_PCA, TRI_PCA, RO_PCA, TPI_PCA,
                         Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)
names(predictor_stack) <- c("Slop_PCA", "Elev_PCA", "Asp_PCA", "PL_PCA", "PR_PCA", "TRI_PCA", "RO_PCA", "TPI_PCA",
                            "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology")

# Train a Random Forest model (final model)
rf_model_PCA_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 2, importance = TRUE)

# Make predictions using the trained Random Forest model
predicted_raster <- predict(predictor_stack, rf_model_PCA_All_Final, type = "response")

# For probability of landslide
probability_raster <- predict(predictor_stack, rf_model_PCA_All_Final, type = "prob", index = 2)

# Save the predicted raster
writeRaster(predicted_raster, filename = "D:/DEM/predicted_PCA_All_Final.tif", format = "GTiff", overwrite = TRUE)

# Save the probability raster
writeRaster(probability_raster, filename = "D:/DEM/probability_PCA_All_Final.tif", format = "GTiff", overwrite = TRUE)




