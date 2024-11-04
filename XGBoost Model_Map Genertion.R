

## SRTM
# Install packages
install.packages("xgboost")
install.packages("rlang")
install.packages("doSNOW")
install.packages("RStoolbox") 
install.packages("doParallel")
install.packages("Matrix")
install.packages("e1071")

library(xgboost)
#library(rgdal)        # spatial data processing
library(raster)       # raster processing
library(plyr)         # data manipulation 
library(dplyr)        # data manipulation 
library(RStoolbox)    # plotting spatial data 
library(RColorBrewer) # color
library(ggplot2)      # plotting
library(sp)           # spatial data
library(caret)        # machine laerning
library(doParallel)   # Parallel processing
library(doSNOW)
library(e1071)


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
# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
glimpse(Non_landslides)
library(tidyverse)
library(dplyr)
landslides <- landslides %>% select(V,
                                    Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, 
                                    TRI_SRTM, RO_SRTM, TPI_SRTM,
                                    Imprevious , LULC_chang, Geology, Rain, NDVI, Landuse)
Non_landslides <- Non_landslides %>% select(V,
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


Data_m$V <- as.factor(Data_m$V)

##Conversion of Value to YES and NO
Data_m$V=ifelse(Data_m$V == 1, "yes","no")

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

## XGBoost function ------------------------------------------------

#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)



tune_grid <- expand.grid(nrounds = 200,           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = 6,            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = 0.3,               # control the learning rate
                         gamma = 0,             # minimum loss reduction required
                         colsample_bytree = 1,  # subsample ratio of columns when constructing each tree
                         min_child_weight = 1,     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = 1)          # subsample ratio of the training instance


# Step 5 modeling
set.seed(849)
names(Data_m)
fit.xgb_SRTM<- train(V~., 
                           data=trainData,
                           method = "xgbTree",
                           metric= "Accuracy",
                           preProc = c("center", "scale"), 
                           trControl = myControl,
                           tuneGrid = tune_grid,
                           tuneLength = 10)
fit.xgb_SRTM$results

fit.xgb_SRTM$resample$Accuracy
X.xgb = varImp(fit.xgb_SRTM)
plot(X.xgb)
#Confusion Matrix - train data
p1<-predict(fit.xgb_SRTM, testData[,c(-1)], type = "raw")
confusionMatrix(p1, as.factor(testData$V))  # using more deep tree, the accuracy linearly increases! 

# # Make predictions on the test set
# rf_pred_prob <- predict(fit.xgb_SRTM , newdata = testData, type = "raw")[,2]
# rf_pred_class <- predict(fit.xgb_SRTM , newdata = testData, type = "response")

# # Evaluate the model
# confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
# print(confusion_matrix_rf)
library(raster)
library(sp)
library(caret) 
library(pROC)# Assuming you used caret for model training

# Ensure pROC package is loaded
library(pROC)

# Predict probabilities for the positive class (assuming positive class is "1")
rf_pred_prob_full <- predict(fit.xgb_SRTM, newdata = testData[, -which(names(testData) == "V")], type = "prob")

# View the column names to identify the correct positive class label
colnames(rf_pred_prob_full)

# Replace 'positive_class' with the actual positive class label obtained from column names
positive_class <- "1"  # Example, replace with actual class name

# Predict probabilities for the positive class
rf_pred_prob <- rf_pred_prob_full[, positive_class]

# Ensure testData$V is a factor with levels corresponding to the positive and negative classes
# Replace "0" and "1" with actual class labels if different
testData$V <- factor(testData$V, levels = c("0", "1"))

# Calculate ROC curve
roc_curve <- roc(testData$V, rf_pred_prob)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for XGBoost Model")

# Print the AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))



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



# Train a Random Forest model (final model)

fit.xgb_SRTM_Final<- train(V~., 
                           data=Data_m,
                           method = "xgbTree",
                           metric= "Accuracy",
                           preProc = c("center", "scale"), 
                           trControl = myControl,
                           tuneGrid = tune_grid,
                           tuneLength = 10)


# ## SRTM
# "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology"
#
writeRaster(Landuse,filename="D:/DEM/resampled/Landuse.tif", format="GTiff", overwrite=TRUE)
writeRaster(LULC_chang,filename="D:/DEM/resampled/LULC_chang.tif", format="GTiff", overwrite=TRUE)
writeRaster(Imprevious,filename="D:/DEM/resampled/Imprevious.tif", format="GTiff", overwrite=TRUE)
writeRaster(Rain,filename="D:/DEM/resampled/Rain.tif", format="GTiff", overwrite=TRUE)
writeRaster(NDVI,filename="D:/DEM/resampled/NDVI.tif", format="GTiff", overwrite=TRUE)
writeRaster(Geology,filename="D:/DEM/resampled/Geology.tif", format="GTiff", overwrite=TRUE)
writeRaster(Slop_SRTM,filename="D:/DEM/resampled/Slop_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(Elev_SRTM,filename="D:/DEM/resampled/Elev_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(Asp_SRTM,filename="D:/DEM/resampled/Asp_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(PL_SRTM,filename="D:/DEM/resampled/PL_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(PR_SRTM,filename="D:/DEM/resampled/PR_SRTM.tif", format="GTiff", overwrite=TRUE)
#writeRaster(TWI_SRTM,filename="D:/DEM/resampled_all/TWI_SRTM.tif", format="GTiff", overwrite=TRUE)
#writeRaster(SPI_SRTM,filename="D:/DEM/resampled_all/SPI_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(TRI_SRTM,filename="D:/DEM/resampled/TRI_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(RO_SRTM,filename="D:/DEM/resampled/RO_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(TPI_SRTM,filename="D:/DEM/resampled/TPI_SRTM.tif", format="GTiff", overwrite=TRUE)




## stack multiple raster files
Stack_List= list.files(path = "D:/DEM/resampled/",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)

names(Rasters)

# 6-1-1 Convert rasters to dataframe with Long-Lat -----------------------
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)

# Now:Prediction using imported Rasters

# check the varaibles names to match with training data
#colnames(Rasters.df)[4] <- "ElEVATION"   # change columns names 
#colnames(Rasters.df)[6] <- "SlOPE"   # change columns names 

#head(Rasters.df[,c(-9,-10)],1)
#head(nn.ce$covariate,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y


##PRODUCE PROBABILITY MAP
p3<-as.data.frame(predict(fit.xgb_SRTM_Final, Rasters.df_N, type = "prob"))
summary(p3)
Rasters.df$Levels_yes<-p3$yes
Rasters.df$Levels_no<-p3$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(Slop_SRTM))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(Slop_SRTM))

spplot(r_ave_yes, main="Landslides SM using XGB")
writeRaster(r_ave_yes,filename="D:/DEM/resampled/XGB_SRTM_All.tif", format="GTiff", overwrite=TRUE) 




## All 30 Factors


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

## XGBoost function ------------------------------------------------

#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)



tune_grid <- expand.grid(nrounds = 200,           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = 6,            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = 0.3,               # control the learning rate
                         gamma = 0,             # minimum loss reduction required
                         colsample_bytree = 1,  # subsample ratio of columns when constructing each tree
                         min_child_weight = 1,     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = 1)          # subsample ratio of the training instance


# Step 5 modeling
set.seed(849)
names(Data_m)
fit.xgb_All<- train(V~., 
                    data=trainData,
                    method = "xgbTree",
                    metric= "Accuracy",
                    preProc = c("center", "scale"), 
                    trControl = myControl,
                    tuneGrid = tune_grid,
                    tuneLength = 10)
fit.xgb_All$results

fit.xgb_All$resample$Accuracy
X.xgb = varImp(fit.xgb_All)
plot(X.xgb)
#Confusion Matrix - train data
p1<-predict(fit.xgb_All, testData[,c(-6)], type = "raw")
confusionMatrix(p1, as.factor(testData$V))  # using more deep tree, the accuracy linearly increases! 

# # Make predictions on the test set
# rf_pred_prob <- predict(fit.xgb_SRTM , newdata = testData, type = "raw")[,2]
# rf_pred_class <- predict(fit.xgb_SRTM , newdata = testData, type = "response")

# Step 6: Calculate ROC AUC and plot ROC curve
# Predict probabilities for the positive class (assuming positive class is "1")
rf_pred_prob_full <- predict(fit.xgb_All, newdata = testData[, -which(names(testData) == "V")], type = "prob")

# View the column names to identify the correct positive class label
colnames(rf_pred_prob_full)

# Replace 'positive_class' with the actual positive class label obtained from column names
positive_class <- "1"  # Example, replace with actual class name

# Predict probabilities for the positive class
rf_pred_prob <- rf_pred_prob_full[, positive_class]

# Ensure testData$V is a factor with levels corresponding to the positive and negative classes
# Replace "0" and "1" with actual class labels if different
testData$V <- factor(testData$V, levels = c("0", "1"))

# Calculate ROC curve
roc_curve <- roc(testData$V, rf_pred_prob)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for XGBoost Model")

# Print the AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))




## Map Generation

# Train a Random Forest model
#rf_model_36_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)

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



# # Stack all predictor rasters
# predictor_stack <- stack(Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, TRI_SRTM, RO_SRTM, TPI_SRTM,
#                          Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, TRI_ASTER, RO_ASTER, TPI_ASTER,
#                          Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, TRI_EDNA, RO_EDNA, TPI_EDNA, Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)
# names(predictor_stack) <- c("Slop_SRTM", "Elev_SRTM", "Asp_SRTM", "PL_SRTM", "PR_SRTM", "TRI_SRTM", "RO_SRTM", "TPI_SRTM",
#                             "Slop_ASTER", "Elev_ASTER", "Asp_ASTER", "PL_ASTER", "PR_ASTER", "TRI_ASTER", "RO_ASTER", "TPI_ASTER",
#                             "Slop_EDNA", "Elev_EDNA", "Asp_EDNA", "PL_EDNA", "PR_EDNA", "TRI_EDNA", "RO_EDNA", "TPI_EDNA", "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology")


# Train a Random Forest model (final model)
# Train a Random Forest model (final model)

fit.xgb_All_Final<- train(V~., 
                         data=Data_m,
                         method = "xgbTree",
                         metric= "Accuracy",
                         preProc = c("center", "scale"), 
                         trControl = myControl,
                         tuneGrid = tune_grid,
                         tuneLength = 10)




# ## SRTM
# "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology"
#
writeRaster(Landuse,filename="D:/DEM/resampled_all/Landuse.tif", format="GTiff", overwrite=TRUE)
writeRaster(LULC_chang,filename="D:/DEM/resampled_all/LULC_chang.tif", format="GTiff", overwrite=TRUE)
writeRaster(Imprevious,filename="D:/DEM/resampled_all/Imprevious.tif", format="GTiff", overwrite=TRUE)
writeRaster(Rain,filename="D:/DEM/resampled_all/Rain.tif", format="GTiff", overwrite=TRUE)
writeRaster(NDVI,filename="D:/DEM/resampled_all/NDVI.tif", format="GTiff", overwrite=TRUE)
writeRaster(Geology,filename="D:/DEM/resampled_all/Geology.tif", format="GTiff", overwrite=TRUE)
writeRaster(Slop_SRTM,filename="D:/DEM/resampled_all/Slop_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(Elev_SRTM,filename="D:/DEM/resampled_all/Elev_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(Asp_SRTM,filename="D:/DEM/resampled_all/Asp_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(PL_SRTM,filename="D:/DEM/resampled_all/PL_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(PR_SRTM,filename="D:/DEM/resampled_all/PR_SRTM.tif", format="GTiff", overwrite=TRUE)
#writeRaster(TWI_SRTM,filename="D:/DEM/resampled_all/TWI_SRTM.tif", format="GTiff", overwrite=TRUE)
#writeRaster(SPI_SRTM,filename="D:/DEM/resampled_all/SPI_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(TRI_SRTM,filename="D:/DEM/resampled_all/TRI_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(RO_SRTM,filename="D:/DEM/resampled_all/RO_SRTM.tif", format="GTiff", overwrite=TRUE)
writeRaster(TPI_SRTM,filename="D:/DEM/resampled_all/TPI_SRTM.tif", format="GTiff", overwrite=TRUE)


##ASTER

writeRaster(Slop_ASTER,filename="D:/DEM/resampled_all/Slop_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(Elev_ASTER,filename="D:/DEM/resampled_all/Elev_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(Asp_ASTER,filename="D:/DEM/resampled_all/Asp_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(PL_ASTER,filename="D:/DEM/resampled_all/PL_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(PR_ASTER,filename="D:/DEM/resampled_all/PR_ASTER.tif", format="GTiff", overwrite=TRUE)
#writeRaster(TWI_ASTER,filename="D:/DEM/resampled_all/TWI_ASTER.tif", format="GTiff", overwrite=TRUE)
#writeRaster(SPI_ASTER,filename="D:/DEM/resampled_all/SPI_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(TRI_ASTER,filename="D:/DEM/resampled_all/TRI_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(RO_ASTER,filename="D:/DEM/resampled_all/RO_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(TPI_ASTER,filename="D:/DEM/resampled_all/TPI_ASTER.tif", format="GTiff", overwrite=TRUE)

## EDNA

writeRaster(Slop_EDNA,filename="D:/DEM/resampled_all/Slop_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(Elev_EDNA,filename="D:/DEM/resampled_all/Elev_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(Asp_EDNA,filename="D:/DEM/resampled_all/Asp_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(PL_EDNA,filename="D:/DEM/resampled_all/PL_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(PR_EDNA,filename="D:/DEM/resampled_all/PR_EDNA.tif", format="GTiff", overwrite=TRUE)
#writeRaster(TWI_EDNA,filename="D:/DEM/resampled_all/TWI_EDNA.tif", format="GTiff", overwrite=TRUE)
#writeRaster(SPI_EDNA,filename="D:/DEM/resampled_all/SPI_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(TRI_EDNA,filename="D:/DEM/resampled_all/TRI_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(RO_EDNA,filename="D:/DEM/resampled_all/RO_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(TPI_EDNA,filename="D:/DEM/resampled_all/TPI_EDNA.tif", format="GTiff", overwrite=TRUE)

#
## stack multiple raster files
Stack_List= list.files(path = "D:/DEM/resampled_all",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)
names(Rasters)
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)



# Now:Prediction using imported Rasters

# check the varaibles names to match with training data
#colnames(Rasters.df)[4] <- "ElEVATION"   # change columns names 
#colnames(Rasters.df)[6] <- "SlOPE"   # change columns names 

#head(Rasters.df[,c(-9,-10)],1)
#head(nn.ce$covariate,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y


##PRODUCE PROBABILITY MAP
p3<-as.data.frame(predict(fit.xgb_All_Final, Rasters.df_N, type = "prob"))
summary(p3)

names(p3)[1] <- "no"
names(p3)[2] <- "yes"
Rasters.df$Levels_yes<-p3$yes
Rasters.df$Levels_no<-p3$no
ww <- as.data.frame(x)
x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(Slop_SRTM))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(Slop_SRTM))

spplot(r_ave_yes, main="Landslides SM using XGB")
writeRaster(r_ave_yes,filename="D:/DEM/resampled_all/XGB_Factors_All.tif", format="GTiff", overwrite=TRUE) 







## EDNA

## Check with 30 DEM based Factors and Others

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
                                     TRI_EDNA, RO_EDNA, TPI_EDNA,
                                     
                                     Imprevious , LULC_chang, Geology, Rain, NDVI, Landuse)
Non_landslides <- Non_landslides %>% select( V,
                                             
                                             Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA,  
                                             TRI_EDNA, RO_EDNA, TPI_EDNA,
                                             
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

#Scale the Variables
# Load necessary library
library(dplyr)


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

## XGBoost function ------------------------------------------------

#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)



tune_grid <- expand.grid(nrounds = 200,           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = 6,            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = 0.3,               # control the learning rate
                         gamma = 0,             # minimum loss reduction required
                         colsample_bytree = 1,  # subsample ratio of columns when constructing each tree
                         min_child_weight = 1,     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = 1)          # subsample ratio of the training instance


# Step 5 modeling
set.seed(849)
names(Data_m)
fit.xgb_EDNA<- train(V~., 
                     data=trainData,
                     method = "xgbTree",
                     metric= "Accuracy",
                     preProc = c("center", "scale"), 
                     trControl = myControl,
                     tuneGrid = tune_grid,
                     tuneLength = 10)
fit.xgb_EDNA$results

fit.xgb_EDNA$resample$Accuracy
X.xgb = varImp(fit.xgb_EDNA)
plot(X.xgb)
#Confusion Matrix - train data
p1<-predict(fit.xgb_EDNA, testData[,c(-1)], type = "raw")
confusionMatrix(p1, as.factor(testData$V))  # using more deep tree, the accuracy linearly increases! 

# # Make predictions on the test set
# rf_pred_prob <- predict(fit.xgb_SRTM , newdata = testData, type = "raw")[,2]
# rf_pred_class <- predict(fit.xgb_SRTM , newdata = testData, type = "response")

# # Evaluate the model
# confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
# print(confusion_matrix_rf)
library(raster)
library(sp)
library(caret) 
library(pROC)# Assuming you used caret for model training

# Ensure pROC package is loaded
library(pROC)

# Step 6: Calculate ROC AUC and plot ROC curve
# Predict probabilities for the positive class (assuming positive class is "1")
rf_pred_prob_full <- predict(fit.xgb_EDNA, newdata = testData[, -which(names(testData) == "V")], type = "prob")

# View the column names to identify the correct positive class label
colnames(rf_pred_prob_full)

# Replace 'positive_class' with the actual positive class label obtained from column names
positive_class <- "1"  # Example, replace with actual class name

# Predict probabilities for the positive class
rf_pred_prob <- rf_pred_prob_full[, positive_class]

# Ensure testData$V is a factor with levels corresponding to the positive and negative classes
# Replace "0" and "1" with actual class labels if different
testData$V <- factor(testData$V, levels = c("0", "1"))

# Calculate ROC curve
roc_curve <- roc(testData$V, rf_pred_prob)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for XGBoost Model")

# Print the AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))






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





# Train a Random Forest model (final model)
# Train a Random Forest model (final model)

fit.xgb_EDNA_Final<- train(V~., 
                           data=Data_m,
                           method = "xgbTree",
                           metric= "Accuracy",
                           preProc = c("center", "scale"), 
                           trControl = myControl,
                           tuneGrid = tune_grid,
                           tuneLength = 10)




# ## SRTM
# "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology"
#
writeRaster(Landuse,filename="D:/DEM/resampled_EDNA/Landuse.tif", format="GTiff", overwrite=TRUE)
writeRaster(LULC_chang,filename="D:/DEM/resampled_EDNA/LULC_chang.tif", format="GTiff", overwrite=TRUE)
writeRaster(Imprevious,filename="D:/DEM/resampled_EDNA/Imprevious.tif", format="GTiff", overwrite=TRUE)
writeRaster(Rain,filename="D:/DEM/resampled_EDNA/Rain.tif", format="GTiff", overwrite=TRUE)
writeRaster(NDVI,filename="D:/DEM/resampled_EDNA/NDVI.tif", format="GTiff", overwrite=TRUE)
writeRaster(Geology,filename="D:/DEM/resampled_EDNA/Geology.tif", format="GTiff", overwrite=TRUE)

## EDNA

writeRaster(Slop_EDNA,filename="D:/DEM/resampled_EDNA/Slop_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(Elev_EDNA,filename="D:/DEM/resampled_EDNA/Elev_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(Asp_EDNA,filename="D:/DEM/resampled_EDNA/Asp_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(PL_EDNA,filename="D:/DEM/resampled_EDNA/PL_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(PR_EDNA,filename="D:/DEM/resampled_EDNA/PR_EDNA.tif", format="GTiff", overwrite=TRUE)
#writeRaster(TWI_EDNA,filename="D:/DEM/resampled_all/TWI_EDNA.tif", format="GTiff", overwrite=TRUE)
#writeRaster(SPI_EDNA,filename="D:/DEM/resampled_all/SPI_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(TRI_EDNA,filename="D:/DEM/resampled_EDNA/TRI_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(RO_EDNA,filename="D:/DEM/resampled_EDNA/RO_EDNA.tif", format="GTiff", overwrite=TRUE)
writeRaster(TPI_EDNA,filename="D:/DEM/resampled_EDNA/TPI_EDNA.tif", format="GTiff", overwrite=TRUE)

#
## stack multiple raster files
Stack_List= list.files(path = "D:/DEM/resampled_EDNA",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)
names(Rasters)
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)



# Now:Prediction using imported Rasters

# check the varaibles names to match with training data
#colnames(Rasters.df)[4] <- "ElEVATION"   # change columns names 
#colnames(Rasters.df)[6] <- "SlOPE"   # change columns names 

#head(Rasters.df[,c(-9,-10)],1)
#head(nn.ce$covariate,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y


##PRODUCE PROBABILITY MAP
p3<-as.data.frame(predict(fit.xgb_EDNA_Final, Rasters.df_N, type = "prob"))
summary(p3)
names(p3)[1] <- "no"
names(p3)[2] <- "yes"
Rasters.df$Levels_yes<-p3$yes
Rasters.df$Levels_no<-p3$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(Slop_SRTM))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(Slop_SRTM))

spplot(r_ave_yes, main="Landslides SM using XGB")
writeRaster(r_ave_yes,filename="D:/DEM/resampled_EDNA/XGB_EDNA_All.tif", format="GTiff", overwrite=TRUE) 


## ASTER
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

## XGBoost function ------------------------------------------------

#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)



tune_grid <- expand.grid(nrounds = 200,           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = 6,            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = 0.3,               # control the learning rate
                         gamma = 0,             # minimum loss reduction required
                         colsample_bytree = 1,  # subsample ratio of columns when constructing each tree
                         min_child_weight = 1,     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = 1)          # subsample ratio of the training instance


# Step 5 modeling
set.seed(849)
names(Data_m)
fit.xgb_ASTER<- train(V~., 
                      data=trainData,
                      method = "xgbTree",
                      metric= "Accuracy",
                      preProc = c("center", "scale"), 
                      trControl = myControl,
                      tuneGrid = tune_grid,
                      tuneLength = 10)
fit.xgb_ASTER$results

fit.xgb_ASTER$resample$Accuracy
X.xgb = varImp(fit.xgb_ASTER)
plot(X.xgb)
#Confusion Matrix - train data
p1<-predict(fit.xgb_ASTER, testData[,c(-6)], type = "raw")
confusionMatrix(p1, as.factor(testData$V))  # using more deep tree, the accuracy linearly increases! 

# # Make predictions on the test set
# rf_pred_prob <- predict(fit.xgb_SRTM , newdata = testData, type = "raw")[,2]
# rf_pred_class <- predict(fit.xgb_SRTM , newdata = testData, type = "response")

# # Evaluate the model
# confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
# print(confusion_matrix_rf)
library(raster)
library(sp)
library(caret) 
library(pROC)# Assuming you used caret for model training

# Ensure pROC package is loaded
library(pROC)

# Step 6: Calculate ROC AUC and plot ROC curve
# Predict probabilities for the positive class (assuming positive class is "1")
rf_pred_prob_full <- predict(fit.xgb_ASTER, newdata = testData[, -which(names(testData) == "V")], type = "prob")

# View the column names to identify the correct positive class label
colnames(rf_pred_prob_full)

# Replace 'positive_class' with the actual positive class label obtained from column names
positive_class <- "1"  # Example, replace with actual class name

# Predict probabilities for the positive class
rf_pred_prob <- rf_pred_prob_full[, positive_class]

# Ensure testData$V is a factor with levels corresponding to the positive and negative classes
# Replace "0" and "1" with actual class labels if different
testData$V <- factor(testData$V, levels = c("0", "1"))

# Calculate ROC curve
roc_curve <- roc(testData$V, rf_pred_prob)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for XGBoost Model")

# Print the AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

## Map Generation

# Train a Random Forest model
#rf_model_36_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)

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

#
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






# # Stack all predictor rasters
# predictor_stack <- stack(Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, TRI_SRTM, RO_SRTM, TPI_SRTM,
#                          Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, TRI_ASTER, RO_ASTER, TPI_ASTER,
#                          Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, TRI_EDNA, RO_EDNA, TPI_EDNA, Landuse, LULC_chang, Imprevious, Rain, Geology, NDVI)
# names(predictor_stack) <- c("Slop_SRTM", "Elev_SRTM", "Asp_SRTM", "PL_SRTM", "PR_SRTM", "TRI_SRTM", "RO_SRTM", "TPI_SRTM",
#                             "Slop_ASTER", "Elev_ASTER", "Asp_ASTER", "PL_ASTER", "PR_ASTER", "TRI_ASTER", "RO_ASTER", "TPI_ASTER",
#                             "Slop_EDNA", "Elev_EDNA", "Asp_EDNA", "PL_EDNA", "PR_EDNA", "TRI_EDNA", "RO_EDNA", "TPI_EDNA", "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology")


# Train a Random Forest model (final model)
# Train a Random Forest model (final model)

fit.xgb_ASTER_Final<- train(V~., 
                            data=Data_m,
                            method = "xgbTree",
                            metric= "Accuracy",
                            preProc = c("center", "scale"), 
                            trControl = myControl,
                            tuneGrid = tune_grid,
                            tuneLength = 10)




# ## SRTM
# "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology"
#
writeRaster(Landuse,filename="D:/DEM/resampled_ASTER/Landuse.tif", format="GTiff", overwrite=TRUE)
writeRaster(LULC_chang,filename="D:/DEM/resampled_ASTER/LULC_chang.tif", format="GTiff", overwrite=TRUE)
writeRaster(Imprevious,filename="D:/DEM/resampled_ASTER/Imprevious.tif", format="GTiff", overwrite=TRUE)
writeRaster(Rain,filename="D:/DEM/resampled_ASTER/Rain.tif", format="GTiff", overwrite=TRUE)
writeRaster(NDVI,filename="D:/DEM/resampled_ASTER/NDVI.tif", format="GTiff", overwrite=TRUE)
writeRaster(Geology,filename="D:/DEM/resampled_ASTER/Geology.tif", format="GTiff", overwrite=TRUE)

##ASTER

writeRaster(Slop_ASTER,filename="D:/DEM/resampled_ASTER/Slop_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(Elev_ASTER,filename="D:/DEM/resampled_ASTER/Elev_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(Asp_ASTER,filename="D:/DEM/resampled_ASTER/Asp_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(PL_ASTER,filename="D:/DEM/resampled_ASTER/PL_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(PR_ASTER,filename="D:/DEM/resampled_ASTER/PR_ASTER.tif", format="GTiff", overwrite=TRUE)
#writeRaster(TWI_ASTER,filename="D:/DEM/resampled_ASTER/TWI_ASTER.tif", format="GTiff", overwrite=TRUE)
#writeRaster(SPI_ASTER,filename="D:/DEM/resampled_ASTER/SPI_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(TRI_ASTER,filename="D:/DEM/resampled_ASTER/TRI_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(RO_ASTER,filename="D:/DEM/resampled_ASTER/RO_ASTER.tif", format="GTiff", overwrite=TRUE)
writeRaster(TPI_ASTER,filename="D:/DEM/resampled_ASTER/TPI_ASTER.tif", format="GTiff", overwrite=TRUE)



#
## stack multiple raster files
Stack_List= list.files(path = "D:/DEM/resampled_ASTER",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)
names(Rasters)
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)



# Now:Prediction using imported Rasters

# check the varaibles names to match with training data
#colnames(Rasters.df)[4] <- "ElEVATION"   # change columns names 
#colnames(Rasters.df)[6] <- "SlOPE"   # change columns names 

#head(Rasters.df[,c(-9,-10)],1)
#head(nn.ce$covariate,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y


##PRODUCE PROBABILITY MAP
p3<-as.data.frame(predict(fit.xgb_ASTER_Final, Rasters.df_N, type = "prob"))
summary(p3)

names(p3)[1] <- "no"
names(p3)[2] <- "yes"
Rasters.df$Levels_yes<-p3$yes
Rasters.df$Levels_no<-p3$no
ww <- as.data.frame(x)
x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(Slop_SRTM))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(Slop_SRTM))

spplot(r_ave_yes, main="Landslides SM using XGB")
writeRaster(r_ave_yes,filename="D:/DEM/resampled_ASTER/XGB_ASTER_All.tif", format="GTiff", overwrite=TRUE) 


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


## XGBoost function ------------------------------------------------

#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)



tune_grid <- expand.grid(nrounds = 200,           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = 6,            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = 0.3,               # control the learning rate
                         gamma = 0,             # minimum loss reduction required
                         colsample_bytree = 1,  # subsample ratio of columns when constructing each tree
                         min_child_weight = 1,     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = 1)          # subsample ratio of the training instance


# Step 5 modeling
set.seed(849)
names(Data_m)
fit.xgb_DEM<- train(V~., 
                    data=trainData,
                    method = "xgbTree",
                    metric= "Accuracy",
                    preProc = c("center", "scale"), 
                    trControl = myControl,
                    tuneGrid = tune_grid,
                    tuneLength = 10)
fit.xgb_DEM$results

fit.xgb_DEM$resample$Accuracy
X.xgb = varImp(fit.xgb_DEM)
plot(X.xgb)
#Confusion Matrix - train data
p1<-predict(fit.xgb_DEM, testData[,c(-6)], type = "raw")
confusionMatrix(p1, as.factor(testData$V))  # using more deep tree, the accuracy linearly increases! 

# # Make predictions on the test set
# rf_pred_prob <- predict(fit.xgb_SRTM , newdata = testData, type = "raw")[,2]
# rf_pred_class <- predict(fit.xgb_SRTM , newdata = testData, type = "response")

# # Evaluate the model
# confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
# print(confusion_matrix_rf)
library(raster)
library(sp)
library(caret) 
library(pROC)# Assuming you used caret for model training

# Ensure pROC package is loaded
library(pROC)

# Step 6: Calculate ROC AUC and plot ROC curve
# Predict probabilities for the positive class (assuming positive class is "1")
rf_pred_prob_full <- predict(fit.xgb_DEM, newdata = testData[, -which(names(testData) == "V")], type = "prob")

# View the column names to identify the correct positive class label
colnames(rf_pred_prob_full)

# Replace 'positive_class' with the actual positive class label obtained from column names
positive_class <- "1"  # Example, replace with actual class name

# Predict probabilities for the positive class
rf_pred_prob <- rf_pred_prob_full[, positive_class]

# Ensure testData$V is a factor with levels corresponding to the positive and negative classes
# Replace "0" and "1" with actual class labels if different
testData$V <- factor(testData$V, levels = c("0", "1"))

# Calculate ROC curve
roc_curve <- roc(testData$V, rf_pred_prob)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for XGBoost Model")

# Print the AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

## Map Generation

# Train a Random Forest model
#rf_model_36_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)

library(raster)
library(sp)
library(caret)  # Assuming you used caret for model training


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


# Train a Random Forest model (final model)

fit.xgb_DEM_Final<- train(V~., 
                          data=Data_m,
                          method = "xgbTree",
                          metric= "Accuracy",
                          preProc = c("center", "scale"), 
                          trControl = myControl,
                          tuneGrid = tune_grid,
                          tuneLength = 10)


# ## SRTM
# "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology"
#
writeRaster(Landuse,filename="D:/DEM/resampled_DEM/Landuse.tif", format="GTiff", overwrite=TRUE)
writeRaster(LULC_chang,filename="D:/DEM/resampled_DEM/LULC_chang.tif", format="GTiff", overwrite=TRUE)
writeRaster(Imprevious,filename="D:/DEM/resampled_DEM/Imprevious.tif", format="GTiff", overwrite=TRUE)
writeRaster(Rain,filename="D:/DEM/resampled_DEM/Rain.tif", format="GTiff", overwrite=TRUE)
writeRaster(NDVI,filename="D:/DEM/resampled_DEM/NDVI.tif", format="GTiff", overwrite=TRUE)
writeRaster(Geology,filename="D:/DEM/resampled_DEM/Geology.tif", format="GTiff", overwrite=TRUE)

##ASTER

writeRaster(Slop_DEM,filename="D:/DEM/resampled_DEM/Slop_DEM.tif", format="GTiff", overwrite=TRUE)
writeRaster(Elev_DEM,filename="D:/DEM/resampled_DEM/Elev_DEM.tif", format="GTiff", overwrite=TRUE)
writeRaster(Asp_DEM,filename="D:/DEM/resampled_DEM/Asp_DEM.tif", format="GTiff", overwrite=TRUE)
writeRaster(PL_DEM,filename="D:/DEM/resampled_DEM/PL_DEM.tif", format="GTiff", overwrite=TRUE)
writeRaster(PR_DEM,filename="D:/DEM/resampled_DEM/PR_DEM.tif", format="GTiff", overwrite=TRUE)
#writeRaster(TWI_DEM,filename="D:/DEM/resampled_DEM/TWI_DEM.tif", format="GTiff", overwrite=TRUE)
#writeRaster(SPI_DEM,filename="D:/DEM/resampled_DEM/SPI_DEM.tif", format="GTiff", overwrite=TRUE)
writeRaster(TRI_DEM,filename="D:/DEM/resampled_DEM/TRI_DEM.tif", format="GTiff", overwrite=TRUE)
writeRaster(RO_DEM,filename="D:/DEM/resampled_DEM/RO_DEM.tif", format="GTiff", overwrite=TRUE)
writeRaster(TPI_DEM,filename="D:/DEM/resampled_DEM/TPI_DEM.tif", format="GTiff", overwrite=TRUE)



#
## stack multiple raster files
Stack_List= list.files(path = "D:/DEM/resampled_DEM",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)
names(Rasters)
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)



# Now:Prediction using imported Rasters

# check the varaibles names to match with training data
#colnames(Rasters.df)[4] <- "ElEVATION"   # change columns names 
#colnames(Rasters.df)[6] <- "SlOPE"   # change columns names 

#head(Rasters.df[,c(-9,-10)],1)
#head(nn.ce$covariate,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y


##PRODUCE PROBABILITY MAP
p3<-as.data.frame(predict(fit.xgb_DEM_Final, Rasters.df_N, type = "prob"))
summary(p3)

names(p3)[1] <- "no"
names(p3)[2] <- "yes"
Rasters.df$Levels_yes<-p3$yes
Rasters.df$Levels_no<-p3$no
ww <- as.data.frame(x)
x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(Slop_SRTM))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(Slop_SRTM))

spplot(r_ave_yes, main="Landslides SM using XGB")
writeRaster(r_ave_yes,filename="D:/DEM/resampled_DEM/XGB_DEM_All.tif", format="GTiff", overwrite=TRUE) 

## Combined

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

## XGBoost function ------------------------------------------------

#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)



tune_grid <- expand.grid(nrounds = 200,           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = 6,            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = 0.3,               # control the learning rate
                         gamma = 0,             # minimum loss reduction required
                         colsample_bytree = 1,  # subsample ratio of columns when constructing each tree
                         min_child_weight = 1,     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = 1)          # subsample ratio of the training instance


# Step 5 modeling
set.seed(849)
names(Data_m)
fit.xgb_New<- train(V~., 
                    data=trainData,
                    method = "xgbTree",
                    metric= "Accuracy",
                    preProc = c("center", "scale"), 
                    trControl = myControl,
                    tuneGrid = tune_grid,
                    tuneLength = 10)
fit.xgb_New$results

fit.xgb_New$resample$Accuracy
X.xgb = varImp(fit.xgb_New)
plot(X.xgb)
#Confusion Matrix - train data
p1<-predict(fit.xgb_New, testData[,c(-6)], type = "raw")
confusionMatrix(p1, as.factor(testData$V))  # using more deep tree, the accuracy linearly increases! 

# # Make predictions on the test set
# rf_pred_prob <- predict(fit.xgb_SRTM , newdata = testData, type = "raw")[,2]
# rf_pred_class <- predict(fit.xgb_SRTM , newdata = testData, type = "response")

# # Evaluate the model
# confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
# print(confusion_matrix_rf)
library(raster)
library(sp)
library(caret) 
library(pROC)# Assuming you used caret for model training

# Ensure pROC package is loaded
library(pROC)

# Step 6: Calculate ROC AUC and plot ROC curve
# Predict probabilities for the positive class (assuming positive class is "1")
rf_pred_prob_full <- predict(fit.xgb_New, newdata = testData[, -which(names(testData) == "V")], type = "prob")

# View the column names to identify the correct positive class label
colnames(rf_pred_prob_full)

# Replace 'positive_class' with the actual positive class label obtained from column names
positive_class <- "1"  # Example, replace with actual class name

# Predict probabilities for the positive class
rf_pred_prob <- rf_pred_prob_full[, positive_class]

# Ensure testData$V is a factor with levels corresponding to the positive and negative classes
# Replace "0" and "1" with actual class labels if different
testData$V <- factor(testData$V, levels = c("0", "1"))

# Calculate ROC curve
roc_curve <- roc(testData$V, rf_pred_prob)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for XGBoost Model")

# Print the AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

## Map Generation

# Train a Random Forest model
#rf_model_36_All_Final <- randomForest(V ~ ., data = Data_m, ntree = 1000, mtry = 6, importance = TRUE)

library(raster)
library(sp)
library(caret)  # Assuming you used caret for model training


Slop_New <- raster("D:/DEM/Slope_New.tif")
Elev_New <- raster("D:/DEM/New_DEM.tif")
Asp_New  <- raster("D:/DEM/Aspect_New.tif")
PL_New   <- raster("D:/DEM/Plan_New.tif")
PR_New   <- raster("D:/DEM/Profile_New.tif")
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

Landuse<- crop(Landuse, extent(Slop_SRTM))
Geology <- crop(Geology, extent(Slop_SRTM))
Imprevious <- crop(Imprevious, extent(Slop_SRTM))
LULC_chang <- crop(LULC_chang, extent(Slop_SRTM))
Rain <- crop(Rain, extent(Slop_SRTM))
NDVI <- crop(NDVI, extent(Slop_SRTM))
## ASTER
Slop_New <- crop(Slop_New, extent(Slop_SRTM))
Elev_New <- crop(Elev_New, extent(Slop_SRTM))
Asp_New  <- crop(Asp_New, extent(Slop_SRTM))
PL_New  <- crop(PL_New, extent(Slop_SRTM))
PR_New  <- crop(PR_New, extent(Slop_SRTM))
TRI_New  <- crop(TRI_New, extent(Slop_SRTM))
RO_New  <- crop(RO_New, extent(Slop_SRTM))
TPI_New <- crop(TPI_New, extent(Slop_SRTM))


# Train a Random Forest model (final model)

fit.xgb_New_Final<- train(V~., 
                          data=Data_m,
                          method = "xgbTree",
                          metric= "Accuracy",
                          preProc = c("center", "scale"), 
                          trControl = myControl,
                          tuneGrid = tune_grid,
                          tuneLength = 10)


# ## SRTM
# "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology"
#
writeRaster(Landuse,filename="D:/DEM/resampled_New/Landuse.tif", format="GTiff", overwrite=TRUE)
writeRaster(LULC_chang,filename="D:/DEM/resampled_New/LULC_chang.tif", format="GTiff", overwrite=TRUE)
writeRaster(Imprevious,filename="D:/DEM/resampled_New/Imprevious.tif", format="GTiff", overwrite=TRUE)
writeRaster(Rain,filename="D:/DEM/resampled_New/Rain.tif", format="GTiff", overwrite=TRUE)
writeRaster(NDVI,filename="D:/DEM/resampled_New/NDVI.tif", format="GTiff", overwrite=TRUE)
writeRaster(Geology,filename="D:/DEM/resampled_New/Geology.tif", format="GTiff", overwrite=TRUE)

##ASTER

writeRaster(Slop_New,filename="D:/DEM/resampled_New/Slop_New.tif", format="GTiff", overwrite=TRUE)
writeRaster(Elev_New,filename="D:/DEM/resampled_New/Elev_New.tif", format="GTiff", overwrite=TRUE)
writeRaster(Asp_New,filename="D:/DEM/resampled_New/Asp_New.tif", format="GTiff", overwrite=TRUE)
writeRaster(PL_New,filename="D:/DEM/resampled_New/PL_New.tif", format="GTiff", overwrite=TRUE)
writeRaster(PR_New,filename="D:/DEM/resampled_New/PR_New.tif", format="GTiff", overwrite=TRUE)
#writeRaster(TWI_New,filename="D:/DEM/resampled_New/TWI_New.tif", format="GTiff", overwrite=TRUE)
#writeRaster(SPI_New,filename="D:/DEM/resampled_New/SPI_New.tif", format="GTiff", overwrite=TRUE)
writeRaster(TRI_New,filename="D:/DEM/resampled_New/TRI_New.tif", format="GTiff", overwrite=TRUE)
writeRaster(RO_New,filename="D:/DEM/resampled_New/RO_New.tif", format="GTiff", overwrite=TRUE)
writeRaster(TPI_New,filename="D:/DEM/resampled_New/TPI_New.tif", format="GTiff", overwrite=TRUE)



#
## stack multiple raster files
Stack_List= list.files(path = "D:/DEM/resampled_New",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)
names(Rasters)
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)



# Now:Prediction using imported Rasters

# check the varaibles names to match with training data
#colnames(Rasters.df)[4] <- "ElEVATION"   # change columns names 
#colnames(Rasters.df)[6] <- "SlOPE"   # change columns names 

#head(Rasters.df[,c(-9,-10)],1)
#head(nn.ce$covariate,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y


##PRODUCE PROBABILITY MAP
p3<-as.data.frame(predict(fit.xgb_New_Final, Rasters.df_N, type = "prob"))
summary(p3)

names(p3)[1] <- "no"
names(p3)[2] <- "yes"
Rasters.df$Levels_yes<-p3$yes
Rasters.df$Levels_no<-p3$no
ww <- as.data.frame(x)
x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(Slop_SRTM))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(Slop_SRTM))

spplot(r_ave_yes, main="Landslides SM using XGB")
writeRaster(r_ave_yes,filename="D:/DEM/resampled_New/XGB_New_All.tif", format="GTiff", overwrite=TRUE) 


## PC

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

## XGBoost function ------------------------------------------------

#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)



tune_grid <- expand.grid(nrounds = 200,           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = 6,            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = 0.3,               # control the learning rate
                         gamma = 0,             # minimum loss reduction required
                         colsample_bytree = 1,  # subsample ratio of columns when constructing each tree
                         min_child_weight = 1,     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = 1)          # subsample ratio of the training instance


# Step 5 modeling
set.seed(849)
names(Data_m)
fit.xgb_PC<- train(V~., 
                   data=trainData,
                   method = "xgbTree",
                   metric= "Accuracy",
                   preProc = c("center", "scale"), 
                   trControl = myControl,
                   tuneGrid = tune_grid,
                   tuneLength = 10)
fit.xgb_PC$results

fit.xgb_PC$resample$Accuracy
X.xgb = varImp(fit.xgb_PC)
plot(X.xgb)
#Confusion Matrix - train data
p1<-predict(fit.xgb_PC, testData[,c(-6)], type = "raw")
confusionMatrix(p1, as.factor(testData$V))  # using more deep tree, the accuracy linearly increases! 

# # Make predictions on the test set
# rf_pred_prob <- predict(fit.xgb_SRTM , newdata = testData, type = "raw")[,2]
# rf_pred_class <- predict(fit.xgb_SRTM , newdata = testData, type = "response")

# # Evaluate the model
# confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
# print(confusion_matrix_rf)
library(raster)
library(sp)
library(caret) 
library(pROC)# Assuming you used caret for model training

# Ensure pROC package is loaded
library(pROC)

# Step 6: Calculate ROC AUC and plot ROC curve
# Predict probabilities for the positive class (assuming positive class is "1")
rf_pred_prob_full <- predict(fit.xgb_PC, newdata = testData[, -which(names(testData) == "V")], type = "prob")

# View the column names to identify the correct positive class label
colnames(rf_pred_prob_full)

# Replace 'positive_class' with the actual positive class label obtained from column names
positive_class <- "1"  # Example, replace with actual class name

# Predict probabilities for the positive class
rf_pred_prob <- rf_pred_prob_full[, positive_class]

# Ensure testData$V is a factor with levels corresponding to the positive and negative classes
# Replace "0" and "1" with actual class labels if different
testData$V <- factor(testData$V, levels = c("0", "1"))

# Calculate ROC curve
roc_curve <- roc(testData$V, rf_pred_prob)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for XGBoost Model")

# Print the AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

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
#TWI_PC1  <- raster("D:/DEM/New Folder/TWI_PC1.tif")
#SPI_PC1  <- raster("D:/DEM/New Folder/TWI_PC1.tif")
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

# Train a Random Forest model (final model)

fit.xgb_New_PC<- train(V~., 
                       data=Data_m,
                       method = "xgbTree",
                       metric= "Accuracy",
                       preProc = c("center", "scale"), 
                       trControl = myControl,
                       tuneGrid = tune_grid,
                       tuneLength = 10)


# ## SRTM
# "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology"
#
writeRaster(Landuse,filename="D:/DEM/resampled_PC1/Landuse.tif", format="GTiff", overwrite=TRUE)
writeRaster(LULC_chang,filename="D:/DEM/resampled_PC1/LULC_chang.tif", format="GTiff", overwrite=TRUE)
writeRaster(Imprevious,filename="D:/DEM/resampled_PC1/Imprevious.tif", format="GTiff", overwrite=TRUE)
writeRaster(Rain,filename="D:/DEM/resampled_PC1/Rain.tif", format="GTiff", overwrite=TRUE)
writeRaster(NDVI,filename="D:/DEM/resampled_PC1/NDVI.tif", format="GTiff", overwrite=TRUE)
writeRaster(Geology,filename="D:/DEM/resampled_PC1/Geology.tif", format="GTiff", overwrite=TRUE)

##ASTER

writeRaster(Slop_PC1,filename="D:/DEM/resampled_PC1/Slop_PC1.tif", format="GTiff", overwrite=TRUE)
writeRaster(Elev_PC1,filename="D:/DEM/resampled_PC1/Elev_PC1.tif", format="GTiff", overwrite=TRUE)
writeRaster(Asp_PC1,filename="D:/DEM/resampled_PC1/Asp_PC1.tif", format="GTiff", overwrite=TRUE)
writeRaster(PL_PC1,filename="D:/DEM/resampled_PC1/PL_PC1.tif", format="GTiff", overwrite=TRUE)
writeRaster(PR_PC1,filename="D:/DEM/resampled_PC1/PR_PC1.tif", format="GTiff", overwrite=TRUE)
#writeRaster(TWI_PC,filename="D:/DEM/resampled_PC/TWI_PC.tif", format="GTiff", overwrite=TRUE)
#writeRaster(SPI_PC,filename="D:/DEM/resampled_PC/SPI_PC.tif", format="GTiff", overwrite=TRUE)
writeRaster(TRI_PC1,filename="D:/DEM/resampled_PC1/TRI_PC1.tif", format="GTiff", overwrite=TRUE)
writeRaster(RO_PC1,filename="D:/DEM/resampled_PC1/RO_PC1.tif", format="GTiff", overwrite=TRUE)
writeRaster(TPI_PC1,filename="D:/DEM/resampled_PC1/TPI_PC1.tif", format="GTiff", overwrite=TRUE)



#
## stack multiple raster files
Stack_List= list.files(path = "D:/DEM/resampled_PC1",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)
names(Rasters)
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)



# Now:Prediction using imported Rasters

# check the varaibles names to match with training data
#colnames(Rasters.df)[4] <- "ElEVATION"   # change columns names 
#colnames(Rasters.df)[6] <- "SlOPE"   # change columns names 

#head(Rasters.df[,c(-9,-10)],1)
#head(nn.ce$covariate,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y


##PRODUCE PROBABILITY MAP
p3<-as.data.frame(predict(fit.xgb_New_PC, Rasters.df_N, type = "prob"))
summary(p3)

names(p3)[1] <- "no"
names(p3)[2] <- "yes"
Rasters.df$Levels_yes<-p3$yes
Rasters.df$Levels_no<-p3$no
ww <- as.data.frame(x)
x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(Slop_SRTM))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(Slop_SRTM))

spplot(r_ave_yes, main="Landslides SM using XGB")
writeRaster(r_ave_yes,filename="D:/DEM/resampled_PC1/XGB_PC_All.tif", format="GTiff", overwrite=TRUE) 

## PCA
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

## XGBoost function ------------------------------------------------

#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)



tune_grid <- expand.grid(nrounds = 200,           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = 6,            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = 0.3,               # control the learning rate
                         gamma = 0,             # minimum loss reduction required
                         colsample_bytree = 1,  # subsample ratio of columns when constructing each tree
                         min_child_weight = 1,     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = 1)          # subsample ratio of the training instance


# Step 5 modeling
set.seed(849)
names(Data_m)
fit.xgb_PCA<- train(V~., 
                    data=trainData,
                    method = "xgbTree",
                    metric= "Accuracy",
                    preProc = c("center", "scale"), 
                    trControl = myControl,
                    tuneGrid = tune_grid,
                    tuneLength = 10)
fit.xgb_PCA$results

fit.xgb_PCA$resample$Accuracy
X.xgb = varImp(fit.xgb_PCA)
plot(X.xgb)
#Confusion Matrix - train data
p1<-predict(fit.xgb_PCA, testData[,c(-6)], type = "raw")
confusionMatrix(p1, as.factor(testData$V))  # using more deep tree, the accuracy linearly increases! 

# # Make predictions on the test set
# rf_pred_prob <- predict(fit.xgb_SRTM , newdata = testData, type = "raw")[,2]
# rf_pred_class <- predict(fit.xgb_SRTM , newdata = testData, type = "response")

# # Evaluate the model
# confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
# print(confusion_matrix_rf)
library(raster)
library(sp)
library(caret) 
library(pROC)# Assuming you used caret for model training

# Ensure pROC package is loaded
library(pROC)

# Step 6: Calculate ROC AUC and plot ROC curve
# Predict probabilities for the positive class (assuming positive class is "1")
rf_pred_prob_full <- predict(fit.xgb_PCA, newdata = testData[, -which(names(testData) == "V")], type = "prob")

# View the column names to identify the correct positive class label
colnames(rf_pred_prob_full)

# Replace 'positive_class' with the actual positive class label obtained from column names
positive_class <- "1"  # Example, replace with actual class name

# Predict probabilities for the positive class
rf_pred_prob <- rf_pred_prob_full[, positive_class]

# Ensure testData$V is a factor with levels corresponding to the positive and negative classes
# Replace "0" and "1" with actual class labels if different
testData$V <- factor(testData$V, levels = c("0", "1"))

# Calculate ROC curve
roc_curve <- roc(testData$V, rf_pred_prob)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for XGBoost Model")

# Print the AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

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
# Train a Random Forest model (final model)

fit.xgb_New_PCA<- train(V~., 
                       data=Data_m,
                       method = "xgbTree",
                       metric= "Accuracy",
                       preProc = c("center", "scale"), 
                       trControl = myControl,
                       tuneGrid = tune_grid,
                       tuneLength = 10)


# ## SRTM
# "Landuse", "LULC_chang", "Imprevious", "Rain", "NDVI", "Geology"
#
writeRaster(Landuse,filename="D:/DEM/resampled_PCA/Landuse.tif", format="GTiff", overwrite=TRUE)
writeRaster(LULC_chang,filename="D:/DEM/resampled_PCA/LULC_chang.tif", format="GTiff", overwrite=TRUE)
writeRaster(Imprevious,filename="D:/DEM/resampled_PCA/Imprevious.tif", format="GTiff", overwrite=TRUE)
writeRaster(Rain,filename="D:/DEM/resampled_PCA/Rain.tif", format="GTiff", overwrite=TRUE)
writeRaster(NDVI,filename="D:/DEM/resampled_PCA/NDVI.tif", format="GTiff", overwrite=TRUE)
writeRaster(Geology,filename="D:/DEM/resampled_PCA/Geology.tif", format="GTiff", overwrite=TRUE)

##ASTER

writeRaster(Slop_PCA,filename="D:/DEM/resampled_PCA/Slop_PCA.tif", format="GTiff", overwrite=TRUE)
writeRaster(Elev_PCA,filename="D:/DEM/resampled_PCA/Elev_PCA.tif", format="GTiff", overwrite=TRUE)
writeRaster(Asp_PCA,filename="D:/DEM/resampled_PCA/Asp_PCA.tif", format="GTiff", overwrite=TRUE)
writeRaster(PL_PCA,filename="D:/DEM/resampled_PCA/PL_PCA.tif", format="GTiff", overwrite=TRUE)
writeRaster(PR_PCA,filename="D:/DEM/resampled_PCA/PR_PCA.tif", format="GTiff", overwrite=TRUE)
#writeRaster(TWI_PC,filename="D:/DEM/resampled_PC/TWI_PC.tif", format="GTiff", overwrite=TRUE)
#writeRaster(SPI_PC,filename="D:/DEM/resampled_PC/SPI_PC.tif", format="GTiff", overwrite=TRUE)
writeRaster(TRI_PCA,filename="D:/DEM/resampled_PCA/TRI_PCA.tif", format="GTiff", overwrite=TRUE)
writeRaster(RO_PCA,filename="D:/DEM/resampled_PCA/RO_PCA.tif", format="GTiff", overwrite=TRUE)
writeRaster(TPI_PCA,filename="D:/DEM/resampled_PCA/TPI_PCA.tif", format="GTiff", overwrite=TRUE)



#
## stack multiple raster files
Stack_List= list.files(path = "D:/DEM/resampled_PCA",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)
names(Rasters)
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)



# Now:Prediction using imported Rasters

# check the varaibles names to match with training data
#colnames(Rasters.df)[4] <- "ElEVATION"   # change columns names 
#colnames(Rasters.df)[6] <- "SlOPE"   # change columns names 

#head(Rasters.df[,c(-9,-10)],1)
#head(nn.ce$covariate,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y


##PRODUCE PROBABILITY MAP
p3<-as.data.frame(predict(fit.xgb_New_PCA, Rasters.df_N, type = "prob"))
summary(p3)

names(p3)[1] <- "no"
names(p3)[2] <- "yes"
Rasters.df$Levels_yes<-p3$yes
Rasters.df$Levels_no<-p3$no
ww <- as.data.frame(x)
x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(Slop_SRTM))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(Slop_SRTM))

spplot(r_ave_yes, main="Landslides SM using XGB")
writeRaster(r_ave_yes,filename="D:/DEM/resampled_PCA/XGB_PCA_All.tif", format="GTiff", overwrite=TRUE) 


