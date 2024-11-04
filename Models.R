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
landslides <- landslides %>% select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                    TRI_ASTER, RO_ASTER, TPI_ASTER,
                                    Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, TWI_EDNA, SPI_EDNA, 
                                    TRI_EDNA, RO_EDNA, TPI_EDNA,
                                    Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, TWI_SRTM, SPI_SRTM, 
                                    TRI_SRTM, RO_SRTM, TPI_SRTM)
Non_landslides <- Non_landslides %>% select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                            TRI_ASTER, RO_ASTER, TPI_ASTER,
                                            Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, TWI_EDNA, SPI_EDNA, 
                                            TRI_EDNA, RO_EDNA, TPI_EDNA,
                                            Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, TWI_SRTM, SPI_SRTM, 
                                            TRI_SRTM, RO_SRTM, TPI_SRTM)

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

Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)


# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- as.data.frame(vif(logistic_model_1m))
names(vif_values)[1] <- "Factors"
names(vif_values)[2] <- "VIF"

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 10, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1

# Create a Predictor object for SHAP values
predictor <- Predictor$new(rf_model_1m, data = trainData, y = "V")

# Calculate SHAP values for a single instance (e.g., first row of testData)
shapley <- Shapley$new(predictor, x.interest = testData[1, ])
library(shapviz)
# Plot SHAP values for the instance
shapley$plot()
# SHAP summary plot
plot(shapley)



# Create a summary plot
shapley_values <- shapley$results
shap.summary_plot(shapley_values, trainData)

# Force plot for a single instance
shap.force_plot(shapley$expected_value, shapley_values, testData[1, ])

library(pdp)
# Plot partial dependence for a specific feature
#partialPlot(rf_model_1m , trainData, "feature_name")

# Perform PCA
# Perform PCA using prcomp
pca_result <- prcomp(trainData[, -which(names(trainData) == "V")], scale. = TRUE, center = TRUE)

# Summarize PCA results
summary(pca_result)
pca <- preProcess(trainData[, -which(names(trainData) == "V")], method = "pca", pcaComp = 10)
predict(pca, trainData[, -which(names(trainData) == "V")])
summary(pca)
pca_scores <- as.data.frame(pca$x)
head(pca_scores)

explained_variance <- summary(pca)$importance[2,]
explained_variance

# Train Random Forest with PCA components
rf_model_1m <- randomForest(V ~ ., data = cbind(trainData, V = trainData$V))

# Assuming you have a testData dataset
predictions <- predict(rf_model_1m, newdata = testData)
# Load necessary libraries
library(caret)

# Assuming your actual test labels are in a column called 'V'
actual_labels <- testData$V

# Calculate accuracy
accuracy <- sum(predictions == actual_labels) / length(actual_labels)
print(paste("Accuracy:", accuracy))

# Confusion matrix
confusionMatrix(predictions, actual_labels)

# Extract the first 10 principal components
pca_scores <- as.data.frame(pca_result$x[, 1:10])
pca_scores$V <- trainData$V

# Train a Random Forest model using the principal components
rf_model <- randomForest(V ~ ., data = pca_scores, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model)

# Predict principal components for the test set
test_pca_scores <- as.data.frame(predict(pca_result, newdata = testData[, -which(names(testData) == "V")]))
test_pca_scores$V <- testData$V

# Make predictions on the test set
rf_pred_prob <- predict(rf_model, newdata = test_pca_scores, type = "prob")[,2]
rf_pred_class <- predict(rf_model, newdata = test_pca_scores, type = "response")

# Evaluate the model
confusion_matrix_rf <- confusionMatrix(as.factor(rf_pred_class), as.factor(testData$V))
print(confusion_matrix_rf)

# Calculate ROC curve and AUC
roc_curve_rf <- roc(testData$V, rf_pred_prob)
auc_value_rf <- auc(roc_curve_rf)
print(paste("AUC for Random Forest:", auc_value_rf))

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
importance_rf <- importance(rf_model)
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()

## ASTER

Data_m_ASTER <- Data_m%>%select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                          TRI_ASTER, RO_ASTER, TPI_ASTER)

# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m_ASTER$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m_ASTER[trainIndex, ]
testData <- Data_m_ASTER[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


##SRTM
Data_m_SRTM <- Data_m%>%select(Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, V, TWI_SRTM, SPI_SRTM, 
                                TRI_SRTM, RO_SRTM, TPI_SRTM)

# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m_SRTM$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m_SRTM[trainIndex, ]
testData <- Data_m_SRTM[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## EDNA



Data_m_EDNA <- Data_m%>%select(Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, V, TWI_EDNA, SPI_EDNA, 
                               TRI_EDNA, RO_EDNA, TPI_EDNA)

# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m_EDNA$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m_EDNA[trainIndex, ]
testData <- Data_m_EDNA[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## Combined

Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
landslides <- landslides %>% select(Slop_Comb, Elev_Comb, Asp_Comb, PL_Comb, PR_Comb, V, TWI_Comb, SPI_Comb, 
                                    TRI_Comb, RO_Comb, TPI_Comb)
Non_landslides <- Non_landslides %>% select(Slop_Comb, Elev_Comb, Asp_Comb, PL_Comb, PR_Comb, V, TWI_Comb, SPI_Comb, 
                                            TRI_Comb, RO_Comb, TPI_Comb)

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

Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## DEM
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
landslides <- landslides %>% select(Slop_DEM, Elev_DEM, Asp_DEM, PL_DEM, PR_DEM, V, TWI_DEM, SPI_DEM, 
                                    TRI_DEM, RO_DEM, TPI_DEM)
Non_landslides <- Non_landslides %>% select(Slop_DEM, Elev_DEM, Asp_DEM, PL_DEM, PR_DEM, V, TWI_DEM, SPI_DEM, 
                                            TRI_DEM, RO_DEM, TPI_DEM)

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

Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## Final

Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
landslides <- landslides %>% select(Slop_Final, Elev_Final, Asp_Final, PL_Final, PR_Final, V, TWI_Final, SPI_Final, 
                                    TRI_Final, RO_Final, TPI_Final)
Non_landslides <- Non_landslides %>% select(Slop_Final, Elev_Final, Asp_Final, PL_Final, PR_Final, V, TWI_Final, SPI_Final, 
                                            TRI_Final, RO_Final, TPI_Final)

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

Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## PCA

library("ggplot2")
library("ggfortify")
library("gridExtra")
library("carData")
library("car")
library("factoextra")


# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)

library(tidyverse)
library(dplyr)
landslides <- landslides %>% select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                    TRI_ASTER, RO_ASTER, TPI_ASTER,
                                    Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, TWI_EDNA, SPI_EDNA, 
                                    TRI_EDNA, RO_EDNA, TPI_EDNA,
                                    Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, TWI_SRTM, SPI_SRTM, 
                                    TRI_SRTM, RO_SRTM, TPI_SRTM)
Non_landslides <- Non_landslides %>% select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                            TRI_ASTER, RO_ASTER, TPI_ASTER,
                                            Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, TWI_EDNA, SPI_EDNA, 
                                            TRI_EDNA, RO_EDNA, TPI_EDNA,
                                            Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, TWI_SRTM, SPI_SRTM, 
                                            TRI_SRTM, RO_SRTM, TPI_SRTM)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))

Data_m.active <- Data_m[1:5, 7:31]
res.pca <- prcomp(Data_m.active, scale = TRUE)
fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

print(res.pca)
summary(res.pca)

eig.val<-get_eigenvalue(res.pca)
eig.val

var <- get_pca_var(res.pca)
var

head(var$cos2)

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

kmeans<-eclust(Data_m.active, k=4)

autoplot(res.pca, data=kmeans, colour="cluster")

Data_m$PC1 <- res.pca$x[, 1]
Data_m$PC2 <- res.pca$x[, 2]
Data_m$PC3 <- res.pca$x[, 3]
Data_m$PC4 <- res.pca$x[, 4]
Data_m$PC5 <- res.pca$x[, 5]


Data_m_PC <- Data_m%>%select(V, PC1, PC2, PC3, PC4, PC5 )

Data_m_PC$V <- as.factor(Data_m_PC$V)


## Logistic Regression 


# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m_PC$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m_PC [trainIndex, ]
testData <- Data_m_PC [-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1



## New (Weight Based on Accuracy)
## ASTER





Data_m_ASTER <- Data_m%>%select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                TRI_ASTER, RO_ASTER, TPI_ASTER)

# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m_ASTER$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m_ASTER[trainIndex, ]
testData <- Data_m_ASTER[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1



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
landslides <- landslides %>% select (Slop_New, Elev_New, Asp_New, PL_New, PR_New, V, TWI_New, SPI_New, 
                                    TRI_New, RO_New, TPI_New)
                                  
Non_landslides <- Non_landslides %>% select (Slop_New, Elev_New, Asp_New, PL_New, PR_New, V, TWI_New, SPI_New, 
                                            TRI_New, RO_New, TPI_New)

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

Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## PCA

# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)

library(tidyverse)
library(dplyr)
landslides <- landslides %>% select (Slop_PCA, Elev_PCA, Asp_PCA, PL_PCA, PR_PCA, V, TWI_PCA, SPI_PCA, 
                                     TRI_PCA, RO_PCA, TPI_PCA)

Non_landslides <- Non_landslides %>% select (Slop_PCA, Elev_PCA, Asp_PCA, PL_PCA, PR_PCA, V, TWI_PCA, SPI_PCA, 
                                             TRI_PCA, RO_PCA, TPI_PCA)

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

Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1



## MLE

# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)

library(tidyverse)
library(dplyr)
landslides <- landslides %>% select (Slop_MLE, Elev_MLE, Asp_MLE, PL_MLE, PR_MLE, V, TWI_MLE, SPI_MLE, 
                                     TRI_MLE, RO_MLE, TPI_MLE)

Non_landslides <- Non_landslides %>% select (Slop_MLE, Elev_MLE, Asp_MLE, PL_MLE, PR_MLE, V, TWI_MLE, SPI_MLE, 
                                             TRI_MLE, RO_MLE, TPI_MLE)

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

Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1

## VIF BAsed Model

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
landslides <- landslides %>% select(Slop_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                   
                                   Asp_EDNA, PL_EDNA, PR_EDNA, TWI_EDNA, TPI_EDNA,
                                   Asp_SRTM, PL_SRTM, PR_SRTM, TWI_SRTM, SPI_SRTM, 
                                     RO_SRTM)
Non_landslides <- Non_landslides %>% select(Slop_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                            
                                            Asp_EDNA, PL_EDNA, PR_EDNA, TWI_EDNA, TPI_EDNA,
                                            Asp_SRTM, PL_SRTM, PR_SRTM, TWI_SRTM, SPI_SRTM, 
                                            RO_SRTM)

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

Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)


# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- as.data.frame(vif(logistic_model_1m))
names(vif_values)[1] <- "Factors"
names(vif_values)[2] <- "VIF"

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 10, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1

## PCA New Way 

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
landslides <- landslides %>% select(Slop_PC1,Slop_PC2,  Elev_PC1, Asp_PC1,Asp_PC2,  PL_PC1,PL_PC2,  PR_PC1,PR_PC2,
                                    V, TWI_PC1,TWI_PC2,  SPI_PC1,SPI_PC2,  
                                    TRI_PC1, RO_PC1, TPI_PC1,TPI_PC2
                                    )
Non_landslides <- Non_landslides %>% select(Slop_PC1,Slop_PC2,  Elev_PC1, Asp_PC1,Asp_PC2,  PL_PC1,PL_PC2,  PR_PC1,PR_PC2,
                                            V, TWI_PC1,TWI_PC2,  SPI_PC1,SPI_PC2,  
                                            TRI_PC1, RO_PC1, TPI_PC1,TPI_PC2)

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

Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)


# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- as.data.frame(vif(logistic_model_1m))
names(vif_values)[1] <- "Factors"
names(vif_values)[2] <- "VIF"

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 10, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1
library(pdp)


## PC Based 10 Factors

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
landslides <- landslides %>% select(Slop_PC1,  Elev_PC1, Asp_PC1,  PL_PC1,  PR_PC1,
                                    V, TWI_PC1,  SPI_PC1,  
                                    TRI_PC1, RO_PC1, TPI_PC1
)
Non_landslides <- Non_landslides %>% select(Slop_PC1,  Elev_PC1, Asp_PC1,  PL_PC1,  PR_PC1,
                                            V, TWI_PC1,  SPI_PC1,  
                                            TRI_PC1, RO_PC1, TPI_PC1)

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

Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)


# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- as.data.frame(vif(logistic_model_1m))
names(vif_values)[1] <- "Factors"
names(vif_values)[2] <- "VIF"

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 10, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1
library(pdp)


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
landslides <- landslides %>% select(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, V)
Non_landslides <- Non_landslides %>% select(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, V)

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

Data_m <- Data_m %>% mutate(across(.cols = -V, .fns = scale_to_z))
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)


# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- as.data.frame(vif(logistic_model_1m))
names(vif_values)[1] <- "Factors"
names(vif_values)[2] <- "VIF"

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 10, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1

#############################################################################################################
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
landslides <- landslides %>% select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                    TRI_ASTER, RO_ASTER, TPI_ASTER,
                                    Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, TWI_EDNA, SPI_EDNA, 
                                    TRI_EDNA, RO_EDNA, TPI_EDNA,
                                    Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, TWI_SRTM, SPI_SRTM, 
                                    TRI_SRTM, RO_SRTM, TPI_SRTM,
                                    Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)
Non_landslides <- Non_landslides %>% select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                            TRI_ASTER, RO_ASTER, TPI_ASTER,
                                            Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, TWI_EDNA, SPI_EDNA, 
                                            TRI_EDNA, RO_EDNA, TPI_EDNA,
                                            Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, TWI_SRTM, SPI_SRTM, 
                                            TRI_SRTM, RO_SRTM, TPI_SRTM,
                                            Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

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
  mutate(across(.cols = -c(V, Soil       ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
Data_m$Soil       <- as.factor(Data_m$Soil        )
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)


# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- as.data.frame(vif(logistic_model_1m))
names(vif_values)[1] <- "Factors"
names(vif_values)[2] <- "VIF"

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 10, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1



## ASTER with All Factors

Data_m_ASTER <- Data_m%>%select(Slop_ASTER, Elev_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                TRI_ASTER, RO_ASTER, TPI_ASTER, Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m_ASTER$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m_ASTER[trainIndex, ]
testData <- Data_m_ASTER[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## SRTM All Factors


Data_m_SRTM <- Data_m%>%select(Slop_SRTM, Elev_SRTM, Asp_SRTM, PL_SRTM, PR_SRTM, V, TWI_SRTM, SPI_SRTM, 
                               TRI_SRTM, RO_SRTM, TPI_SRTM, 
                               Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m_SRTM$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m_SRTM[trainIndex, ]
testData <- Data_m_SRTM[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation


library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## EDNA with All Factors

## EDNA

glimpse(Data_m)

Data_m_EDNA <- Data_m%>%select(Slop_EDNA, Elev_EDNA, Asp_EDNA, PL_EDNA, PR_EDNA, V, TWI_EDNA, SPI_EDNA, 
                               TRI_EDNA, RO_EDNA, TPI_EDNA,
                               Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

# Load necessary libraries
library(caret)
library(dplyr)

# Set a seed for reproducibility
set.seed(123)

# Perform stratified sampling
trainIndex <- createDataPartition(Data_m_EDNA$V, p = 0.8, list = FALSE)

# Split the data into training and test sets
trainData <- Data_m_EDNA[trainIndex, ]
testData <- Data_m_EDNA[-trainIndex, ]

# Load necessary libraries
library(pROC)
library(ggplot2)

library(gt)
library(caTools)
library(car)

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## Combined

Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
landslides <- landslides %>% select(Slop_Comb, Elev_Comb, Asp_Comb, PL_Comb, PR_Comb, V, TWI_Comb, SPI_Comb, 
                                    TRI_Comb, RO_Comb, TPI_Comb,
                                    Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)
Non_landslides <- Non_landslides %>% select(Slop_Comb, Elev_Comb, Asp_Comb, PL_Comb, PR_Comb, V, TWI_Comb, SPI_Comb, 
                                            TRI_Comb, RO_Comb, TPI_Comb,
                                            Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

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

# Function to scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

Data_m <- Data_m %>%
  mutate(across(.cols = -c(V, Soil       ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
Data_m$Soil       <- as.factor(Data_m$Soil        )
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

#write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## DEM
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
landslides <- landslides %>% select(Slop_DEM, Elev_DEM, Asp_DEM, PL_DEM, PR_DEM, V, TWI_DEM, SPI_DEM, 
                                    TRI_DEM, RO_DEM, TPI_DEM,
                                    Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)
Non_landslides <- Non_landslides %>% select(Slop_DEM, Elev_DEM, Asp_DEM, PL_DEM, PR_DEM, V, TWI_DEM, SPI_DEM, 
                                            TRI_DEM, RO_DEM, TPI_DEM,
                                            Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

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

# Function to scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

Data_m <- Data_m %>%
  mutate(across(.cols = -c(V, Soil       ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
Data_m$Soil       <- as.factor(Data_m$Soil        )
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

##write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## Final

Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)
landslides <- landslides %>% select(Slop_Final, Elev_Final, Asp_Final, PL_Final, PR_Final, V, TWI_Final, SPI_Final, 
                                    TRI_Final, RO_Final, TPI_Final,
                                    Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)
Non_landslides <- Non_landslides %>% select(Slop_Final, Elev_Final, Asp_Final, PL_Final, PR_Final, V, TWI_Final, SPI_Final, 
                                            TRI_Final, RO_Final, TPI_Final,
                                            Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

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

# Function to scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

Data_m <- Data_m %>%
  mutate(across(.cols = -c(V, Soil       ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
Data_m$Soil       <- as.factor(Data_m$Soil        )
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

#write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1

## Weight Based on Accuracy with All


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
landslides <- landslides %>% select (Slop_New, Elev_New, Asp_New, PL_New, PR_New, V, TWI_New, SPI_New, 
                                     TRI_New, RO_New, TPI_New, 
                                     Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

Non_landslides <- Non_landslides %>% select (Slop_New, Elev_New, Asp_New, PL_New, PR_New, V, TWI_New, SPI_New, 
                                             TRI_New, RO_New, TPI_New,
                                             Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))

# Scale variables to z-scores
Data_m <- Data_m %>%
  mutate(across(.cols = -c(V, Soil       ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
Data_m$Soil       <- as.factor(Data_m$Soil        )
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1


## PCA

# Upload the Shape Files
library(sf)
Landslides_Points <- st_read("Landslides.shp")
Non_Landslides_Points <- st_read("Non_landslides.shp")

# Convert to Data Frames
landslides <- as.data.frame(Landslides_Points)
Non_landslides <- as.data.frame(Non_Landslides_Points)

library(tidyverse)
library(dplyr)
landslides <- landslides %>% select (Slop_PCA, Elev_PCA, Asp_PCA, PL_PCA, PR_PCA, V, TWI_PCA, SPI_PCA, 
                                     TRI_PCA, RO_PCA, TPI_PCA,
                                     Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

Non_landslides <- Non_landslides %>% select (Slop_PCA, Elev_PCA, Asp_PCA, PL_PCA, PR_PCA, V, TWI_PCA, SPI_PCA, 
                                             TRI_PCA, RO_PCA, TPI_PCA,
                                             Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))

# Function to scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

Data_m <- Data_m %>%
  mutate(across(.cols = -c(V, Soil       ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
Data_m$Soil       <- as.factor(Data_m$Soil        )
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)

# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- vif(logistic_model_1m) 
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 5, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1



## VIF BAsed Model and Others

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
landslides <- landslides %>% select(Slop_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                    
                                    Asp_EDNA, PL_EDNA, PR_EDNA, TWI_EDNA, TPI_EDNA,
                                    Asp_SRTM, PL_SRTM, PR_SRTM, TWI_SRTM, SPI_SRTM, 
                                    RO_SRTM,
                                    Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)
Non_landslides <- Non_landslides %>% select(Slop_ASTER, Asp_ASTER, PL_ASTER, PR_ASTER, V, TWI_ASTER, SPI_ASTER, 
                                            
                                            Asp_EDNA, PL_EDNA, PR_EDNA, TWI_EDNA, TPI_EDNA,
                                            Asp_SRTM, PL_SRTM, PR_SRTM, TWI_SRTM, SPI_SRTM, 
                                            RO_SRTM,
                                            Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))

# Function to scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

Data_m <- Data_m %>%
  mutate(across(.cols = -c(V, Soil       ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
Data_m$Soil       <- as.factor(Data_m$Soil        )
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)


# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- as.data.frame(vif(logistic_model_1m))
names(vif_values)[1] <- "Factors"
names(vif_values)[2] <- "VIF"

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 10, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1

## PCA New Way 

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
landslides <- landslides %>% select(Slop_PC1,Slop_PC2,  Elev_PC1, Asp_PC1,Asp_PC2,  PL_PC1,PL_PC2,  PR_PC1,PR_PC2,
                                    V, TWI_PC1,TWI_PC2,  SPI_PC1,SPI_PC2,  
                                    TRI_PC1, RO_PC1, TPI_PC1,TPI_PC2,Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse 
)
Non_landslides <- Non_landslides %>% select(Slop_PC1,Slop_PC2,  Elev_PC1, Asp_PC1,Asp_PC2,  PL_PC1,PL_PC2,  PR_PC1,PR_PC2,
                                            V, TWI_PC1,TWI_PC2,  SPI_PC1,SPI_PC2,  
                                            TRI_PC1, RO_PC1, TPI_PC1,TPI_PC2,
                                            Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))

# Function to scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

Data_m <- Data_m %>%
  mutate(across(.cols = -c(V, Soil       ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
Data_m$Soil       <- as.factor(Data_m$Soil        )
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)


# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- as.data.frame(vif(logistic_model_1m))
names(vif_values)[1] <- "Factors"
names(vif_values)[2] <- "VIF"

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 10, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1
library(pdp)


## PC Based 10 Factors

library(sf)
library(tidyverse)
library(randomForest)

library(caret)
library(pROC)
library(e1071)

##
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
                                    V, TWI_PC1,  SPI_PC1,  
                                    TRI_PC1, RO_PC1, TPI_PC1, Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse
)
Non_landslides <- Non_landslides %>% select(Slop_PC1,  Elev_PC1, Asp_PC1,  PL_PC1,  PR_PC1,
                                            V, TWI_PC1,  SPI_PC1,  
                                            TRI_PC1, RO_PC1, TPI_PC1,
                                            Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))

# Function to scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

Data_m <- Data_m %>%
  mutate(across(.cols = -c(V, Soil       ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
Data_m$Soil       <- as.factor(Data_m$Soil        )
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)


# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- as.data.frame(vif(logistic_model_1m))
names(vif_values)[1] <- "Factors"
names(vif_values)[2] <- "VIF"

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 10, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1
library(pdp)


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
landslides <- landslides %>% select(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, V,
                                    Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse
                                    
)
Non_landslides <- Non_landslides %>% select(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, V,
                                            Imprevious , LULC_chang, Geology, Rain, Distance_R, NDVI, Soil, Landuse
                                            
)

# # Rename columns for consistency
# landslides <- rename(landslides, Slope = Slope_12, PL = Plan, PR = Pr)
# Non_landslides <- rename(Non_landslides, Slope = Slope_12, PL = Plan, PR = Pr, V = CID)

landslides$V <- as.character(landslides$V)
Non_landslides$V <- as.character(Non_landslides$V)
Data <- rbind(landslides, Non_landslides)
Data_m <- Data

Data_m <- Data_m %>% filter(across(-V, ~ . != -9999))

# Function to scale variables to z-scores
scale_to_z <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

Data_m <- Data_m %>%
  mutate(across(.cols = -c(V, Soil       ,  LULC_chang, Geology, Landuse ), .fns = scale_to_z))
Data_m$Soil       <- as.factor(Data_m$Soil        )
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

# Train a logistic regression model
logistic_model_1m <- glm(V ~ ., data = trainData, family = binomial)


# Summarize the model
summary(logistic_model_1m)
exp(coef(logistic_model_1m))
#kable(l$coefficients, caption="g")

vif(logistic_model_1m)
vif_values <- as.data.frame(vif(logistic_model_1m))
names(vif_values)[1] <- "Factors"
names(vif_values)[2] <- "VIF"

barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 10, lwd = 3, lty = 2) 
#add vertical line at 5 as after 5 there is severe correlation
cor(Data_m)

library(dplyr)
library(stargazer)
library(caret)

pred_prob_train <- predict(logistic_model_1m , newdata = trainData, type = "response")
pred_class_train <- ifelse(pred_prob_train > 0.5, 1, 0)

# Evaluate the model on training set
confusion_matrix_train <- table(trainData$V, pred_class_train)

##knitr::kable(confusion_matrix_train)
accuracy_train <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)
print(confusion_matrix_train)
print(paste("Training Accuracy:", accuracy_train))
#kable(t)
# Make predictions on the test set
pred_prob_test <- predict(logistic_model_1m, newdata = testData, type = "response")
pred_class_test <- ifelse(pred_prob_test > 0.5, 1, 0)

# Evaluate the model on test set
confusion_matrix_test <- table(testData$V, pred_class_test)
accuracy_test <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)
print(confusion_matrix_test)
print(paste("Test Accuracy:", accuracy_test))
accuracy_test_1m <- as.data.frame(accuracy_test)

# Calculate ROC curve and AUC
roc_curve <- roc(testData$V, pred_prob_test)
auc_value <- auc(roc_curve)
auc_1m <- auc_value 
print(paste("AUC:", auc_value))

# Plot ROC curve using ggplot2
roc_data <- data.frame(
  specificity = rev(roc_curve$specificities),
  sensitivity = rev(roc_curve$sensitivities)
)

ggplot(roc_data, aes(x = specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ROC Curve for Logistic Regression_1m", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

write.csv(Data_m, "Data_1m.csv", row.names = FALSE)



# Train a Random Forest model
rf_model_1m <- randomForest(V ~ ., data = trainData, ntree = 500, mtry = 6, importance = TRUE)

# Summarize the model
print(rf_model_1m)

# Make predictions on the test set
rf_pred_prob <- predict(rf_model_1m, newdata = testData, type = "prob")[,2]
rf_pred_class <- predict(rf_model_1m, newdata = testData, type = "response")

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
importance_rf <- importance(rf_model_1m )
importance_df <- data.frame(Variable = rownames(importance_rf), 
                            MeanDecreaseGini = importance_rf[, "MeanDecreaseGini"])

# Plot variable importance using ggplot2
p1 <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Variable Importance Plot_1m", x = "Variable", y = "Mean Decrease in Gini") +
  theme_minimal()
p1
