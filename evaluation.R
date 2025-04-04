# ------------------------------------------------------------------
# Cardiovascular Disease Model Evaluation Script
# Author : Hephzi
# Editor : Arslan
# Description: Load trained models, make predictions, evaluate results
# ------------------------------------------------------------------

# Install and load required libraries
packages <- c("tidyverse", "caret", "randomForest", "e1071")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))

# Load the feature-engineered dataset
df <- read.csv("cardio_feature_engineered.csv")

# Split dataset into training (80%) and testing (20%) sets
set.seed(123)
trainIndex <- createDataPartition(df$cardio, p = 0.8, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Load trained models
rf_model <- readRDS("cardio_rf_model.rds")
log_model <- readRDS("cardio_logistic_model.rds")
svm_model <- readRDS("cardio_svm_model.rds")
knn_model <- readRDS("cardio_knn_model.rds")

# Make Predictions
rf_predictions <- predict(rf_model, testData)
log_predictions <- predict(log_model, testData, type = "response")
log_predicted_classes <- ifelse(log_predictions > 0.5, 1, 0)
svm_predictions <- predict(svm_model, testData)
knn_predictions <- predict(knn_model, testData)

# Evaluate Models
rf_conf_matrix <- confusionMatrix(rf_predictions, testData$cardio)
log_conf_matrix <- confusionMatrix(as.factor(log_predicted_classes), testData$cardio)
svm_conf_matrix <- confusionMatrix(svm_predictions, testData$cardio)
knn_conf_matrix <- confusionMatrix(knn_predictions, testData$cardio)

# Print Accuracy Scores
print("Random Forest Accuracy:")
print(rf_conf_matrix$overall['Accuracy'])

print("Logistic Regression Accuracy:")
print(log_conf_matrix$overall['Accuracy'])

print("SVM Accuracy:")
print(svm_conf_matrix$overall['Accuracy'])

print("knn Accuracy:")
print(knn_conf_matrix$overall['Accuracy'])

# Print Precision, Recall, and F1-Score
print("Random Forest Metrics:")
print(rf_conf_matrix$byClass)

print("Logistic Regression Metrics:")
print(log_conf_matrix$byClass)

print("SVM Metrics:")
print(svm_conf_matrix$byClass)

print("KNN Metrics:")
print(knn_conf_matrix$byClass)

evaluate_model <- function(conf) {
  acc <- conf$overall["Accuracy"]
  sens <- conf$byClass["Sensitivity"]
  spec <- conf$byClass["Specificity"]
  f1 <- conf$byClass["F1"]
  return(c(Accuracy = acc, Sensitivity = sens, Specificity = spec, F1_Score = f1))
}

results_table <- data.frame(
  t(
    rbind(
      Random_Forest = evaluate_model(rf_conf_matrix),
      Logistic_Regression = evaluate_model(log_conf_matrix),
      SVM = evaluate_model(svm_conf_matrix),
      KNN = evaluate_model(knn_conf_matrix)
    )
  )
)

print("------ Overall Evaluation Summary ------")
print(round(results_table, 4))
