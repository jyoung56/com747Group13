# Install and load required libraries
packages <- c("tidyverse", "caret", "e1071")
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

# Train Support Vector Machine (SVM) model
svm_model <- svm(cardio ~ ., data = trainData, kernel = "radial", cost = 1, gamma = 0.1)

# Make Predictions
svm_predictions <- predict(svm_model, testData)

# Evaluate Model
svm_conf_matrix <- confusionMatrix(svm_predictions, testData$cardio)

# Print Results
print(svm_conf_matrix)

# Save the model
saveRDS(svm_model, "cardio_svm_model.rds")

cat("SVM model training completed. Model saved as cardio_svm_model.rds")
