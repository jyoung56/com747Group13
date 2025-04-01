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
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(df$cardio, p = 0.8, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Train a Random Forest model
model <- randomForest(cardio ~ ., data = trainData, ntree = 100, importance = TRUE)

# Model Evaluation on Test Set
predictions <- predict(model, testData)
conf_matrix <- confusionMatrix(predictions, testData$cardio)

# Print Evaluation Metrics
print(conf_matrix)

# Save the trained model
saveRDS(model, "cardio_rf_model.rds")

cat("Model training completed. Model saved as cardio_rf_model.rds")
