# Install and load required libraries
packages <- c("tidyverse", "caret")
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

# Train Logistic Regression model
log_model <- glm(cardio ~ ., data = trainData, family = binomial)

# Make Predictions
log_predictions <- predict(log_model, testData, type = "response")
log_predicted_classes <- ifelse(log_predictions > 0.5, 1, 0)

# Evaluate Model
log_conf_matrix <- confusionMatrix(as.factor(log_predicted_classes), testData$cardio)

# Print Results
print(log_conf_matrix)

# Save the model
saveRDS(log_model, "cardio_logistic_model.rds")

cat("Logistic Regression model training completed. Model saved as cardio_logistic_model.rds")
