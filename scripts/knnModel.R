
# Here we train a K-Nearest Neighbors (KNN) classification model
# to predict whether a patient has cardiovascular disease (Yes/No)
# using the cleaned and preprocessed dataset.
# -------------------------------------------------------------

# Load the necessary libraries for training and evaluating ML models. Also used dplyr data manipulation
library(caret)
library(dplyr)   

# Step 1: Load the dataset that is already preprocessed and model-ready
df <- read.csv("data/cardio_model_ready.csv")

# Step 2: Ensure the target variable (cardio) is a factor
df$cardio <- as.factor(df$cardio)

# Step 3: Split the dataset into training (70%) and testing (30%) sets
set.seed(123)  # setting seed for reproducibility
partition <- createDataPartition(df$cardio, p = 0.7, list = FALSE)
trainData <- df[partition, ]
testData <- df[-partition, ]

# Step 4: Set up training control using 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Step 5: Train the KNN model
# Preprocessing includes centering and scaling the numeric features
# tuneLength = 10 means it will test 10 different 'k' values automatically
knn_model <- train(cardio ~ ., data = trainData, method = "knn", trControl = ctrl, preProcess = c("center", "scale"), tuneLength = 10)

# Step 6: Output the trained model details including best 'k'
print(knn_model)

# Step 7: Make predictions on the test set
knn_preds <- predict(knn_model, newdata = testData)

# Step 8: Evaluate the model using a confusion matrix
# It will show accuracy, sensitivity, specificity, etc.
confusion <- confusionMatrix(knn_preds, testData$cardio)
print(confusion)
