# Installing and loading Caret
install.packages("caret")
library(caret)

# Using the feature engineered dataset
# Selecting the whole dataset
df <- read.csv("data/cardio_model_ready.csv")

# Setting a seed so can be performed again
set.seed(1)

# Creating the partition
# 70% training, 30% testing
partition <- createDataPartition(df$cardio, p = 0.7, list=FALSE)

# Allocating training and testing data
trainingData <- df[partition, ]
testData <- df[-partition, ]

# Checking proportion of cardio data is roughly even
table(trainingData$cardio)
table(testData$cardio)   

# Saving for use in model
write.csv(trainingData, "data/trainingData.csv", row.names = FALSE)
write.csv(testData, "data/testData.csv", row.names = FALSE)

print("Data splitting completed. Datasets saved as testData.csv and trainingData.csv")
