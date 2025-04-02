# IMPORTING AND LOADING LIBRARIES
packages <- c("tidyverse", "ggplot2", "dplyr", "DataExplorer", "caret", "corrplot")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))



# SECTION ONE: Loading the dataset

# Load the dataset  (make sure cardio_train.csv is in your working directory)
data <- read.csv("data/cardio_train.csv", sep = ";")

head(data)
str(data)
summary(data)
print("Data Loading Completed.")



# SECTION TWO: Data Cleansing

# Load the dataset (make sure cardio_train.csv is in your working directory)
df <- read.csv("data/cardio_train.csv", sep = ";")


# View basic information about the dataset
print("Structure of dataset:")
str(df)

print("Summary statistics:")
summary(df)

print("Missing values per column:")
print(colSums(is.na(df)))

print("Column names:")
print(names(df))

print("First few rows of dataset:")
head(df)

# Basic Exploratory Data Analysis, more in eda.R
# Distribution of target variable (cardio)
ggplot(df, aes(x = factor(cardio))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Cardiovascular Disease", x = "Cardio (0=No, 1=Yes)", y = "Count")

# Correlation matrix plot
# Notable correlations: 
#   gender&height, height&weight, gender&smoke, chol&gluc, age&cardio, weight&cardio, chol&cardio
numeric_df <- df[sapply(df, is.numeric)]
cor_matrix <- cor(numeric_df)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# Data Cleaning
# Removing outliers for height, weight, and blood pressure
df <- df %>% filter(height > 100 & height < 250)
df <- df %>% filter(weight > 30 & weight < 200)
df <- df %>% filter(ap_hi > 80 & ap_hi < 250)
df <- df %>% filter(ap_lo > 40 & ap_lo < 200)

# Remove duplicate rows
df <- df[!duplicated(df), ]

# Add new column for age in years (Dataset uses days) and round down to the nearest whole number
df <- df%>%
  mutate(age_years = floor(age / 365))

# Feature Engineering
# Create BMI variable
df$bmi <- df$weight / ((df$height / 100)^2)

df <- df %>%
  mutate(bmi = weight / ((height / 100) ^ 2))  # height is in cm

# Convert categorical variables to factors
df$gender <- as.factor(df$gender)
df$cholesterol <- as.factor(df$cholesterol)
df$gluc <- as.factor(df$gluc)
df$smoke <- as.factor(df$smoke)
df$alco <- as.factor(df$alco)
df$active <- as.factor(df$active)
df$cardio <- as.factor(df$cardio)

# Save the cleaned dataset
write.csv(df, "data/cardio_cleaned.csv", row.names = FALSE)

print("Data exploration, cleaning, and preprocessing completed. Cleaned dataset saved as cardio_cleaned.csv")




# SECTION THREE: Exploratory Data Analysis - THIS SHOULD MAYBE BE THREE
# I need to sort this out a little bit, not sure if should use data or df here
# Maybe should rename variables to reflect status, like initial, cleaned, model_ready
# Should be saving plots, not just displaying them
#library(scales)

# Add new column for age in years (Dataset uses days)
data <- data%>%
  mutate(age_years = age / 365)

# Age Distribution
gg_age <- ggplot(df, aes(x = age_years)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  xlab("Age (Years)") +
  ggtitle("Age Distribution of Patients")
gg_age

# Cholesterol Levels (categorical)
gg_chol <- ggplot(df, aes(x = cholesterol, fill = cholesterol)) +
  geom_bar() +
  xlab("Cholesterol Level") + 
  ggtitle("Cholesterol Levels in Dataset") +
  theme(legend.position = "none")
gg_chol


# Comparing Gender Distribution
# Important for ethical considerations, need to consider how this data effects the results and if it's a fair distribution
# Since there's a far greater number (roughly 65%)of female patients we need to consider why in the report
ggplot(data, aes(x=factor(gender))) +
  geom_bar(fill= "steelblue") +
  xlab("Gender (1 = Women, 2 = Men)") +
  ggtitle("Gender Distribution of Patients")

# Comparing gender distribution against CVD
# Very slightly more men have CVD proportionally than women but statistically insignificant
ggplot(data, aes(x = factor(gender), fill = factor(cardio))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Gender (1 = Women, 2 = Men)") +
  ylab("Proportion of Patients") +
  ggtitle("Gender by Cardiovascular Disease Status") +
  labs(fill = "CVD (0 = No, 1 = Yes)")

# Comparing gender distribution against Age
# Accounts for difference in number of men and women
ggplot(data, aes(x = age_years, fill = factor(gender))) +
  geom_density(alpha = 0.4, adjust = 1.5) +  # Adjusting for a smoother curve
  xlab("Age (Years)") +
  ylab("Density") +
  ggtitle("Normalised Age Distribution against Gender") +
  labs(fill = "Gender (1 = Women, 2 = Men)")

# Comparing gender distribution against height
ggplot(data, aes(x = height, fill = factor(gender))) +
  geom_density(alpha = 0.4, adjust = 1.5) +  # Adjusting for a smoother curve
  xlab("Height (cm)") +
  ylab("Density") +
  ggtitle("Normalised Height Distribution against Gender") +
  labs(fill = "Gender (1 = Women, 2 = Men)")

# Comparing gender distribution against weight
ggplot(data, aes(x = weight, fill = factor(gender))) +
  geom_density(alpha = 0.4, adjust = 1.5) +  # Adjusting for a smoother curve
  xlab("Weight (Kg)") +
  ylab("Density") +
  ggtitle("Normalised Weight Distribution against Gender") +
  labs(fill = "Gender (1 = Women, 2 = Men)")

# Comparing smoking by gender
# Smoking is far more prevalent in men
ggplot(data, aes(x = factor(gender), fill = factor(smoke))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Gender (1 = Women, 2 = Men)") +
  ylab("Proportion of Patients") +
  ggtitle("Smoking against Gender") +
  labs(fill = "Smoking (0 = No, 1 = Yes)")

# Comparing Alcohol consumption by gender
# Alcohol consumption is also more prevalent in men, but less so than smoking
ggplot(data, aes(x = factor(gender), fill = factor(alco))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Gender (1 = Women, 2 = Men)") +
  ylab("Proportion of Patients") +
  ggtitle("Alcohol Consumption against Gender") +
  labs(fill = "Alcohol (0 = No, 1 = Yes)")

# Comparing cholesterol levels against gender
# Women in the population have a generally higher cholesterol level than men
ggplot(data, aes(x = factor(gender), fill = factor(cholesterol))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Gender (1 = Women, 2 = Men)") +
  ylab("Proportion of Patients") +
  ggtitle("Cholesterol Levels against Gender") +
  labs(fill = "Cholesterol Level (1 = Normal, 2 = Above Normal, 3 = Well Above Normal)")

# Comparing Systolic and Diastolic blood pressure against gender
# Not entirely happy about how these look, might be better way to represent
# However women have a greater max blood pressure than men
ggplot(data, aes(x = factor(gender), y = ap_hi, fill = factor(gender))) +
  geom_boxplot() +
  xlab("Gender (1 = Women, 2 = Men)") +
  ylab("Systolic Blood Pressure") +
  ggtitle("Systolic Blood Pressure against Gender")

ggplot(data, aes(x = factor(gender), y = ap_lo, fill = factor(gender))) +
  geom_boxplot() +
  xlab("Gender (1 = Women, 2 = Men)") +
  ylab("Diastolic Blood Pressure") +
  ggtitle("Diastolic Blood Pressure against Gender")

# Comparing Glucose levels between men and women
# Women have slightly higher glucose levels
ggplot(data, aes(x = factor(gender), fill = factor(gluc))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Gender (1 = Women, 2 = Men)") +
  ylab("Proportion of Patients") +
  ggtitle("Glucose Levels against Gender") +
  labs(fill = "Glucose Level (1 = Normal, 2 = Above Normal, 3 = Well Above Normal)")

# Comparing activity by gender
# Activity levels are very similar
ggplot(data, aes(x = factor(gender), fill = factor(active))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Gender (1 = Women, 2 = Men)") +
  ylab("Proportion of Patients") +
  ggtitle("Activity against Gender") +
  labs(fill = "Activity (0 = Not Active, 1 = Active)")

# Visualize age distribution
ggplot(data, aes(x = age_years)) +
  geom_histogram(bins = 30) +
  xlab("Age (Years)") +
  ggtitle("Age Distribution Of Patients")

# Visualize distribution of cholesterol levels
ggplot(data, aes(x = factor(cholesterol))) +
  geom_bar() +
  xlab("Cholesterol Level (1: Normal, 2: Above Normal, 3: Well Above Normal)") +
  ggtitle("Cholesterol Level Distribution")

# Cardiovascular disease class balance
gg_cardio <- ggplot(df, aes(x = factor(cardio))) +
  geom_bar(fill = "darkred") +
  xlab("Cardiovascular Disease (0 = No, 1 = Yes)") +
  ggtitle("Distribution of Cardiovascular Disease Cases")
gg_cardio

# Age vs Disease status (boxplot)
gg_age_cardio <- ggplot(df, aes(x = factor(cardio), y = age_years, fill = factor(cardio))) +
  geom_boxplot() +
  xlab("Cardiovascular Disease (0 = No, 1 = Yes)") +
  ylab("Age (Years)") +
  ggtitle("Age Distribution by Cardiovascular Disease") +
  theme(legend.position = "none")
gg_age_cardio

# Smoking vs. Disease (stacked proportion)

gg_smoke <- ggplot(df, aes(x = factor(smoke), fill = factor(cardio))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  xlab("Smoking (0 = No, 1 = Yes)") +
  ylab("Proportion of Patients") +
  ggtitle("Smoking Habits of Cardiovascular Disease Status") + 
  labs(fill = "CVD (0 = No, 1 = Yes)")
gg_smoke

# Correlation matrix for numeric variables
numeric_vars <- df%>%select_if(is.numeric)
cor_matrix <- cor(numeric_vars)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

# BMI distribution by disease
gg_bmi <- ggplot(df, aes(x = bmi, fill = factor(cardio))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  xlab("BMI") + 
  ggtitle("BMI Distribution by Cardiovascular Disease") + 
  labs(fill = "CVD (0 = No, 1 = Yes)")
gg_bmi

# Relationship between alcohol status intake and cardiovascular disease
gg_alcohol <- ggplot(data, aes(x = factor(alco), fill = factor(cardio))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Alcohol Consumption (0 = No, 1 = Yes)") +
  ylab("Proportion of Patients") +
  ggtitle("Alcohol Consumption by Cardiovascular Disease Status") +
  labs(fill = "CVD (0 = No, 1 = Yes)")

# Display gg_alcohol
gg_alcohol

# Required variables are treated correctly
df$gluc <- factor(df$gluc, levels = c(1, 2, 3),
                  labels = c("Normal", "Above Normal", "Well Above Normal"))
df$active <- factor(df$active, levels = c(0, 1), labels = c("Inactive", "Active"))
df$cardio <- factor(df$cardio, levels = c(0, 1), labels = c("No", "Yes"))


# Height Distribution
gg_height <- ggplot(df, aes(x = height)) +
  geom_histogram(bins = 30, fill = "seagreen") +
  xlab("Height (cm)") +
  ggtitle("Height Distribution of Patients")
gg_height

# Weight Distribution
gg_weight <- ggplot(df, aes(x = weight)) +
  geom_histogram(bins = 30, fill = "purple") +
  xlab("Weight (kg)") +
  ggtitle("Weight Distribution of Patients")
gg_weight

# Glucose Levels by Disease Status
gg_gluc <- ggplot(df, aes(x = gluc, fill = cardio)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Glucose Level") +
  ylab("Proportion of Patients") +
  ggtitle("Glucose Levels by Cardiovascular Disease Status") +
  labs(fill = "CVD")
gg_gluc

# Physical Activity by Disease Status
gg_active <- ggplot(df, aes(x = active, fill = cardio)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Physical Activity") +
  ylab("Proportion of Patients") +
  ggtitle("Physical Activity by Cardiovascular Disease Status") +
  labs(fill = "CVD")
gg_active







# SECTION FOUR: Feature Engineering

# Load cleaned data
df <- read.csv("data/cardio_cleaned.csv")

# Drop unnecessary columns
# ID as not relevant to our research
# COMMENTING THIS OUT TEMPORARILY, NEED TO CHECK SOMETHING, GETTING ERROR, NOT SHOWING UP IN COLUMN NAMES
#df <- df %>%
#  select(-id)

# Scale numeric features
# So easier to compare
numeric_cols <- c("age_years", "height", "weight", "ap_hi", "ap_lo", "bmi")
df[numeric_cols] <- scale(df[numeric_cols])

# Changes chol & gluc to meaningful labels
df$cholesterol <- factor(df$cholesterol, levels = c(1,2,3),
                         labels = c("normal", "above_normal", "well_above_normal"))
df$gluc <- factor(df$gluc, levels = c(1,2,3),
                  labels = c("normal", "above_normal", "well_above_normal"))

# Convert chol & gluc to dummy variables, one-hot encoding
df <- cbind(df, model.matrix(~ cholesterol + gluc - 1, data = df))
df <- df %>% select(-cholesterol, -gluc)

# Convert cardio values to factors
# Changing this temporarily, model will need this as 0 or 1
#df$cardio <- factor(df$cardio, levels = c(0,1), labels = c("No", "Yes"))
df$cardio <- factor(df$cardio, levels = c(0,1))


# Save 
write.csv(df, "data/cardio_model_ready.csv")


# SECTION FIVE: Data Splitting

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

# SECTION SIX: Creation and evaluation of a simple logistic regression model
# We need to consider why we've used this and alternative things
# Do we want to see if we can predict it based on only one category like age or BMI?
# This model has an accuracy of roughly 0.7279 which is pretty decent! 
# This is adapted from the lab with the logistic regression tutorial & the confusionMatrixModel script

log_model <- glm(cardio ~., data= trainingData, family=binomial(link="logit"))

# Make Predictions
log_predictions <- predict(log_model, testData, type = "response")
log_predicted_classes <- ifelse(log_predictions > 0.5, 1, 0)

# Begin evaluation of model using a confusion matrix
# Big issues here, data and reference should be factors with the same levels
# Fix here, but if have time need to go back and check
log_conf_matrix <- confusionMatrix(
  factor(log_predicted_classes, levels = levels(factor(testData$cardio))), 
  factor(testData$cardio, levels = levels(factor(testData$cardio)))
)

print(log_conf_matrix)

# Printing a summary of the model here
summary(log_model)

# computing odds ratios and confidence intervals
exp(coef(log_model))
exp(cbind(OR = coef(log_model), confint(log_model)))

# AUC ROC - Sensitivity vs specificity
roc_object <- roc( testData$cardio, log_predictions)
ggroc(roc_object) + ggtitle("ROC Curve for Logistic Regression Model")
# AUC = 0.7897, the closer the auc is to 1, the better the model
# Close to 1: Good at distinguishing between positive and negative classes
# Close to 0.5: Performs no better than random guessing
# https://www.geeksforgeeks.org/plotting-roc-curve-in-r-programming/
auc(roc_object)

# Saving the model
# We might want to move this up before the evaluation, not sure it really matters
saveRDS(log_model, "models/cardio_logistic_model.rds")

print("Logistic Regression model created and saved as cardio_logistic_model.rds")

# Now need to do proper evaluation looking at the stats calculated above. This is where the marks are for creating the model
# Will work on this next
 
# SECTION SEVEN: Clustering model
# Will work on this next, I think since there's such an emphasis in the labs on this it would be the best move to create this
# Especially given the last guest seminar which was fascinating, but I don't think we need to be worrying about latent factors
# as we're trying to predict a non-latent factor


