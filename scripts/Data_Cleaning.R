
# Install and load required libraries
packages <- c("tidyverse", "ggplot2", "dplyr", "DataExplorer", "caret", "corrplot")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))

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
