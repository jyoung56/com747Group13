
# ---------------------------------------------
# Cardiovascular Disease Dataset - Feature Engineering
# ---------------------------------------------

# Install and load required packages
packages <- c("tidyverse", "caret")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))

# Load the cleaned dataset (update the path if needed)
df <- read.csv("data/cardio_cleaned.csv")

# --------------------------
# Feature Engineering
# --------------------------

# Create BMI feature
df$bmi <- df$weight / ((df$height / 100)^2)

# Convert categorical variables to factors
df$gender <- as.factor(df$gender)
df$cholesterol <- as.factor(df$cholesterol)
df$gluc <- as.factor(df$gluc)
df$smoke <- as.factor(df$smoke)
df$alco <- as.factor(df$alco)
df$active <- as.factor(df$active)
df$cardio <- as.factor(df$cardio)

# Feature scaling (Normalize age, systolic/diastolic BP, and BMI)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df$age <- normalize(df$age)
df$ap_hi <- normalize(df$ap_hi)
df$ap_lo <- normalize(df$ap_lo)
df$bmi <- normalize(df$bmi)

# --------------------------
# Save Transformed Dataset
# --------------------------
write.csv(df, "cardio_feature_engineered.csv", row.names = FALSE)

cat("Feature Engineering Completed. Transformed dataset saved as cardio_feature_engineered.csv.")
