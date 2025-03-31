# Load necessary libraries
library(dplyr)
library(readr)

# Load cleaned data
df <- read_csv("data/cardio_cleaned.csv")

# Drop unnecessary columns
# ID as not relevant to our research
df <- df %>%
  select(-id)

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
df$cardio <- factor(df$cardio, levels = c(0,1), labels = c("No", "Yes"))

# Save 
write_csv(df, "data/cardio_model_ready.csv")

