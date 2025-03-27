# Load necessary libraries
library(dplyr)
library(readr)

# Load cleaned data
df <- read_csv("data/cardio_cleaned.csv")

# Drop unnecessary columns
df <- df %>%
  select(-id)

# Scale numeric features
numeric_cols <- c("age_years", "height", "weight", "ap_hi", "ap_lo", "bmi")
df[numeric_cols] <- scale(df[numeric_cols])

df$cholesterol <- factor(df$cholesterol, levels = c(1,2,3),
                         labels = c("normal", "above_normal", "well_above_normal"))
df$gluc <- factor(df$gluc, levels = c(1,2,3),
                  labels = c("normal", "above_normal", "well_above_normal"))

# Convert to dummy variables
df <- cbind(df, model.matrix(~ cholesterol + gluc - 1, data = df))
df <- df %>% select(-cholesterol, -gluc)

df$cardio <- factor(df$cardio, levels = c(0,1), labels = c("No", "Yes"))

# Save 
write_csv(df, "data/cardio_model_ready.csv")

