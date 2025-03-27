# Load required libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(scales)

# Load the dataset
df <- read.csv("data/cardio_cleaned.csv", header = TRUE)


# Preview data
str(df)
summary(df)

# 1. Age Distribution
gg_age <- ggplot(df, aes(x = age_years)) +
  geom_histogram(bins = 30, fill = "steelblue") +
    xlab("Age (Years)") +
    ggtitle("Age Distribution of Patients")
gg_age

# 2. Cholesterol Levels (categorical)
gg_chol <- ggplot(df, aes(x = cholesterol, fill = cholesterol)) +
  geom_bar() +
  xlab("Cholesterol Level") + 
  ggtitle("Cholesterol Levels in Dataset") +
  theme(legend.position = "none")
gg_chol

# 3. Cardiovascular disease class balance
gg_cardio <- ggplot(df, aes(x = factor(cardio))) +
  geom_bar(fill = "darkred") +
  xlab("Cardiovascular Disease (0 = No, 1 = Yes)") +
  ggtitle("Distribution of Cardiovascular Disease Cases")
gg_cardio

# 4. Age vs Disease status (boxplot)
gg_age_cardio <- ggplot(df, aes(x = factor(cardio), y = age_years, fill = factor(cardio))) +
  geom_boxplot() +
  xlab("Cardiovascular Disease (0 = No, 1 = Yes)") +
  ylab("Age (Years)") +
  ggtitle("Age Distribution by Cardiovascular Disease") +
  theme(legend.position = "none")
gg_age_cardio

# 5. Smoking vs. Disease (stacked proportion)

gg_smoke <- ggplot(df, aes(x = factor(smoke), fill = factor(cardio))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) + 
  xlab("Smoking (0 = No, 1 = Yes)") +
  ylab("Proportion of Patients") +
  ggtitle("Smoking Habits of Cardiovascular Disease Status") + 
  labs(fill = "CVD (0 = No, 1 = Yes")
gg_smoke

# 6. Correlation matrix for numeric variables
numeric_vars <- df%>%select_if(is.numeric)
cor_matrix <- cor(numeric_vars)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

# 7. BMI distribution by disease
gg_bmi <- ggplot(df, aes(x = bmi, fill = factor(cardio))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  xlab("BMI") + 
  ggtitle("BMI Distribution by Cardiovascular Disease") + 
  labs(fill = "CVD (0 = No, 1 = Yes)")
gg_bmi
