
# Load required libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(scales)

# EDA for height, weight, glucose and activity could be useful


# install libraries
install.packages("corrplot")
install.packages("ggplot2")
install.packages("dplyr")

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

# Cholesterol Levels (categorical)
gg_chol <- ggplot(df, aes(x = cholesterol, fill = cholesterol)) +
  geom_bar() +
  xlab("Cholesterol Level") + 
  ggtitle("Cholesterol Levels in Dataset") +
  theme(legend.position = "none")
gg_chol

# Add new column for age in years (Dataset uses days)
data <- data%>%
  mutate(age_years = age / 365)

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
  labs(fill = "Alcohol (0 = No, 1 = Yes)")

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

  ggtitle("Smoking Habits by Cardiovascular Disease Status") +
  labs(fill = "CVD (0 = No, 1 = Yes)")

# Display gg_smoking
gg_smoking

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

