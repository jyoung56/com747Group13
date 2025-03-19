# install libraries
install.packages("corrplot")
install.packages("ggplot2")
install.packages("dplyr")

# Load necessary libraries
library("corrplot")
library("ggplot2")
library("dplyr")

# Check structure of the data
str(data)

# Summary statistics for all columns
summary(data)

# Check for missing values in dataset
colSums(is.na(data))

# Add new column for age in years (Dataset uses days)
data <- data%>%
  mutate(age_years = age / 365)

# Visualize age distribution
ggplot(data, aes(x = age_years)) +
  geom_histogram(bins = 30) +
  xlab("Age (Years)") +
  ggtitle("Age Distribution Of Patients")

# Visualize distribution of cholesterol levels
ggplot(data, aes(x = factor(cholesterol))) +
  geom_bar() +
  xlab("Cholesterol Level (1: Normal, 2: Above Normal, 3: Well Above Normal") +
  ggtitle("Cholesterol Level Distribution")

# Correlation analysis of numeric variables
numeric_data <- data[, sapply(data, is.numeric)]
corr_matrix <- cor(numeric_data)
corrplot(corr_matrix, method = "color", type = "upper")

# Visualize the count of cardiovascular disease cases
ggplot(data, aes(x = factor(cardio))) +
  geom_bar(fill = "steelblue") +
  xlab("Cardiovascular Disease (0: No, 1: Yes)") +
  ggtitle("Distribution of Cardiovascular Disease Cases")

# Boxplot to compare age distribution between those with and without CVD
gg_age_cardio <- ggplot(data, aes(x = factor(cardio), y = age_years)) +
  geom_boxplot(fill = "lightblue") +
  xlab("Cardiovascular Disease (0 = No, 1 = Yes)") +
  ylab("Age (Years)") +
  ggtitle("Age Distribution by Cardiovascular Disease")

# Display gg_age_cardio
gg_age_cardio

# Compare smoking habits between those with and without CVD
gg_smoking <- ggplot(data, aes(x = factor(smoke), fill = factor(cardio))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  xlab("Smoking (0 = No, 1 = Yes)") +
  ylab("Proportion of Patients") +
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

# Compare systolic blood pressure distribution for patients
gg_bp <- ggplot(data, aes(x = ap_hi, fill = factor(cardio))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  xlab("Systolic Blood Pressure (ap_hi)") +
  ggtitle("Systolic Blood Pressure Distribution by Disease Status") +
  labs(fill = "CVD (0 = No, 1 = Yes")

# Display gg_bp
gg_bp