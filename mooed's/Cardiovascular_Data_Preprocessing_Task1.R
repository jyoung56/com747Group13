
# ---------------------------------------------
# Cardiovascular Disease Dataset - Task 1: Data Exploration, Cleaning & Preprocessing
# ---------------------------------------------
packages <- c("tidyverse", "ggplot2", "dplyr", "DataExplorer", "caret", "corrplot")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))
df <- read.csv("cardio_train.csv", sep = ";")
str(df)
summary(df)
colSums(is.na(df))
names(df)
head(df)
ggplot(df, aes(x = factor(cardio))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Cardiovascular Disease", x = "Cardio (0=No, 1=Yes)", y = "Count")
cor_matrix <- cor(df)
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)
df <- df %>% filter(height > 100 & height < 250)
df <- df %>% filter(weight > 30 & weight < 200)
df <- df %>% filter(ap_hi > 80 & ap_hi < 250)
df <- df %>% filter(ap_lo > 40 & ap_lo < 200)
df <- df[!duplicated(df), ]
df$bmi <- df$weight / ((df$height / 100)^2)
df$gender <- as.factor(df$gender)
df$cholesterol <- as.factor(df$cholesterol)
df$gluc <- as.factor(df$gluc)
df$smoke <- as.factor(df$smoke)
df$alco <- as.factor(df$alco)
df$active <- as.factor(df$active)
df$cardio <- as.factor(df$cardio)
write.csv(df, "cardio_cleaned.csv", row.names = FALSE)
cat("✅ Data cleaned and saved as cardio_cleaned.csv\n")
