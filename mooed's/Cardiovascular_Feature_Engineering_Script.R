
packages <- c("tidyverse", "caret")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))
df <- read.csv("cardio_cleaned.csv")
df$bmi <- df$weight / ((df$height / 100)^2)
df$gender <- as.factor(df$gender)
df$cholesterol <- as.factor(df$cholesterol)
df$gluc <- as.factor(df$gluc)
df$smoke <- as.factor(df$smoke)
df$alco <- as.factor(df$alco)
df$active <- as.factor(df$active)
df$cardio <- as.factor(df$cardio)
normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}
df$age <- normalize(df$age)
df$ap_hi <- normalize(df$ap_hi)
df$ap_lo <- normalize(df$ap_lo)
df$bmi <- normalize(df$bmi)
write.csv(df, "cardio_feature_engineered.csv", row.names = FALSE)
cat("✅ Feature engineering completed: cardio_feature_engineered.csv saved\n")
