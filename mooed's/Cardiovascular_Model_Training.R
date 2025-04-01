
packages <- c("caret", "randomForest")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))
df <- read.csv("cardio_feature_engineered.csv")
set.seed(123)
trainIndex <- createDataPartition(df$cardio, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]
write.csv(train_data, "train_data.csv", row.names = FALSE)
write.csv(test_data, "test_data.csv", row.names = FALSE)
log_model <- glm(cardio ~ ., data = train_data, family = binomial)
saveRDS(log_model, "log_model.rds")
rf_model <- randomForest(cardio ~ ., data = train_data, ntree = 100, importance = TRUE)
saveRDS(rf_model, "rf_model.rds")
cat("✅ Models trained and saved: log_model.rds, rf_model.rds\n")
