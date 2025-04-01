
packages <- c("caret", "pROC", "randomForest")
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))
test_data <- read.csv("test_data.csv")
log_model <- readRDS("log_model.rds")
rf_model <- readRDS("rf_model.rds")
test_data$cardio <- as.factor(test_data$cardio)
cat("\n==============================\n")
cat("🔍 Logistic Regression Results\n")
cat("==============================\n")
log_probs <- predict(log_model, newdata = test_data, type = "response")
log_preds <- ifelse(log_probs > 0.5, 1, 0)
log_preds <- factor(log_preds, levels = c(0, 1))
print(confusionMatrix(log_preds, test_data$cardio))
cat("AUC: ", round(auc(test_data$cardio, log_probs), 4), "\n")
cat("\n==============================\n")
cat("🌲 Random Forest Results\n")
cat("==============================\n")
rf_preds <- predict(rf_model, newdata = test_data)
rf_probs <- predict(rf_model, newdata = test_data, type = "prob")[, 2]
print(confusionMatrix(rf_preds, test_data$cardio))
cat("AUC: ", round(auc(test_data$cardio, rf_probs), 4), "\n")
cat("✅ Evaluation complete.\n")
