library(tidyverse)
library(caret)
library(pROC)

train <- read_csv("data/train.csv")
test  <- read_csv("data/test.csv")

train$heart_risk <- as.factor(train$heart_risk)
test$heart_risk  <- as.factor(test$heart_risk)

set.seed(42)
ctrl <- trainControl(method="cv", number=5, classProbs=TRUE,
                     summaryFunction=twoClassSummary)

# -----------------------------
# 1️⃣ Logistic Regression
# -----------------------------
log_model <- train(
  heart_risk ~ ., data=train,
  method="glm",
  family="binomial",
  trControl=ctrl,
  metric="ROC"
)

# -----------------------------
# 2️⃣ Random Forest
# -----------------------------
rf_model <- train(
  heart_risk ~ ., data=train,
  method="rf",
  trControl=ctrl,
  metric="ROC",
  tuneLength=5
)

# -----------------------------
# 3️⃣ XGBoost (Boosting Model)
# -----------------------------
xgb_model <- train(
  heart_risk ~ ., data=train,
  method="xgbTree",
  trControl=ctrl,
  metric="ROC",
  tuneLength=5
)

# -----------------------------
# Evaluate on Test Set
# -----------------------------
log_probs <- predict(log_model, test, type="prob")[,"Yes"]
rf_probs  <- predict(rf_model,  test, type="prob")[,"Yes"]
xgb_probs <- predict(xgb_model, test, type="prob")[,"Yes"]

auc_log <- auc(roc(test$heart_risk, log_probs, levels=rev(levels(test$heart_risk))))
auc_rf  <- auc(roc(test$heart_risk, rf_probs,  levels=rev(levels(test$heart_risk))))
auc_xgb <- auc(roc(test$heart_risk, xgb_probs, levels=rev(levels(test$heart_risk))))

results <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "XGBoost"),
  AUC   = c(auc_log, auc_rf, auc_xgb)
)

print(results)

# -----------------------------
# Save All Models
# -----------------------------
saveRDS(log_model, "models/log_model.rds")
saveRDS(rf_model,  "models/rf_model.rds")
saveRDS(xgb_model, "models/xgb_model.rds")
