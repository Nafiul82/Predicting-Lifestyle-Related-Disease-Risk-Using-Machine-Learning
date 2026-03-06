library(tidyverse)
library(caret)
library(pROC)

train <- read_csv("data/train.csv")
test  <- read_csv("data/test.csv")

train$heart_risk <- as.factor(train$heart_risk)
test$heart_risk  <- as.factor(test$heart_risk)

set.seed(42)
ctrl <- trainControl(method="cv", number=3, classProbs=TRUE,
                     summaryFunction=twoClassSummary)


# Logistic Regression

log_model <- train(
  heart_risk ~ ., data=train,
  method="glm",
  family="binomial",
  trControl=ctrl,
  metric="ROC"
)


# Random Forest

rf_model <- train(
  heart_risk ~ ., data=train,
  method="rf",
  trControl=ctrl,
  metric="ROC",
  tuneGrid = data.frame(mtry = 3),
  ntree = 100 
)

# Gradient Boosting Machine (GBM)

gbm_model <- train(
  heart_risk ~ ., data=train,
  method="gbm",
  trControl=ctrl,
  metric="ROC",
  tuneLength=5,
  verbose=FALSE
)



# Evaluating on the Test Set

log_probs <- predict(log_model, test, type="prob")[,"Yes"]
rf_probs  <- predict(rf_model,  test, type="prob")[,"Yes"]
gbm_probs <- predict(gbm_model, test, type="prob")[,"Yes"]


# ROC-AUC Calculation

auc_log <- auc(roc(test$heart_risk, log_probs, levels=rev(levels(test$heart_risk))))
auc_rf  <- auc(roc(test$heart_risk, rf_probs,  levels=rev(levels(test$heart_risk))))
auc_gbm <- auc(roc(test$heart_risk, gbm_probs, levels=rev(levels(test$heart_risk))))


results <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "Gradient Boosting (GBM)"),
  AUC   = c(auc_log, auc_rf, auc_gbm)
)

print(results)


# Saving all the Models

dir.exists("models")
dir.create("models")

saveRDS(log_model, "models/log_model.rds")
saveRDS(rf_model,  "models/rf_model.rds")
saveRDS(gbm_model, "models/gbm_model.rds")




# Calculating Accuracy, Precision, Recall, F1

# Convert probabilities → class predictions

log_pred_class <- ifelse(log_probs > 0.5, "Yes", "No")
log_pred_class <- factor(log_pred_class, levels=c("No","Yes"))

rf_pred_class <- ifelse(rf_probs > 0.5, "Yes", "No")
rf_pred_class <- factor(rf_pred_class, levels=c("No","Yes"))

gbm_pred_class <- ifelse(gbm_probs > 0.5, "Yes", "No")
gbm_pred_class <- factor(gbm_pred_class, levels=c("No","Yes"))



# Confusion matrices

cm_log <- confusionMatrix(log_pred_class, test$heart_risk, positive="Yes")
cm_rf  <- confusionMatrix(rf_pred_class,  test$heart_risk, positive="Yes")
cm_gbm <- confusionMatrix(gbm_pred_class, test$heart_risk, positive="Yes")
print(cm_log)
print(cm_rf)
print(cm_gbm)


# Accuracy

acc_log <- cm_log$overall["Accuracy"]
acc_rf  <- cm_rf$overall["Accuracy"]
acc_gbm <- cm_gbm$overall["Accuracy"]



# Precision, Recall, F1

metrics_log <- cm_log$byClass[c("Precision","Recall","F1")]
metrics_rf  <- cm_rf$byClass[c("Precision","Recall","F1")]
metrics_gbm <- cm_gbm$byClass[c("Precision","Recall","F1")]



# Final Evaluation Table

results_metrics <- data.frame(
  Model = c("Logistic Regression","Random Forest","Gradient Boosting (GBM)"),
  
  Accuracy  = c(acc_log, acc_rf, acc_gbm),
  
  Precision = c(metrics_log["Precision"],
                metrics_rf["Precision"],
                metrics_gbm["Precision"]),
  
  Recall    = c(metrics_log["Recall"],
                metrics_rf["Recall"],
                metrics_gbm["Recall"]),
  
  F1_Score  = c(metrics_log["F1"],
                metrics_rf["F1"],
                metrics_gbm["F1"]),
  
  ROC_AUC   = c(auc_log, auc_rf, auc_gbm)
)

print(results_metrics)
