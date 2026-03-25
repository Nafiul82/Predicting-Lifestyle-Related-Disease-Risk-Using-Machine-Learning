
# Feature Importance Analysis


if (!require(gbm)) install.packages("gbm")
if (!require(caret)) install.packages("caret")
if (!require(ggplot2)) install.packages("ggplot2")

library(gbm)
library(caret)
library(ggplot2)


final_model <- readRDS("models/final_model.rds")

print(class(final_model))


# Extract

importance <- caret::varImp(final_model, scale = TRUE)

# Print results
print(importance)


# Converting to dataframe

imp_df <- importance$importance
imp_df$Feature <- rownames(imp_df)

# Removing rownames
rownames(imp_df) <- NULL
View(imp_df)   # Opens spreadsheet-style viewer

cat("\nTop 10 Important Features:\n")
print(head(imp_df, 10))


# Plotting feature importance

p <- ggplot(imp_df, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance for Heart Disease Prediction (Gradient Boosting)",
    x = "Features",
    y = "Importance Score"
  ) +
  theme_minimal()

print(p)


if (!dir.exists("docs")) {
  dir.create("docs")
}

# Saving plot

ggsave(
  filename = "docs/feature_importance.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

cat("Feature importance plot saved in docs folder.")
