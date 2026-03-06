# -----------------------------
# Chi-square Analysis Script
# Heart Disease Risk Project
# -----------------------------

# Install packages if needed
install.packages("dplyr")
install.packages("readr")
install.packages("rlang")

# Load libraries
library(dplyr)
library(readr)
library(rlang)

# -----------------------------
# Chi-square Analysis Script
# -----------------------------

# Load dataset (use base R to avoid package issues)
df <- read.csv("data/train.csv")

# -----------------------------
# Convert target to factor
# -----------------------------
df$heart_risk <- as.factor(df$heart_risk)

# -----------------------------
# Your categorical variables
# -----------------------------
categorical_vars <- c(
  "chest_pain",
  "shortness_of_breath",
  "fatigue",
  "palpitations",
  "high_bp",
  "high_cholesterol",
  "diabetes",
  "smoking",
  "obesity",
  "family_history",
  "chronic_stress",
  "gender"
)

# Convert to factor
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)

# -----------------------------
# Function for Chi-square test
# -----------------------------
chi_square_test <- function(var) {
  
  tbl <- table(df[[var]], df$heart_risk)
  
  test <- chisq.test(tbl)
  
  data.frame(
    Variable = var,
    Chi_square = round(test$statistic, 3),
    df = test$parameter,
    p_value = round(test$p.value, 5)
  )
}

# -----------------------------
# Run tests
# -----------------------------
results <- do.call(rbind, lapply(categorical_vars, chi_square_test))

# -----------------------------
# Significance column
# -----------------------------
results$Significant <- ifelse(results$p_value < 0.05, "Yes", "No")

# -----------------------------
# View results
# -----------------------------
print(results)

# -----------------------------
# Save results for dissertation
# -----------------------------
if (!dir.exists("docs")) {
  dir.create("docs")
}

write.csv(results, "docs/chi_square_results.csv", row.names = FALSE)

# -----------------------------
# Optional view
# -----------------------------
View(results)


