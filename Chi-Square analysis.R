

install.packages("dplyr")
install.packages("readr")
install.packages("rlang")

library(dplyr)
library(readr)
library(rlang)

# Chi-square Analysis Script

df <- read.csv("data/train.csv")


df$heart_risk <- as.factor(df$heart_risk)

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

# Converting to factor
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)


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


results <- do.call(rbind, lapply(categorical_vars, chi_square_test))


results$Significant <- ifelse(results$p_value < 0.05, "Yes", "No")


print(results)


if (!dir.exists("docs")) {
  dir.create("docs")
}

write.csv(results, "docs/chi_square_results.csv", row.names = FALSE)


View(results)


