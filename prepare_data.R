
install.packages(c("tidyverse", "janitor", "caret", "pROC"))


library(tidyverse)
library(janitor)
library(caret)

df <- read_csv("Data/HealthRiskPredictorDataset.csv") %>%
  clean_names()


# Handling  Missing Values

df <- df %>% drop_na()


# Convert Binary Columns to Factors

binary_cols <- c("chest_pain","shortness_of_breath","fatigue","palpitations",
                 "high_bp","high_cholesterol","diabetes","smoking",
                 "obesity","family_history","chronic_stress","gender","heart_risk")

df[binary_cols] <- lapply(df[binary_cols], factor)


# Only Age is numeric, so we scale Age
preproc <- preProcess(df["age"], method = c("center", "scale"))
scaled_age <- predict(preproc, df["age"])

df$age_scaled <- scaled_age$age
str(df$age_scaled)



names(df)





write_csv(df, "data/HealthRiskPredictorDataset.csv")


saveRDS(preproc, "models/age_scaler.rds")







