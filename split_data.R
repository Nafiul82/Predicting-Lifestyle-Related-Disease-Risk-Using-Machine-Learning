library(caret)

df <- read_csv("data/HealthRiskPredictorDataset.csv")
df$heart_risk <- factor(df$heart_risk, levels=c(0,1), labels=c("No","Yes"))

set.seed(123)

train_index <- createDataPartition(df$heart_risk, p=0.8, list=FALSE)
train <- df[train_index, ]
test  <- df[-train_index,] 

write_csv(train, "data/train.csv")
write_csv(test,  "data/test.csv")

dim(train)
dim(test)


