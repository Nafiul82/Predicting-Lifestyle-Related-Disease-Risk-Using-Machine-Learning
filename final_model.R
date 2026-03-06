gbm_model <- readRDS("models/gbm_model.rds")
saveRDS(gbm_model, "models/final_model.rds")
getwd()
list.files()
list.files("models")
source("app.R")
predict(final_model, new_user, type="prob")
final_model <- readRDS("models/final_model.rds")
final_model
install.packages("rlang")
update.packages(ask = FALSE)
chooseCRANmirror()

