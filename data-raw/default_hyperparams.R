## code to prepare `default_hyperparams` dataset
library(dplyr)
default_hyperparams <- readr::read_csv("data-raw/default_hyperparams.csv") %>%
  mutate(method = recode(method,
    L2_Logistic_Regression = "regLogistic",
    RBF_SVM = "svmRadial",
    Decision_Tree = "rpart2",
    XGBoost = "xgbTree",
    Random_Forest = "rf"
  ))
usethis::use_data(default_hyperparams, overwrite = TRUE)
