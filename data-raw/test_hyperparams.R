## code to prepare `test_hyperparams` dataset
library(dplyr)
test_hyperparams <- readr::read_csv("data-raw/test_hyperparams.csv") %>%
  mutate(method = recode(method,
    L2_Logistic_Regression = "regLogistic",
    RBF_SVM = "svmRadial",
    Decision_Tree = "rpart2",
    XGBoost = "xgbTree",
    Random_Forest = "rf"
  ))
usethis::use_data(test_hyperparams, overwrite = TRUE)
