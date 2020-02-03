'ML Pipeline Microbiome

Usage:
  main.R --seed=<num> --model=<name> --data=<csv> --hyperparams=<csv> --outcome=<colname> [--permutation]
  main.R --configfile=<yml>
  main.R --help

Options
  -h --help                  Display this help message.
  --configfile=<yml>         Config file in yaml format containing all paramters listed below.
  --seed=<num>               Random seed.
  --model=<name>             Model name. options:
                                L2_Logistic_Regression
                                L1_Linear_SVM
                                L2_Linear_SVM
                                RBF_SVM
                                Decision_Tree
                                Random_Forest
                                XGBoost
  --data=<csv>               Dataset filename in csv format.
  --hyperparams=<csv>        Hyperparameters filename in csv format.
  --outcome=<colname>        Outcome column name from the metadata file.
  --permutation              Whether to perform permutation importance.

' -> doc

deps = c(
  "docopt",
  "tictoc",
  "caret" ,
  "rpart",
  "xgboost",
  "randomForest",
  "kernlab",
  "LiblineaR",
  "pROC",
  "tidyverse",
  "yaml"
)

for (dep in deps) {
  library(dep, character.only = TRUE)
}

args <- docopt(doc)
if ("configfile" %in% names(args) & !is.null(args$configfile)) {
  args <- read_yaml(args$configfile)
}

source("code/R/run_model.R")
run_model(
  args$seed,
  args$model,
  args$data,
  args$hyperparams,
  args$outcome,
  args$permutation
)
