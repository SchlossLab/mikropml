'ML Pipeline Microbiome

Usage:
  main.R --seed=<num> --model=<name> --metadata=<tsv> --otu=<tsv> --hyperparams=<tsv> --outcome=<colname> [--permutation]
  main.R --help

Options
  -h --help                  Display this help message.
  --seed=<num>               Random seed.
  --model=<name>             Model name. options:
                                L2_Logistic_Regression
                                L1_Linear_SVM
                                L2_Linear_SVM
                                RBF_SVM Decision_Tree
                                Random_Forest
                                XGBoost
  --metadata=<tsv>           Metadata filename in tsv format.
  --otu=<tsv>                OTU table filename in mothur shared format.
  --hyperparams=<tsv>        Hyperparameters filename in tsv format.
  --outcome=<colname>        Outcome column name from the metadata file.
  --permutation              Whether to perform permutation.

' -> doc
################### IMPORT LIBRARIES and FUNCTIONS ###################
# The dependinces for this script are consolidated in the first part
deps = c(
  "docopt",
  "dplyr",
  "tictoc",
  "caret" ,
  "rpart",
  "xgboost",
  "randomForest",
  "kernlab",
  "LiblineaR",
  "pROC",
  "tidyverse"
)

for (dep in deps) {
  if (!(dep %in% installed.packages())) {
    install.packages(
      dep,
      quiet = TRUE,
      repos = "http://cran.us.r-project.org",
      dependencies = TRUE
    )

  }
  library(dep, verbose = FALSE, character.only = TRUE)
}

args <- docopt(doc)
source("code/R/run_model.R")
run_model(
  args$seed,
  args$model,
  args$metadata,
  args$otu,
  args$hyperparams,
  args$outcome,
  args$permutation
)
