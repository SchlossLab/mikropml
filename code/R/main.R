######################################################################
# Author: Begum Topcuoglu
# Date: 2018-12-20
# Title: Main pipeline for 7 classifiers in R programming language
######################################################################

######################################################################
# Description:

# This script will read in data from Baxter et al. 2016
#     - 0.03 subsampled OTU dataset
#     - CRC metadata: SRN information


# It will run the following machine learning pipelines:
#     - L2 Logistic Regression
#     - L1 and L2 Linear SVM
#     - RBF SVM
#     - Decision Tree
#     - Random Forest
#     - XGBoost
######################################################################

######################################################################
# Dependencies and Outputs:

# Be in the project directory.

# The outputs are:
#   (1) AUC values for cross-validation and testing for each data-split
#   (2) meanAUC values for each hyper-parameter tested during each split.
######################################################################


################### IMPORT LIBRARIES and FUNCTIONS ###################
# The dependinces for this script are consolidated in the first part
deps = c("dplyr", "tictoc", "caret" ,"rpart", "xgboost", "randomForest", "kernlab","LiblineaR", "pROC", "tidyverse");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE, repos = "http://cran.us.r-project.org", dependencies=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}
# Load in needed functions and libraries
source('code/R/tuning_grid.R')
source('code/R/model_pipeline.R') # has pipeline function defined here (called within generate.AUCs.R)
source('code/R/generateAUCs.R') # has get_results function defined here
source('code/R/permutation_importance.R')
######################################################################


######################## RUN PIPELINE #############################
# Choose which classification methods we want to run on command line
#                "L2_Logistic_Regression",
#                "L1_Linear_SVM",
#                "L2_Linear_SVM",
#                "RBF_SVM",
#                "Decision_Tree",
#                "Random_Forest",
#                "XGBoost"

# We will run main.R from command line with arguments
#  - These arguments will be saved into variable "input"
#  - First argument is the seed number which is the array index
#  - Second argument is the model name (one of the list above)

input <- commandArgs(trailingOnly=TRUE)
dataset <- input[1] # Has all the features and the outcome column in a csv file.
seed <- as.numeric(input[2])
model <- input[3]
permutation <- as.numeric(input[4]) # 1 if permutation performed
                                    # 0 if permutation not performed
outcome <- input[5]



# Then arguments 1 and 2 will be placed respectively into the functions:
#   1. set.seed() : creates reproducibility and variability
#   2. get_results(): self-defined function that
#                     - runs the modeling pipeline
#                     - saves performance and hyper-parameters and imp features
set.seed(seed)
# Start walltime for running model
tic("model")
# Run the model
  # User can define the outcome and to do permutation or not here:
  # example: get_results(data, model, seed, 0, "dx)
  # OR pass as NA

data <- read.csv(dataset)

get_results(data, model, seed, permutation, outcome)
# Stop walltime for running model
secs <- toc()
# Save elapsed time
walltime <- secs$toc-secs$tic
# Save wall-time
write.csv(walltime, file=paste0("data/temp/walltime_", model, "_", seed, ".csv"), row.names=F)
###################################################################
