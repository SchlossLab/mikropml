
# Author: Begum Topcuoglu
# Date: 2019-01-14
######################################################################
# Description:
# This script trains and tests the model according to proper pipeline
######################################################################

######################################################################
# Dependencies and Outputs:
#    Model to put to function:
#       1. "L2_Logistic_Regression"
#       2. "L2_Linear_SVM"
#       3. "RBF_SVM"
#       4. "Decision_Tree"
#       5. "Random_Forest"
#       6. "XGBoost"
#    data to put to function:
#         Features: Hemoglobin levels and 16S rRNA gene sequences in the stool
#         Labels: - Colorectal lesions of 490 patients.
#                 - Defined as cancer or not.(Cancer here means: SRN)
#
# Usage:
# Call as source when using the function. The function is:
#   pipeline(data, model)

# Output:
#  A results list of:
#     1. cvAUC and testAUC for 1 data-split
#     2. cvAUC for all hyper-parameters during tuning for 1 datasplit
#     3. feature importance info on first 10 features for 1 datasplit
#     4. trained model as a caret object
######################################################################

######################################################################
#------------------------- DEFINE FUNCTION -------------------#
######################################################################
source("code/R/tuning_grid.R")
source("code/R/permutation_importance.R")

pipeline <- function(data, model, split_number, outcome=NA, hyperparameters=NULL, permutation=TRUE){

  # -----------------------Get outcome variable----------------------------->
  # If no outcome specified, use first column in data
  if(is.na(outcome)){
    outcome <- colnames(data)[1]
  }else{
    # check to see if outcome is in column names of data
    if(!outcome %in% colnames(data)){
      stop(paste('Outcome',outcome,'not in column names of data.'))
    }
  }

  # ------------------Pre-process the full data------------------------->
  # We are doing the pre-processing to the full data and then splitting 80-20
  # Scale all features between 0-1
  preProcValues <- preProcess(data, method = "range")
  dataTransformed <- predict(preProcValues, data)
  # ----------------------------------------------------------------------->

  # Get outcome variables
  first_outcome = as.character(data[,outcome][1])
  outcome_vals = unique(data[,outcome])
  if(length(outcome_vals) != 2) stop('A binary outcome variable is required.')
  second_outcome = as.character(outcome_vals[!outcome_vals == first_outcome])
  print(paste(c('first outcome:','second outcome:'),c(first_outcome,second_outcome)))


  # ------------------80-20 Datasplit for each seed------------------------->
  # Do the 80-20 data-split
  # Stratified data partitioning %80 training - %20 testing
  inTraining <- createDataPartition(dataTransformed[,outcome], p = .80, list = FALSE)
  trainTransformed <- dataTransformed[ inTraining,]
  testTransformed  <- dataTransformed[-inTraining,]
  # ----------------------------------------------------------------------->

  # -------------Define hyper-parameter and cv settings-------------------->
  # Define hyper-parameter tuning grid and the training method
  # Uses function tuning_grid() in file ('code/learning/tuning_grid.R')
  tune <- tuning_grid(trainTransformed, model, outcome, hyperparameters)
  grid <- tune[[1]]
  method <- tune[[2]]
  cv <- tune[[3]]
  # ----------------------------------------------------------------------->

  # ---------------------------Train the model ---------------------------->
  # ------------------------------- 1. -------------------------------------
  # - We train on the 80% of the full data.
  # - We use the cross-validation and hyper-parameter settings defined above to train
  # ------------------------------- 2. -------------------------------------
  # We use ROC metric for all the models
  # To do that I had to make changes to the caret package functions.
  # The files 'data/caret_models/svmLinear3.R and svmLinear5.R are my functions.
  # I added 1 line to get Decision Values for linear SVMs:
  #
  #           prob = function(modelFit, newdata, submodels = NULL){
  #             predict(modelFit, newdata, decisionValues = TRUE)$decisionValues
  #           },
  #
  # This line gives decision values instead of probabilities and computes ROC in:
  #   1. train function with the cross-validataion
  #   2. final trained model
  # using decision values and saves them in the variable "prob"
  # ------------------------------- 3. --------------------------------------
  # - If the model is logistic regression, we need to add a family=binomial parameter.
  # - If the model is random forest, we need to add a ntree=1000 parameter.
  #         We chose ntree=1000 empirically.
  # ----------------------------------------------------------------------->
  # Make formula based on outcome
  f <- as.formula(paste(outcome, '~ .'))
  print('Machine learning formula:')
  print(f)
  # Start walltime for training model
  tic("train")
  if(model=="L2_Logistic_Regression"){
  print(model)
  trained_model <-  train(f, # label
                          data=trainTransformed, #total data
                          method = method,
                          trControl = cv,
                          metric = "ROC",
                          tuneGrid = grid,
                          family = "binomial")
  }
  else if(model=="Random_Forest"){
      print(model)
      trained_model <-  train(f,
                              data=trainTransformed,
                              method = method,
                              trControl = cv,
                              metric = "ROC",
                              tuneGrid = grid,
                              ntree=1000) # not tuning ntree
  }
  else{
    print(model)
    trained_model <-  train(f,
                            data=trainTransformed,
                            method = method,
                            trControl = cv,
                            metric = "ROC",
                            tuneGrid = grid)
  }
  # Stop walltime for running model
  seconds <- toc()
  # Save elapsed time
  train_time <- seconds$toc-seconds$tic
  # Save wall-time
  write.csv(train_time, file=paste0("data/temp/traintime_", model, "_", split_number, ".csv"), row.names=F)
  # ------------- Output the cvAUC and testAUC for 1 datasplit ---------------------->
  # Mean cv AUC value over repeats of the best cost parameter during training
  cv_auc <- getTrainPerf(trained_model)$TrainROC
  # Save all results of hyper-parameters and their corresponding meanAUCs over 100 internal repeats
  results_individual <- trained_model$results
  # ---------------------------------------------------------------------------------->

  # -------------------------- Feature importances ----------------------------------->
  #   if linear: Output the weights of features of linear models
  #   else: Output the feature importances based on random permutation for non-linear models
  # Here we look at the top 20 important features
  if(permutation){
    if(model=="L1_Linear_SVM" || model=="L2_Linear_SVM" || model=="L2_Logistic_Regression"){
      # We will use the permutation_importance function here to:
      #     1. Predict held-out test-data
      #     2. Calculate ROC and AUROC values on this prediction
      #     3. Get the feature importances for correlated and uncorrelated feautures
      roc_results <- permutation_importance(trained_model, testTransformed, first_outcome, outcome)
      test_auc <- roc_results[[1]]  # Predict the base test importance
      feature_importance_non_cor <- roc_results[2] # save permutation results
      # Get feature weights
      feature_importance_cor <- trained_model$finalModel$W
    }
    else{
      # We will use the permutation_importance function here to:
      #     1. Predict held-out test-data
      #     2. Calculate ROC and AUROC values on this prediction
      #     3. Get the feature importances for correlated and uncorrelated feautures
      roc_results <- permutation_importance(trained_model, testTransformed, first_outcome, outcome)
      test_auc <- roc_results[[1]] # Predict the base test importance
      feature_importance_non_cor <- roc_results[2] # save permutation results of non-cor
      feature_importance_cor <- roc_results[3] # save permutation results of cor
    }
  }else{
    print("No permutation test being performed.")
    if(model=="L1_Linear_SVM" || model=="L2_Linear_SVM" || model=="L2_Logistic_Regression"){
      # Get feature weights
      feature_importance_non_cor <- trained_model$finalModel$W
      # Get feature weights
      feature_importance_cor <- trained_model$finalModel$W
    }else{
      # Get feature weights
      feature_importance_non_cor <- NULL
      # Get feature weights
      feature_importance_cor <- NULL
    }
    # Calculate the test-auc for the actual pre-processed held-out data
    rpartProbs <- predict(trained_model, testTransformed, type="prob")
    test_roc <- roc(ifelse(testTransformed[,outcome] == first_outcome, 1, 0), rpartProbs[[1]])
    test_auc <- test_roc$auc

    p_class <- ifelse(rpartProbs$cancer > 0.5, "cancer", "normal")
    r <- confusionMatrix(as.factor(p_class), testTransformed$dx)
    sensitivity <- r$byClass[[1]]
    specificity <- r$byClass[[2]]
  }

  # ---------------------------------------------------------------------------------->

  # ----------------------------Save metrics as vector ------------------------------->
  # Return all the metrics
  results <- list(cv_auc, test_auc, results_individual, feature_importance_non_cor, feature_importance_cor, trained_model, sensitivity, specificity)
  return(results)
}
