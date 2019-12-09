######################################################################
# Author: Begum Topcuoglu
# Date: 2019-01-15
# Title: Generate files that has cv and test AUCs for 1 data-split 
######################################################################

######################################################################
# Dependencies and Outputs: 
# This function accept:
#   1. Data file generated in main.R (features and label as dataframe)
#   2. Model name defined in command line as an argument: 
#       "L2_Logistic_Regression"
#       "L1_Linear_SVM"
#       "L2_Linear_SVM"
#       "RBF_SVM"
#       "Decision_Tree"
#       "Random_Forest"
#       "XGBoost"
#   3. Seed number defined as argument in command line:
#       [1-100]


# Call as source when using the function. The function is:
#   get_AUCs()

# The output:
#  Results .csv files:
#     1. cvAUC and testAUC for 1 data-split
#     2. cvAUC for all hyper-parameters during tuning for 1 datasplit
#     3. feature importance info on first 10 features for 1 datasplit
######################################################################


######################################################################
#------------------------- DEFINE FUNCTION -------------------#
######################################################################
get_results <- function(dataset, models, split_number, perm=T, outcome=NA, hyperparameters=NULL){
  # Save results of the modeling pipeline as a list
  results <- pipeline(dataset, models, split_number,outcome=outcome,perm=perm,hyperparameters=hyperparameters) 
  # These results have
  # 1. cv_auc, 
  # 2. test_auc
  # 3. total_results of all hyper-parameter settings
  # 4. feature importance
  
  # ------------------------------------------------------------------ 
  # Create a matrix with cv_aucs and test_aucs from 1 data split
  aucs <- matrix(c(results[[1]], results[[2]]), ncol=2) 
  # Convert to dataframe and add a column noting the model name
  aucs_dataframe <- data.frame(aucs) %>% 
    rename(cv_aucs=X1, test_aucs=X2) %>% 
    mutate(model=models) %>% 
    write_csv(path = paste0("data/temp/best_hp_results_", models,"_", split_number, ".csv"))
  # ------------------------------------------------------------------   

  # ------------------------------------------------------------------   
  # Save results for all hyper-parameters for 1 datasplit and corresponding AUCs
  all_results <- results[3]
  # Convert to dataframe and add a column noting the model name
  dataframe <- data.frame(all_results) %>% 
    mutate(model=models) %>% 
    write_csv(path=paste0("data/temp/all_hp_results_", models,"_", split_number, ".csv"))
  # ------------------------------------------------------------------ 
  
  # ------------------------------------------------------------------   
  # Save all non-correlated feature importance of the model for 1 datasplit
  imp_features <- results[4]
  # Convert to dataframe and add a column noting the model name
  dataframe <- data.frame(imp_features) %>% 
    mutate(model=models) %>% 
    write_csv(path=paste0("data/temp/all_imp_features_non_cor_results_", models,"_", split_number, ".csv"))
  # ------------------------------------------------------------------ 
  
  # Save all correlated feature importance of the model for 1 datasplit
  corr_imp_features <- results[5]
  # Convert to dataframe and add a column noting the model name
  dataframe <- data.frame(corr_imp_features) %>% 
    mutate(model=models) %>% 
    write_csv(path=paste0("data/temp/all_imp_features_cor_results_", models,"_", split_number, ".csv"), col_names = TRUE)

}

