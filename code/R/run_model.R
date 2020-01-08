source("code/R/model_pipeline.R")
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
run_model <-
    function(seed,
             model,
             data_filename,
             hyperparam_filename,
             outcome,
             permutation) {

        dir.create(file.path("data", "temp"), showWarnings = FALSE)
        set.seed(seed)
        # Start walltime for running model
        tic("model")
        # Run the model
        # User can define the outcome and to do permutation or not here:
        # example: get_results(data, model, seed, 0, "dx)
        # OR pass as NA

        data <- read.csv(data_filename)

        if(permutation){
            if(file.exists("data/process/sig_flat_corr_matrix.csv")){
                print("Running permutation importance")
            }else{
                stop('Permutation importance can be computed only if you have created a correlation matrix.')}
        }
        else{
            print("Not running permutation importance")
        }

        # Save results of the modeling pipeline as a list
        hyperparameters <- NULL # TODO: use hyperparameters csv file
        results <- pipeline(data, model, seed, outcome=outcome, permutation=permutation, hyperparameters=hyperparameters)
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
            mutate(model=model) %>%
            write_csv(path = paste0("data/temp/best_hp_results_", model,"_", seed, ".csv"))
        # ------------------------------------------------------------------

        # ------------------------------------------------------------------
        # Save results for all hyper-parameters for 1 datasplit and corresponding AUCs
        all_results <- results[3]
        # Convert to dataframe and add a column noting the model name
        dataframe <- data.frame(all_results) %>%
            mutate(model=model) %>%
            write_csv(path=paste0("data/temp/all_hp_results_", model,"_", seed, ".csv"))
        # ------------------------------------------------------------------

        # Save sensitivity and specificity for 0.5 threshold for each datasplit

        threshold_results <- matrix(c(results[[7]], results[[8]]), ncol=2, dimnames = list(c("values"), c("sens", "spec")))

        sensspec <- data.frame(threshold_results) %>%
            mutate(model=model) %>%
            write_csv(path=paste0("data/temp/sensspec_results_", model,"_", seed, ".csv"))


        # ------------------------------------------------------------------
        # Save all non-correlated feature importance of the model for 1 datasplit
        imp_features <- results[4]
        # Convert to dataframe and add a column noting the model name
        dataframe <- data.frame(imp_features) %>%
            mutate(model=model) %>%
            write_csv(path=paste0("data/temp/all_imp_features_non_cor_results_", model,"_", seed, ".csv"))
        # ------------------------------------------------------------------

        # Save all correlated feature importance of the model for 1 datasplit
        corr_imp_features <- results[5]
        # Convert to dataframe and add a column noting the model name
        dataframe <- data.frame(corr_imp_features) %>%
            mutate(model=model) %>%
            write_csv(path=paste0("data/temp/all_imp_features_cor_results_", model,"_", seed, ".csv"), col_names = TRUE)

        # Stop walltime for running model
        secs <- toc()
        # Save elapsed time
        walltime <- secs$toc-secs$tic
        # Save wall-time
        write.csv(walltime, file=paste0("data/temp/walltime_", model, "_", seed, ".csv"), row.names=F)
    }
