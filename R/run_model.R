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

#' Title
#'
#' @param seed
#' @param model
#' @param data_filename
#' @param hyperparameters
#' @param outcome
#' @param level
#' @param permutation
#'
#' @return
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#' @examples
run_model <-
  function(seed,
           model,
           data_filename,
           hyperparameters,
           outcome,
           level,
           permutation) {
    dir.create(file.path("data", "temp", level), showWarnings = FALSE)
    set.seed(seed)
    # Start walltime for running model
    tictoc::tic("model")
    # Run the model
    # User can define the outcome and to do permutation or not here:
    # example: get_results(data, model, seed, 0, "dx)
    # OR pass as NA

    # TODO: take df as input instead of reading csv
    data <- readr::read_csv(data_filename)

    if (permutation) {
      # TODO: take correlation matrix as input instead of reading csv
      if (file.exists(paste0("data/process/sig_flat_corr_matrix_", level, ".csv"))) {
        print("Running permutation importance")
      } else {
        stop("Permutation importance can be computed only if you have created a correlation matrix. Generate your own correlation matrix by using code/R/generate_corr_matrix.R")
      }
    }
    else {
      print("Not running permutation importance")
    }

    # Save results of the modeling pipeline as a list
    results <- model_pipeline(data, model, seed, outcome = outcome, permutation = permutation, level = level, hyperparameters = hyperparameters)

    # These results have
    # 1. cv_auc,
    # 2. test_auc
    # 3. total_results of all hyper-parameter settings
    # 4. feature importance

    # ------------------------------------------------------------------
    # Create a matrix with cv_aucs and test_aucs from 1 data split
    aucs <- matrix(c(results[[1]], results[[2]], results[[7]]), ncol = 3)
    # Convert to dataframe and add a column noting the model name
    aucs_dataframe <- data.frame(aucs) %>%
      dplyr::rename(cv_aucs = X1, test_aucs = X2, test_auprc = X3) %>%
      dplyr::mutate(model = model) %>%
      readr::write_csv(path = paste0("data/temp/", level, "/best_hp_results_", model, "_", seed, ".csv"))
    # ------------------------------------------------------------------

    # ------------------------------------------------------------------
    # Save results for all hyper-parameters for 1 datasplit and corresponding AUCs
    all_results <- results[3]
    # Convert to dataframe and add a column noting the model name
    dataframe <- data.frame(all_results) %>%
      dplyr::mutate(model = model) %>%
      readr::write_csv(path = paste0("data/temp/", level, "/all_hp_results_", model, "_", seed, ".csv"))
    # ------------------------------------------------------------------


    # ------------------------------------------------------------------
    # Save all non-correlated feature importance of the model for 1 datasplit
    imp_features <- results[4]
    # Convert to dataframe and add a column noting the model name
    dataframe <- data.frame(imp_features) %>%
      dplyr::mutate(model = model) %>%
      readr::write_csv(path = paste0("data/temp/", level, "/all_imp_features_weights_results_", model, "_", seed, ".csv"))
    # ------------------------------------------------------------------

    # Save all correlated feature importance of the model for 1 datasplit
    corr_imp_features <- results[5]
    # Convert to dataframe and add a column noting the model name
    # TODO: return df instead of writing csv
    dataframe <- data.frame(corr_imp_features) %>%
      dplyr::mutate(model = model) %>%
      readr::write_csv(path = paste0("data/temp/", level, "/all_imp_features_perm_results_", model, "_", seed, ".csv"), col_names = TRUE)

    # Stop walltime for running model
    secs <- tictoc::toc()
    # Save elapsed time
    walltime <- secs$toc - secs$tic
    # Save wall-time
    # TODO: return df instead of writing walltime to file
    readr::write_csv(walltime, file = paste0("data/temp/", level, "/walltime_", model, "_", seed, ".csv"), row.names = F)
  }
