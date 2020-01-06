######################################################################
# Author: Begum Topcuoglu
# Date: 2018-03-15
# Title: Permutation Importance for features in each model
######################################################################

######################################################################
# Description:

# This script will read in:
#     - Trained model
#     - Pre-processed held-out test data



# It will run the following:
#     - Predict the held-out test data for one data-split
#     - Permutate each feature in the test data randomly
#     - Predict transformed test data for each permutation
#     - Substract transformed prediction auc from original prediction auc
#     - Determine which feature makes the biggest difference in auc
######################################################################

######################################################################
# Dependencies and Outputs:

#     - The funtion needs transformed test set
#     - Trained model for one data-split

# Be in the project directory.

# The outputs are:
#   (1) AUC difference for each feature transformation
######################################################################


################### IMPORT LIBRARIES and FUNCTIONS ###################
# The dependinces for this script are consolidated in the first part
deps = c("tictoc", "caret", "pROC", "tidyverse");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep),
                     quiet=TRUE,
                     repos = "http://cran.us.r-project.org", dependencies=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}
######################################################################



####################### DEFINE FUNCTION  #############################
permutation_importance <- function(model, full, first_outcome, outcome){

  # Set outcome as first column if null
  #if(is.null(outcome)){
   # outcome <- colnames(full)[1]
  #}

  # -----------Get the original testAUC from held-out test data--------->
  # Calculate the test-auc for the actual pre-processed held-out data
  rpartProbs <- predict(model, full, type="prob")
  base_roc <- roc(ifelse(full[,outcome]  == first_outcome, 1, 0), rpartProbs[[1]])
  base_auc <- base_roc$auc
  # -------------------------------------------------------------------->

  # ----------- Read in the correlation matrix of full dataset---------->
  # Get the correlation matrix made by full dataset
  # This correlation matrix used Spearman correlation
  # Only has the correlatons that has:
  #     1. Coefficient = 1
  #     2. Adjusted p-value < 0.01
  corr <- read_csv("data/process/sig_flat_corr_matrix.csv") %>%
    select(-p, -cor)
  # -------------------------------------------------------------------->

  # ----------- Get the names of correlated OTUs------------------------>
  # Get the correlated unique OTU ids
  correlated_otus <- unique(c(corr$row, corr$column))
  # -------------------------------------------------------------------->

  # ----------- Get the names of non-correlated OTUs-------------------->
  # Remove those names as columns from full test data
  # Remove the diagnosis column to only keep non-correlated features
  non_correlated_otus <- full %>%
    select(-correlated_otus)

  non_correlated_otus[,outcome] <- NULL

  non_correlated_otus <- non_correlated_otus %>%
    colnames()
  # -------------------------------------------------------------------->

  # ----------- Get feature importance of non-correlated OTUs------------>
  # Start the timer
  library(tictoc)
  tic("perm")
  # Permutate each feature in the non-correlated dimensional feature vector
  # Here we are
  #     1. Permuting the values in the OTU column randomly for each OTU in the list
  #     2. Applying the trained model to the new test-data where 1 OTU is randomly shuffled
  #     3. Getting the new AUROC value
  #     4. Calculating how much different the new AUROC is from original AUROC
  # Because we do this with lapply we randomly permute each OTU one by one.
  # We get the impact each non-correlated OTU makes in the prediction performance (AUROC)
  non_corr_imp <- do.call('rbind', lapply(non_correlated_otus, function(i){
    full_permuted <- full
    full_permuted[,i] <- sample(full[,i])
    # Predict the diagnosis outcome with the one-feature-permuted test dataset
    rpartProbs_permuted <- predict(model, full_permuted, type="prob")
    # Calculate the new auc
    new_auc <- roc(ifelse(full_permuted[,outcome] == first_outcome, 1, 0), rpartProbs_permuted[[1]])$auc
    # Return how does this feature being permuted effect the auc
    return(new_auc)
  }))
  print(non_corr_imp)
  # Save non correlated results in a dataframe.
  non_corr_imp <- as.data.frame(non_corr_imp) %>%
    mutate(names=factor(non_correlated_otus)) %>%
    rename(new_auc=V1)
  # -------------------------------------------------------------------->




  # ----------- Get feature importance of correlated OTUs -------------->

  # ------------------------------- 1 --------------------------------------- #
  # Corr dataset has all the correlated OTUs in 2 columns with pairwise correlation
  # But (1) the pairwise correlations are repeated twice one in each column
  #     (2) If one OTU is correlated with more than one OTU, we want to group those
  # So the first step is:
  # Have each OTU in a group with all the other OTUs its correlated with
  # Each OTU should only be in a group once.
  non_matched_corr <- corr %>% filter(!row %in% column) %>%
    group_by(row)
  # ---------------------------------------------------------------------------- #

  # --------------------------------- 2 ---------------------------------------- #
  # We want to see what are the OTUs in each group
  # We use that tidyverse group_split to create a list of the OTUs that are grouped
  split <- group_split(non_matched_corr)
  elements_no_in_split <- length(split)
  # All the pairwise correlations are now in a list (for each OTU group, there is 1 list entry)
  # For example 1. list entry (split[[1]]) looks like this:

  # A tibble: 2 x 2
  #      row      column
  #      <chr>    <chr>
  #  1 Otu00462 Otu04448
  #  2 Otu00462 Otu06075
  # So the nested list structure is still pairwise
  # ---------------------------------------------------------------------------- #

  # --------------------------------- 3 ---------------------------------------- #
    # But that is still to many nested lists and still pairwise.
  # We want groups of OTUs all together and no repetetion
  groups <- lapply(1:elements_no_in_split, function(i){
    grouped_corr_otus <- split[[i]][2] %>%
      add_case(column=unlist(unique(split[[i]][1])))
    return(grouped_corr_otus)
  })
  # We remove the nested list to this:
  # groups[[1]]
  #
  # A tibble: 3 x 1
  #     column
  #     <chr>
  #  1 Otu04448
  #  2 Otu06075
  #  3 Otu00462
  # ---------------------------------------------------------------------------- #

  # --------------------------------------- 4----------------------------------- #
  # The list still had dataframes is them. We want the list entries to be lists as well
  groups_list <- map(groups[1:elements_no_in_split], "column")
  groups_list_sorted <- map(groups_list[1:elements_no_in_split], sort)
  # Now it looks like this for each OTU group:
  # > groups_list_sorted[[1]]
  # [1] "Otu00462" "Otu04448" "Otu06075"
  # > groups_list_sorted[[2]]
  # [1] "Otu00520" "Otu04360" "Otu04810" "Otu05861" "Otu06060" "Otu06149" "Otu06477" "Otu07111" "Otu07196" "Otu09376"
  # This goes all the way to total numbers in list in the list, because there are elements_no_in_split groups of correlated OTU groups.
  # ----------------------------------------------------------------------------- #

  # ---------------------------------- 5 ---------------------------------------- #
  # Permute the grouped OTUs together and calculate AUC change
  corr_imp <- do.call('rbind', lapply(groups_list_sorted, function(i){
    full_permuted_corr <- full
    full_permuted_corr[,unlist(groups_list_sorted[i])] <- sample(full[,unlist(groups_list_sorted[i])])
    # Predict the diagnosis outcome with the group-permuted test dataset
    rpartProbs_permuted_corr <- predict(model, full_permuted_corr, type="prob")
    # Calculate the new auc
    new_auc <- roc(ifelse(full_permuted_corr[,outcome] == first_outcome, 1, 0), rpartProbs_permuted_corr[[1]])$auc
    list <- list(new_auc, unlist(i))
    return(list)
  }))
  print(corr_imp)
  # ------------------------------------------------------------------------------ #

  # -------------------------------------- 6 ------------------------------------- #
  # Save non correlated results in a dataframe

  # Create a bunch of columns so that each OTU in the group has its own column
  # We use seperate function to break up the grouped list otf OTUs
  # Now correlated OTUs are in one row, seperated by each OTU as columns
  # Last column has the percent AUC change per group of OTUs
  x <- as.character(seq(0, elements_no_in_split, 1))
  corr_imp_appended <- as.data.frame(corr_imp) %>%
    separate(V2, into = x)
  # Unlist percent auc change to save it as a csv later
  results <- corr_imp_appended %>%
    mutate(new_auc=unlist(corr_imp_appended$V1))
  # Only keep the columns that are not all NA
  not_all_na <- function(x) any(!is.na(x))
  correlated_auc_results <- results %>%
    select(-V1, -"0") %>%
    select_if(not_all_na)
  # ------------------------------------------------------------------------------ #


  # stop timer
  secs <- toc()
  walltime <- secs$toc-secs$tic
  print(walltime)
  # Save the original AUC, non-correlated importances and correlated importances
  roc_results <- list(base_auc, non_corr_imp, correlated_auc_results)
  return(roc_results)
}
