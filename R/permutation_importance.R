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


####################### DEFINE FUNCTION  #############################
#  This function groups correlated features together and returns all features as a list (grouped ones together, ungrouped ones individually)
# Corr dataset has all the correlated OTUs in 2 columns with pairwise correlation
# But (1) the pairwise correlations are repeated twice one in each column
#    (2) If one OTU is correlated with more than one OTU, we want to group those



#' Title
#'
#' @param corr
#' @param test_data
#'
#' @return
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#' @examples
group_correlated_features <- function(corr,test_data){
  all_feats = colnames(test_data)[2:ncol(test_data)]
  corr_feats = unique(c(corr$column,corr$row))
  noncorr_feats = all_feats[!all_feats %in% corr_feats]

  grps = as.list(noncorr_feats)
  accounted_for = rep(NA,length(all_feats))
  af_length = sum(!is.na(accounted_for))
  c=length(grps)+1
  for(i in corr_feats){
    if(i %in% accounted_for) next
    feats = unique(c(i,corr$row[corr$column == i],corr$column[corr$row == i]))
    new_feats = T
    while(new_feats){
      len_feats = length(feats)
      for(j in feats){
        feats = unique(c(feats,j,corr$row[corr$column == j],corr$column[corr$row == j]))
      }
      new_feats = length(feats) > len_feats
    }
    grps[[c]] = feats
    af_length_new = sum(af_length,length(feats))
    accounted_for[(af_length+1):af_length_new] = feats
    af_length = af_length_new
    c = c+1
  }
  grps = sapply(grps,paste,collapse='|')
  return(grps)
}

# get permuted AUROC difference for a single feature (or group of features)
#' Title
#'
#' @param model
#' @param test_data
#' @param outcome
#' @param feat
#' @param fewer_samples
#'
#' @return
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#' @examples
find_permuted_auc <- function(model,test_data,outcome,feat, fewer_samples){
  # -----------Get the original testAUC from held-out test data--------->
  # Calculate the test-auc for the actual pre-processed held-out data
  test_auc <- calc_aucs(model, test_data, outcome, fewer_samples)$auroc
  print('test_auc')
  print(test_auc)
  print('new_auc')
  # permute grouped features together
  fs = strsplit(feat, '\\|')[[1]]
  # only include ones in the test data split
  fs = fs[fs %in% colnames(test_data)]
  print(fs)
  # get the new AUC and AUC differences
  auc_diffs <- sapply(0:99, function(s){
    set.seed(s)
    full_permuted <- test_data
    if(length(fs) == 1){
      full_permuted[,fs] <- sample(full_permuted[,fs])
    }else{
      full_permuted[,fs] <- t(sample(data.frame(t(full_permuted[,fs]))))
    }
    print(sum(test_data != full_permuted))
    # Calculate the new auc
    new_auc <- calc_aucs(model, full_permuted, outcome, fewer_samples)$auroc
    # Return how does this feature being permuted effect the auc
    return(c(new_auc=new_auc,diff=(test_auc - new_auc)))
  })
  auc = mean(auc_diffs['new_auc',])
  auc_diff = mean(auc_diffs['diff',])
  print(auc)
  print(auc_diff)
  return(c(auc=auc,auc_diff=auc_diff))
}

#' Title
#'
#' @param model
#' @param test_data
#' @param first_outcome
#' @param second_outcome
#' @param outcome
#' @param fewer_samples
#' @param level
#'
#' @return
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#' @examples
permutation_importance <- function(model, test_data, first_outcome, second_outcome, outcome, fewer_samples, level){



  # ----------- Read in the correlation matrix of full dataset---------->
  # Get the correlation matrix made by full dataset
  # This correlation matrix used Spearman correlation
  # Only has the correlatons that has:
  #     1. Coefficient = 1
  #     2. Adjusted p-value < 0.01
  # TODO: don't read csv, take a df
  corr = readr::read_csv(paste0("data/process/sig_flat_corr_matrix_",level,".csv")) %>%
    dplyr::select(-p, -cor)  #TODO: maybe shouldn't hard-code column names?
  # -------------------------------------------------------------------->

  # get groups of correlated features
  grps = group_correlated_features(corr, test_data)

  # -------------------------------------------------------------------->

  # ----------- Get feature importance of OTUs------------>
  # Start the timer
  tictoc::tic("perm")
  # Permutate each feature in the non-correlated dimensional feature vector
  # Here we are
  #     1. Permuting the values in the OTU column randomly for each OTU in the list
  #     2. Applying the trained model to the new test-data where 1 OTU is randomly shuffled
  #     3. Getting the new AUROC value
  #     4. Calculating how much different the new AUROC is from original AUROC
  # Because we do this with lapply we randomly permute each OTU one by one.
  # We get the impact each non-correlated OTU makes in the prediction performance (AUROC)
  imps <- do.call('rbind', lapply(grps, function(feat){
    res=find_permuted_auc(model,test_data,outcome,feat,fewer_samples)
    return(res)
  }))

  imps <- as.data.frame(imps) %>%
    dplyr::mutate(names=factor(grps))


  # -------------------------------------- 6 ------------------------------------- #
  # Save non correlated results in a dataframe

  # Create a bunch of columns so that each OTU in the group has its own column
  # We use seperate function to break up the grouped list otf OTUs
  # Now correlated OTUs are in one row, seperated by each OTU as columns
  # Last column has the percent AUC change per group of OTUs
  # x <- as.character(seq(0, elements_no_in_split, 1))
  # corr_imp_appended <- as.data.frame(corr_imp) %>%
  #   separate(V2, into = x)
  # # Unlist percent auc change to save it as a csv later
  # results <- corr_imp_appended %>%
  #   mutate(new_auc=unlist(corr_imp_appended$V1))
  # # Only keep the columns that are not all NA
  # not_all_na <- function(x) any(!is.na(x))
  # correlated_auc_results <- results %>%
  #   select(-V1, -"0") %>%
  #   select_if(not_all_na)
  # ------------------------------------------------------------------------------ #


  # stop timer
  secs <- tictoc::toc()
  walltime <- secs$toc-secs$tic
  print(walltime)
  # Save the original AUC, non-correlated importances and correlated importances
  perm_results <- imps #list(base_auc, non_corr_imp, correlated_auc_results)
  return(perm_results)
}
