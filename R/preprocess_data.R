
preprocess_data <- function(dataset,outcome_colname,method=c("center","scale","nzv")){
  
  # input validation
  check_dataset(dataset)
  check_outcome_column(dataset,outcome_colname)
  
  # get outcome and features
  split_dat <- split_outcome_features(dataset,outcome_colname)
  outcome <- split_dat$outcome
  features <- split_dat$features
  
  # get binary features
  bin_feats_bool <- apply(features,2,function(x) length(unique(x)) == 2)
  bin_feats <- features %>% dplyr::select_if(bin_feats_bool)
  nonbin_feats <- features %>% dplyr::select_if(!bin_feats_bool) %>% dplyr::as_tibble()
  
  # change any categorical binary variables to 0 and 1 (full rank, i.e. only 1 column for each variable)
  feature_design_bin <- caret::dummyVars(" ~ .", data = bin_feats, fullRank = TRUE)
  feature_design_bin_mat <- stats::predict(feature_design_bin, bin_feats) %>% dplyr::as_tibble()
  
  # transform continuous features
  preproc_values_nonbin <- caret::preProcess(nonbin_feats, method = method)
  transformed_nonbin <- stats::predict(preproc_values_nonbin, nonbin_feats)# %>% dplyr::as_tibble()
  # change any categorical non-binary features to 0 and 1 (not full rank, i.e. one column for each unique element in the column)
  feature_design_nonbin <- caret::dummyVars(" ~ .", data = transformed_nonbin)
  feature_design_nonbin_mat <- stats::predict(feature_design_nonbin, transformed_nonbin) %>% dplyr::as_tibble()
  
  dat_transformed <- dplyr::bind_cols(outcome, feature_design_nonbin_mat, feature_design_bin_mat)
  
  return(dat_transformed)
}
