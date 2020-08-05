
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
  
  # get binary categorical features
  
  
  # get continuous features
  cont_feats_bool <- sapply(nonbin_feats,function(x) class(x) %in% c('numeric','integer'))
  cont_feats <- nonbin_feats %>% dplyr::select_if(cont_feats_bool)
  print(cont_feats)
  print(method)
  # pre-process continuous features
  cont_preproc_values <- caret::preProcess(cont_feats, method = method)
  print(cont_preproc_values)
  cont_transformed <- stats::predict(cont_preproc_values, cont_feats) %>% dplyr::as_tibble()

  # get categorical features
  cat_feats <- nonbin_feats %>% dplyr::select_if(!cont_feats_bool) %>% dplyr::as_tibble()
  # create design (model) matrix by expanding characters/factors to dummy variables
  feature_design <- caret::dummyVars(" ~ .", data = cat_feats)
  feature_design_matrix <- predict(feature_design, cat_feats) %>% dplyr::as_tibble()
  
  dat_transformed <- dplyr::bind_cols(outcome, cont_transformed, feature_design_matrix, bin_feats)
  
  return(dat_transformed)
}
