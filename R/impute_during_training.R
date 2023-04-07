impute_during_training <- function(dataset, outcome_colname, prefilter_threshold, method, impute_in_preprocessing, to_numeric) {
  source("preprocess.R")
  dataset[[outcome_colname]] <- replace_spaces(dataset[[outcome_colname]])
  dataset <- rm_missing_outcome(dataset, outcome_colname)
  split_dat <- split_outcome_features(dataset, outcome_colname)
  
  features <- split_dat$features
  removed_feats <- character(0)
  if (to_numeric) {
    feats <- change_to_num(features) %>%
      remove_singleton_columns(threshold = prefilter_threshold)
    removed_feats <- feats$removed_feats
    features <- feats$dat
  }
  pbtick(progbar)
  
  nv_feats <- process_novar_feats(features, progbar = progbar)
  pbtick(progbar)
  split_feats <- process_cat_feats(nv_feats$var_feats, progbar = progbar)
  pbtick(progbar)
  cont_feats <- process_cont_feats(split_feats$cont_feats, method, impute_in_preprocessing)
  pbtick(progbar)
  return(cont_feats)
}