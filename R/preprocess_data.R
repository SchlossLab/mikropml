
#' Preprocess data prior to running machine learning
#' 
#'
#' @param dataset dataframe with an outcome variable and other columns as features
#' @param outcome_colname column name as a string of the outcome variable
#' @param method methods to preprocess the data, described in `caret::preProcess` (defaut: `c("center","scale")`)
#' @param rm_nzv whether to remove variables with near-zero variance (default: `TRUE`)
#'
#' @return preprocessed data
#' @export
#'
#' @examples preprocess_data(mikRopML::otu_small,'dx')
preprocess_data <- function(dataset,outcome_colname,method=c("center","scale"),rm_nzv=TRUE){
  
  # input validation
  check_dataset(dataset)
  check_outcome_column(dataset,outcome_colname)
  
  # get outcome and features
  split_dat <- split_outcome_features(dataset,outcome_colname)
  outcome <- split_dat$outcome
  features <- split_dat$features
  
  # process features with no variation
  nv_feats <- process_novar_feats(features)
  novar_feats <- nv_feats$novar_feats
  var_feats <- nv_feats$var_feats
  
  # process binary features 
  b_feats <- process_bin_feats(var_feats)
  bin_feats <- b_feats$bin_feats
  nonbin_feats <- b_feats$nonbin_feats
  
  # process nonbinary features
  nonbin_feats_transformed <- nonbin_feats
  if(!is.null(nonbin_feats_transformed)) 
    nonbin_feats_transformed <- process_nonbin_feats(nonbin_feats,method)
  
  # combine all features
  proccessed_feats <- dplyr::bind_cols(nonbin_feats_transformed, bin_feats, novar_feats)
  
  # remove features with near-zero variance
  if(rm_nzv) proccessed_feats <- get_caret_processed_df(proccessed_feats, 'nzv')
  
  # combine outcome and features
  dat_transformed <- dplyr::bind_cols(outcome,proccessed_feats) %>% dplyr::as_tibble()
  
  return(dat_transformed)
}

#' Process features with no variation
#'
#' @param features dataframe of features for machine learning 
#'
#' @return list of two dataframes: features with variability (unprocessed) and without (processed)
#' @export
#'
#' @examples process_novar_feats(mikRopML::otu_small[,2:ncol(otu_small)])
process_novar_feats <- function(features){
  
  check_features(features)
  
  # get features with no variation
  novar_feats_bool <- apply(features,2,function(x) length(unique(x)) == 1)
  novar_feats <- features %>% dplyr::select_if(novar_feats_bool)
  
  # change categorical features with no variation to zero
  novar_feats_mat <- sapply(novar_feats, function(x){
    if(class(x) %in% c('factor','character')){
      rep(0,length(x))
    }else{
      x
    }
  }) %>% dplyr::as_tibble()
  
  if(ncol(novar_feats_mat) == 0) novar_feats_mat = NULL
  
  # get features with variation
  var_feats <- features %>% dplyr::select_if(!novar_feats_bool) %>% dplyr::as_tibble()
  
  if(ncol(var_feats) == 0) stop('All features have zero variance.')
  
  return(list(novar_feats=novar_feats_mat,var_feats=var_feats))
  
}

#' Process binary features
#'
#' @param features dataframe of features for machine learning 
#'
#' @return list of two dataframes: binary (processed) and nonbinary features (unprocessed)
#' @export
#'
#' @examples process_bin_feats(mikRopML::otu_small[,2:ncol(otu_small)])
process_bin_feats <- function(features){
  
  check_features(features)
  
  bin_feats_bool <- apply(features,2,function(x) length(unique(x)) == 2)
  bin_feats <- features %>% dplyr::select_if(bin_feats_bool)
  nonbin_feats <- features %>% dplyr::select_if(!bin_feats_bool) %>% dplyr::as_tibble()
  if(ncol(nonbin_feats) == 0) nonbin_feats = NULL
  
  
  feature_design_bin_mat <- NULL
  if(ncol(bin_feats) != 0){
    # change categorical binary variables to 0 and 1 (full rank, i.e. only 1 column for each variable)
    feature_design_bin_mat <- get_caret_dummyvars_df(bin_feats, full_rank = TRUE)
    
  }
  
  return(list(bin_feats=feature_design_bin_mat,nonbin_feats=nonbin_feats))
  
}

#' Preprocess nonbinary features
#'
#' @param features dataframe of features for machine learning 
#' @param method methods to preprocess the data, described in `caret::preProcess` (defaut: `c("center","scale")`)
#'
#' @return dataframe of preprocessed features
#' @export
#'
#' @examples process_nonbin_feats(mikRopML::otu_small[,2:ncol(otu_small)],c('center','scale','nzv'))
process_nonbin_feats <- function(features, method){
  
  check_features(features)
  
  feature_design_nonbin_mat <- NULL
  
  if(ncol(features) != 0){
    # types of features present
    class_feats <- sapply(features, function(x) class(x))
    cont_feats <- sum(class_feats %in% c('integer','numeric'))
    cat_feats <- sum(class_feats %in% c('character','factor'))
    
    # transform continuous features
    transformed_nonbin <- features
    if(cont_feats > 0 & !is.null(method)){
      transformed_nonbin <- get_caret_processed_df(features, method)
    }
    
    # change categorical non-binary features to 0 and 1 (not full rank, i.e. one column for each unique element in the column)
    
    feature_design_nonbin_mat <- transformed_nonbin
    if(cat_feats > 0){
      feature_design_nonbin_mat <- get_caret_dummyvars_df(transformed_nonbin, full_rank = FALSE)
    }
  }
    
    return(feature_design_nonbin_mat)
}

#' Get preprocessed dataframe for continuous variables
#'
#' @inheritParams process_nonbin_feats 
#'
#' @param features dataframe of features for machine learning 
#' @param method methods to preprocess the data, described in `caret::preProcess`
#'
#' @return processed matrix
#' @export
#'
#' @examples get_caret_processed_df(mikRopML::otu_small[,2:ncol(otu_small)],c('center','scale'))
get_caret_processed_df <- function(features, method){
  check_features(features)
  preproc_values <- caret::preProcess(features, method = method)
  processed <- stats::predict(preproc_values, features)
  return(processed)
}

#' Get dummyvars dataframe (i.e. design matrix)
#'
#' @param features dataframe of features for machine learning 
#' @param full_rank whether to return full rank matrix (see `caret::dummyVars`)
#'
#' @return design matrix
#' @export
#'
#' @examples 
#' df <- data.frame(
#' outcome = c("normal", "normal", "cancer"),
#' var1 = 1:3,
#' var2 = c('a','b','c'),
#' var3 = c('no','yes','no'),
#' var4 = c(0,1,0)
#' )
#' get_caret_dummyvars_df(df, TRUE)
get_caret_dummyvars_df <- function(features, full_rank){
  check_features(features)
  if(!is.null(process_novar_feats(features)$novar_feats)){
    stop('Some variables have no variation. Please remove prior to running this function.')
  }
  feature_design <- caret::dummyVars(" ~ .", data = features, fullRank = full_rank)
  feature_design_mat <- stats::predict(feature_design, features) %>% 
    dplyr::as_tibble()
  return(feature_design_mat)
}
  
