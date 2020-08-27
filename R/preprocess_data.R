
#' Preprocess data prior to running machine learning
#'
#'
#' @param dataset dataframe where rows are samples and colums are the outcome variable and features
#' @param outcome_colname column name as a string of the outcome variable
#' @param method methods to preprocess the data, described in `caret::preProcess` (defaut: `c("center","scale")`, use `NULL` for no normalization)
#' @param rm_nzv whether to remove variables with near-zero variance (default: `TRUE`)
#' @param rm_corr_feats whether to keep only one of perfectly correlated featurse
#'
#' @return preprocessed data
#' @export
#'
#' @examples
#' preprocess_data(mikRopML::otu_small, "dx")
preprocess_data <- function(dataset, outcome_colname, method = c("center", "scale"), rm_nzv = TRUE, rm_corr_feats = TRUE) {

  # input validation
  check_dataset(dataset)
  check_outcome_column(dataset, outcome_colname)
  
  # remove outcomes that are NA
  dataset <- rm_missing_outcome(dataset, outcome_colname)

  # if rm_corr_feats is TRUE, rm_nzv must also be TRUE (error otherwise)
  if (rm_corr_feats & !rm_nzv) {
    stop("`rm_nzv` must be true if `rm_corr_feats` is true. If you would like to group features based on correlation, please re-run this function with `rm_nzv` = TRUE")
  }

  # get outcome and features
  split_dat <- split_outcome_features(dataset, outcome_colname)
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
  if (!is.null(nonbin_feats_transformed)) {
    nonbin_feats_transformed <- process_nonbin_feats(nonbin_feats, method)
  }

  # combine all features
  processed_feats <- dplyr::bind_cols(nonbin_feats_transformed, bin_feats, novar_feats)

  # remove features with non-zero variance
  if (rm_nzv) processed_feats <- get_caret_processed_df(processed_feats, "nzv")

  # remove perfectly correlated features
  grp_feats <- NULL
  if (rm_corr_feats) {
    feats_and_grps <- rm_corr_feats(processed_feats)
    processed_feats <- feats_and_grps$features
    grp_feats <- feats_and_grps$grp_feats
  }

  # combine outcome and features
  dat_transformed <- dplyr::bind_cols(outcome, processed_feats) %>% dplyr::as_tibble()

  return(list(dat_transformed = dat_transformed, grp_feats = grp_feats))
}

#' Remove missing outcome values
#'
#' @inheritParams run_ml
#'
#' @return dataset with no missing outcomes
#' @export
#'
#' @examples 
#' rm_missing_outcome(mikRopML::otu_mini, "dx")
#' 
#' test_df <- mikRopML::otu_mini
#' test_df[1:100,'dx'] = NA
#' rm_missing_outcome(test_df, "dx")
rm_missing_outcome <- function(dataset, outcome_colname){
  n_outcome_na <- sum(is.na(dataset[,outcome_colname]))
  total_outcomes <- nrow(dataset)
  perc_na <- round(n_outcome_na/total_outcomes*100, 2)
  dataset <- dataset %>% dplyr::filter(!is.na(!!(dplyr::sym(outcome_colname))))
  if(n_outcome_na != 0){
    message(paste0('Removed ', n_outcome_na, '/', total_outcomes, ' (',  perc_na, '%) of samples because of missing outcome value (NA).'))
  }
  return(dataset)
}

#' Process features with no variation
#'
#' @param features dataframe of features for machine learning
#'
#' @return list of two dataframes: features with variability (unprocessed) and without (processed)
#' @export
#'
#' @examples
#' process_novar_feats(mikRopML::otu_small[, 2:ncol(otu_small)])
process_novar_feats <- function(features) {
  check_features(features)

  # get features with no variation
  novar_feats_bool <- apply(features, 2, function(x) length(unique(x)) == 1)
  novar_feats <- features %>% dplyr::select_if(novar_feats_bool)

  # change categorical features with no variation to zero
  novar_feats_mat <- sapply(novar_feats, function(x) {
    if (class(x) %in% c("factor", "character")) {
      rep(0, length(x))
    } else {
      x
    }
  }) %>% dplyr::as_tibble()

  if (ncol(novar_feats_mat) == 0) novar_feats_mat <- NULL

  # get features with variation
  var_feats <- features %>%
    dplyr::select_if(!novar_feats_bool) %>%
    dplyr::as_tibble()

  if (ncol(var_feats) == 0) stop("All features have zero variance.")

  return(list(novar_feats = novar_feats_mat, var_feats = var_feats))
}

#' Process binary features
#'
#' @param features dataframe of features for machine learning
#'
#' @return list of two dataframes: binary (processed) and nonbinary features (unprocessed)
#' @export
#'
#' @examples
#' process_bin_feats(mikRopML::otu_small[, 2:ncol(otu_small)])
process_bin_feats <- function(features) {
  check_features(features)

  bin_feats_bool <- apply(features, 2, function(x) length(unique(x)) == 2)
  bin_feats <- features %>% dplyr::select_if(bin_feats_bool)
  nonbin_feats <- features %>%
    dplyr::select_if(!bin_feats_bool) %>%
    dplyr::as_tibble()
  if (ncol(nonbin_feats) == 0) nonbin_feats <- NULL


  feature_design_bin_mat <- NULL
  if (ncol(bin_feats) != 0) {
    # change categorical binary variables to 0 and 1 (full rank, i.e. only 1 column for each variable)
    feature_design_bin_mat <- get_caret_dummyvars_df(bin_feats, full_rank = TRUE)
  }

  return(list(bin_feats = feature_design_bin_mat, nonbin_feats = nonbin_feats))
}

#' Preprocess nonbinary features
#'
#' @param features dataframe of features for machine learning
#' @param method methods to preprocess the data, described in `caret::preProcess` (defaut: `c("center","scale")`)
#'
#' @return dataframe of preprocessed features
#' @export
#'
#' @examples
#' process_nonbin_feats(mikRopML::otu_small[, 2:ncol(otu_small)], c("center", "scale", "nzv"))
process_nonbin_feats <- function(features, method) {
  check_features(features)

  feature_design_nonbin_mat <- NULL

  if (ncol(features) != 0) {
    # types of features present
    class_feats <- sapply(features, function(x) class(x))
    cont_feats <- sum(class_feats %in% c("integer", "numeric"))
    cat_feats <- sum(class_feats %in% c("character", "factor"))

    # transform continuous features
    transformed_nonbin <- features
    if (cont_feats > 0 & !is.null(method)) {
      transformed_nonbin <- get_caret_processed_df(features, method)
    }

    # change categorical non-binary features to 0 and 1 (not full rank, i.e. one column for each unique element in the column)

    feature_design_nonbin_mat <- transformed_nonbin
    if (cat_feats > 0) {
      feature_design_nonbin_mat <- get_caret_dummyvars_df(transformed_nonbin, full_rank = FALSE)
    }
  }

  return(feature_design_nonbin_mat)
}

#' Get preprocessed dataframe for continuous variables
#'
#' @inheritParams process_nonbin_feats
#'
#' @return processed matrix
#' @export
#'
#' @examples
#' get_caret_processed_df(mikRopML::otu_small[, 2:ncol(otu_small)], c("center", "scale"))
get_caret_processed_df <- function(features, method) {
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
#'   outcome = c("normal", "normal", "cancer"),
#'   var1 = 1:3,
#'   var2 = c("a", "b", "c"),
#'   var3 = c("no", "yes", "no"),
#'   var4 = c(0, 1, 0)
#' )
#' get_caret_dummyvars_df(df, TRUE)
get_caret_dummyvars_df <- function(features, full_rank) {
  check_features(features)
  if (!is.null(process_novar_feats(features)$novar_feats)) {
    stop("Some variables have no variation. Please remove prior to running this function.")
  }
  feature_design <- caret::dummyVars(" ~ .", data = features, fullRank = full_rank)
  feature_design_mat <- stats::predict(feature_design, features) %>%
    dplyr::as_tibble()
  return(feature_design_mat)
}


#' Remove correlated features
#'
#' @param features features for ML
#'
#' @return features where perfectly correlated ones are collapsed
#' @export
#'
#' @examples
#' rm_corr_feats(mikRopML::otu_small[, 2:ncol(otu_small)])
rm_corr_feats <- function(features) {
  if (any(sapply(features, class) %in% c("character", "factor"))) {
    stop("Some features are charactors or factors. Please remove these before proceeding with `rm_corr_feats`.")
  }
  if (!is.null(process_novar_feats(features)$novar_feats)) {
    stop("Some features have no variation. Please remove these before proceeding with `rm_corr_feats`.")
  }
  if (ncol(features) == 1) {
    output <- list(features = features, grp_feats = NULL)
  } else {
    corr_feats <- group_correlated_features(get_corr_feats(features), features)
    corr_mat <- stats::cor(features)
    corr_cols <- caret::findCorrelation(corr_mat, cutoff = 1 - 10e-15)
    feats_nocorr <- features %>% dplyr::select(-dplyr::all_of(corr_cols))
    names_grps <- sapply(names(feats_nocorr), function(n) {
      not_corr <- n %in% corr_feats
      if (not_corr) {
        name <- n
      } else {
        name <- corr_feats[grep(paste0("^", n, "\\||\\|", n, "\\||\\|", n, "$"), corr_feats)]
      }
    })
    grp_cols <- grep("\\|", names_grps)
    num_grps <- length(grp_cols)
    if (num_grps == 0) {
      output <- list(features = feats_nocorr, grp_feats = NULL)
    } else {
      names(names_grps)[grp_cols] <- paste0("grp", 1:num_grps)
      names(feats_nocorr) <- names(names_grps)
      grp_feats <- sapply(names_grps, function(x) strsplit(x, split = "\\|"))
      output <- list(features = feats_nocorr, grp_feats = grp_feats)
    }
  }
  return(output)
}
