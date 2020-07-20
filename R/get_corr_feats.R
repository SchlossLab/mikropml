# Author: Begum Topcuoglu
# Date: 2019-12-19
# Updated: 2020-05-06 NL
######################################################################
# Description:
# These functions compute a correlation matrix of the features and return all features that are correlated above a certain threshold.
######################################################################

#' Title
#'
#' @param cormat correlation matrix computed with stats::cor
#'
#' @return flattened correlation matrix (pairs of features their correlation)
#' @export
#'
#' @examples
flatten_corr_mat <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor = (cormat)[ut],
  )
}

#' Identify correlated features
#'
#' @param features features used for machine learning
#' @param cor_value return correlations above or equal to cor_value (default: 1)
#'
#' @return correlated features
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
get_corr_feats <- function(features, cor_value = 1) {
  # get correlation matrix
  cormat <- stats::cor(features, method = "spearman")
  # get correlated features
  corr_feats <- flatten_corr_mat(cormat) %>%
    dplyr::filter(cor >= cor_value)
  # return correlated features
  return(corr_feats)
  }
