# These functions compute a correlation matrix of the features and return all features that are correlated above a certain threshold.

#' Flatten correlation matrix to pairs
#'
#' @param cormat correlation matrix computed with stats::cor
#'
#' @return flattened correlation matrix (pairs of features their correlation)
#' @export
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' flatten_corr_mat(data.frame(a = runif(4, -1, 1), b = runif(4, -1, 1), c = runif(4, -1, 1)))
flatten_corr_mat <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    feature1 = rownames(cormat)[row(cormat)[ut]],
    feature2 = rownames(cormat)[col(cormat)[ut]],
    corr = (cormat)[ut]
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
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' set.seed(0)
#' get_corr_feats(data.frame(a = runif(4), b = runif(4), c = runif(4)), 0.6)
get_corr_feats <- function(features, cor_value = 1) {
  # get correlation matrix
  cormat <- stats::cor(features, method = "spearman")
  # get correlated features
  corr_feats <- flatten_corr_mat(cormat) %>%
    dplyr::filter(.data$corr >= cor_value)
  # return correlated features
  return(corr_feats)
}
