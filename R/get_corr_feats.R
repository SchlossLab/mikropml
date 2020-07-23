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
#' set.seed(0)
#' mat <- matrix(runif(100),nrow=20)
#' rownames(mat) <- 1:nrow(mat)
#' colnames(mat) <- 1:ncol(mat)
#' corr_mat <- stats::cor(mat,method = "spearman")
#' flatten_corr_mat(corr_mat)
flatten_corr_mat <- function(cormat) {
  ut <- upper.tri(cormat)
  return(data.frame(
    feature1 = rownames(cormat)[row(cormat)[ut]],
    feature2 = rownames(cormat)[col(cormat)[ut]],
    corr = (cormat)[ut]
  ))
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
#' mat = matrix(runif(100),nrow=20)
#' rownames(mat) = 1:nrow(mat)
#' colnames(mat) = 1:ncol(mat)
#' get_corr_feats(mat, 0.4)
#' @importFrom dplyr .data
get_corr_feats <- function(features, cor_value = 1) {
  return(
    features %>%
      stats::cor(method = "spearman") %>%
      flatten_corr_mat() %>%
      dplyr::filter(.data$corr >= cor_value | .data$corr <= -cor_value)
  )
}
