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
#' mat <- matrix(runif(100), nrow = 20)
#' rownames(mat) <- 1:nrow(mat)
#' colnames(mat) <- 1:ncol(mat)
#' corr_mat <- stats::cor(mat, method = "spearman")
#' flatten_corr_mat(corr_mat)
flatten_corr_mat <- function(cormat) {
  ut <- upper.tri(cormat)
  return(data.frame(
    feature1 = rownames(cormat)[row(cormat)[ut]],
    feature2 = rownames(cormat)[col(cormat)[ut]],
    corr = cormat[ut]
  ))
}

#' Identify correlated features
#'
#' @param features features used for machine learning
#' @inheritParams run_ml
#'
#' @return correlated features
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' set.seed(0)
#' mat <- matrix(runif(100), nrow = 20)
#' rownames(mat) <- 1:nrow(mat)
#' colnames(mat) <- 1:ncol(mat)
#' get_corr_feats(mat, 0.4)
#' @importFrom dplyr .data
get_corr_feats <- function(features, corr_thresh = 1) {
  return(
    features %>%
      stats::cor(method = "spearman") %>%
      flatten_corr_mat() %>%
      dplyr::filter(.data$corr >= corr_thresh | .data$corr <= -corr_thresh)
  )
}

#' Group correlated features
#'
#' @param corr output of get_corr_feats (pairs of correlated features)
#' @param features features for ML
#'
#' @return vector of correlated features where each element is the group of correlated features separated by pipes (|)
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
group_correlated_features <- function(corr, features) {
  all_feats <- colnames(features)
  corr_feats <- unique(c(corr$feature2, corr$feature1))
  noncorr_feats <- all_feats[!all_feats %in% corr_feats]

  grps <- as.list(noncorr_feats)
  accounted_for <- rep(NA, length(all_feats))
  af_length <- sum(!is.na(accounted_for))
  c <- length(grps) + 1
  for (i in corr_feats) {
    if (i %in% accounted_for) next
    feats <- unique(c(i, corr$feature1[corr$feature2 == i], corr$feature2[corr$feature1 == i]))
    new_feats <- T
    while (new_feats) {
      len_feats <- length(feats)
      for (j in feats) {
        feats <- unique(c(feats, j, corr$feature1[corr$feature2 == j], corr$feature2[corr$feature1 == j]))
      }
      new_feats <- length(feats) > len_feats
    }
    grps[[c]] <- feats
    af_length_new <- sum(af_length, length(feats))
    accounted_for[(af_length + 1):af_length_new] <- feats
    af_length <- af_length_new
    c <- c + 1
  }
  return(sapply(grps, paste, collapse = "|"))
}
