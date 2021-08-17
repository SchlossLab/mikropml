
#' Identify correlated features
#'
#' @param features a dataframe with each column as a feature for ML
#' @param corr_method correlation method. options or the same as those supported
#'   by `stats::cor`: spearman, pearson, kendall. (default: spearman)
#' @param group_neg_corr Whether to group negatively correlated features
#'   together (e.g. c(0,1) and c(1,0)).
#' @inheritParams run_ml
#'
#' @return Dataframe of correlated features where the columns are feature1,
#'   feature2, and the correlation between those two features
#'   (anything exceeding corr_thresh).
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
get_corr_feats <- function(features, corr_thresh = 1, group_neg_corr = TRUE,
                           corr_method = "spearman") {
  corr_feats <- features %>%
    stats::cor(method = corr_method) %>%
    flatten_corr_mat()
  if (group_neg_corr) {
    corr_feats <- corr_feats %>%
      dplyr::filter(.data$corr >= corr_thresh | .data$corr <= -corr_thresh)
  } else {
    corr_feats <- corr_feats %>%
      dplyr::filter(.data$corr >= corr_thresh)
  }
  return(corr_feats)
}

#' Flatten correlation matrix to pairs
#'
#' @param cormat correlation matrix computed with stats::cor
#'
#' @return flattened correlation matrix (pairs of features their correlation)
#' @noRd
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

#' Identify correlated features as a binary matrix
#'
#' @inheritParams run_ml
#' @inheritParams get_corr_feats
#'
#' @return A binary matrix of correlated features
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' features <- data.frame(a = 1:3, b = 2:4, c = c(1,0,1),
#'                        d = (5:7), e = c(5,1,4))
#' get_bin_corr_mat(features)
get_binary_corr_mat <- function(features, corr_thresh = 1, group_neg_corr = TRUE,
                             corr_method = "spearman") {
  corr_mat <- features %>%
    stats::cor(method = corr_method)
  if (group_neg_corr) {
    in_thresh <- corr_mat >= corr_thresh | corr_mat <= -corr_thresh
  } else {
    in_thresh <- corr_mat >= corr_thresh
  }
  bin_mat <- corr_mat
  bin_mat[in_thresh] <- 1
  bin_mat[!in_thresh] <- 0
  return(bin_mat)
}

#' Group correlated features
#'
#' @inheritParams run_ml
#' @inheritParams get_corr_feats
#' @return vector of where each element is a group of
#'   correlated features separated by pipes (`|`)
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#' @examples
#' features <- data.frame(a = 1:3, b = 2:4, c = c(1,0,1),
#'                        d = (5:7), e = c(5,1,4), f = c(-1,0,-1))
#' group_correlated_features(features)
group_correlated_features <- function(features, corr_thresh = 1,
                                      group_neg_corr = TRUE, corr_method = "spearman") {
  bin_corr_mat <- get_binary_corr_mat(features,
                                      corr_thresh = corr_thresh,
                                      group_neg_corr = group_neg_corr,
                                      corr_method = corr_method
                                      )
  dist_mat <- 1 - bin_corr_mat %>% stats::as.dist()
  tree <- stats::hclust(dist_mat, method = 'single')
  cluster_ids <- cutree(tree, h = 0)
  num_clusters <- max(cluster_ids)
  feat_groups <- character(num_clusters)
  for (feat in names(cluster_ids)) {
    cluster_id <- cluster_ids[[feat]]
    current_cluster <- feat_groups[cluster_id]
    if (nchar(current_cluster) > 0) {
      new_cluster <- paste(c(current_cluster, feat), collapse = '|')
    } else {
      new_cluster <- feat
    }
    feat_groups[cluster_id] <- new_cluster
  }
  return(feat_groups)
}

#' Group correlated features
#'
#' @inheritParams run_ml
#' @inheritParams get_corr_feats
#' @return vector of where each element is a group of
#'   correlated features separated by pipes (`|`)
#' @noRd
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
group_correlated_features_OLD <- function(features, corr_thresh = 1,
                                      group_neg_corr = TRUE, corr_method = "spearman") {
  corr <- get_corr_feats(features,
    corr_thresh = corr_thresh,
    group_neg_corr = group_neg_corr,
    corr_method = corr_method
  )
  corr <- dplyr::select_if(corr, !(names(corr) %in% c("corr")))

  all_feats <- colnames(features)
  corr_feats <- unique(c(corr$feature2, corr$feature1))
  noncorr_feats <- all_feats[!all_feats %in% corr_feats]

  grps <- as.list(noncorr_feats)
  accounted_for <- rep(NA, length(all_feats))
  af_length <- sum(!is.na(accounted_for))
  c <- length(grps) + 1
  for (i in corr_feats) {
    if (i %in% accounted_for) next
    feats <- unique(c(
      i, corr$feature1[corr$feature2 == i],
      corr$feature2[corr$feature1 == i]
    ))
    new_feats <- TRUE
    while (new_feats) {
      len_feats <- length(feats)
      for (j in feats) {
        feats <- unique(c(
          feats, j, corr$feature1[corr$feature2 == j],
          corr$feature2[corr$feature1 == j]
        ))
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
