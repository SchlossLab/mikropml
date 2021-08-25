#' Group correlated features
#'
#' @inheritParams run_ml
#' @param features a dataframe with each column as a feature for ML
#' @param corr_method correlation method. options or the same as those supported
#'   by `stats::cor`: spearman, pearson, kendall. (default: spearman)
#' @param group_neg_corr Whether to group negatively correlated features
#'   together (e.g. c(0,1) and c(1,0)).
#'
#' @return vector where each element is a group of correlated features
#'   separated by pipes (`|`)
#'
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#' @examples
#' features <- data.frame(
#'   a = 1:3, b = 2:4, c = c(1, 0, 1),
#'   d = (5:7), e = c(5, 1, 4), f = c(-1, 0, -1)
#' )
#' group_correlated_features(features)
group_correlated_features <- function(features, corr_thresh = 1,
                                      group_neg_corr = TRUE, corr_method = "spearman") {
  bin_corr_mat <- get_binary_corr_mat(features,
    corr_thresh = corr_thresh,
    group_neg_corr = group_neg_corr,
    corr_method = corr_method
  )
  # get single linkage clusters at height zero
  cluster_ids <- cluster_corr_mat(bin_corr_mat)
  return(get_groups_from_clusters(cluster_ids))
}

#' Identify correlated features as a binary matrix
#'
#' @inheritParams run_ml
#' @inheritParams group_correlated_features
#'
#' @return A binary matrix of correlated features
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' features <- data.frame(
#'   a = 1:3, b = 2:4, c = c(1, 0, 1),
#'   d = (5:7), e = c(5, 1, 4)
#' )
#' get_binary_corr_mat(features)
get_binary_corr_mat <- function(features, corr_thresh = 1, group_neg_corr = TRUE,
                                corr_method = "spearman") {
  corr_mat <- features %>%
    stats::cor(method = corr_method)
  corr_mat[is.na(corr_mat)] <- 0 # switch NAs to zero
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

#' Cluster a matrix of correlated features
#'
#' @param bin_corr_mat a binary correlation matrix created by `get_binary_corr_mat()`.
#' @param hclust_method the `method` to use in `stats::hclust()` [default: 'single'].
#' @param cut_height the cut height (`h`) to use in `stats::cutree()` [default: 0].
#'
#' @return a named vector from `stats::cutree()`. Each element is a cluster and
#'   the name is a feature in that cluster.
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#' @author Pat Schloss, \email{pschloss@@umich.edu}
#'
#' @examples
#' corr_mat <- matrix(
#'   data = c(1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1),
#'   nrow = 4,
#'   dimnames = list(
#'     c("a", "b", "c", "d"),
#'     c("a", "b", "c", "d")
#'   )
#' )
#' corr_mat
#' cluster_corr_mat(corr_mat)
cluster_corr_mat <- function(bin_corr_mat,
                             hclust_method = "single",
                             cut_height = 0) {
  dist_mat <- 1 - bin_corr_mat %>% stats::as.dist()
  if (identical(dist_mat, numeric(0))) {
    stop("The correlation matrix contains nothing. Hint: is the features data frame empty?")
  }
  return(stats::cutree(stats::hclust(dist_mat,
    method = hclust_method
  ),
  h = cut_height
  ))
}

#' Assign features to groups
#'
#' @param cluster_ids named vector created by `cluster_corr_mat()`.
#'   Each element is a cluster and the name is a feature in that cluster.
#' @return a vector where each element is a group of correlated features
#'   separated by pipes (`|`)
#'
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' corr_mat <- matrix(
#'   data = c(1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1),
#'   nrow = 4,
#'   dimnames = list(
#'     c("a", "b", "c", "d"),
#'     c("a", "b", "c", "d")
#'   )
#' )
#' corr_mat
#' get_groups_from_clusters(cluster_corr_mat(corr_mat))
get_groups_from_clusters <- function(cluster_ids) {
  feat_groups <- character(length = max(cluster_ids))
  for (feat in radix_sort(names(cluster_ids))) { # assign each feature to its group/cluster
    cluster_id <- cluster_ids[[feat]]
    current_cluster <- feat_groups[cluster_id]
    if (nchar(current_cluster) > 0) {
      new_cluster <- paste(c(current_cluster, feat), collapse = "|")
    } else { # no need for paste if the current cluster has nothing in it
      new_cluster <- feat
    }
    feat_groups[cluster_id] <- new_cluster
  }
  return(radix_sort(feat_groups))
}


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
#' @noRd
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @importFrom dplyr .data
get_corr_feats <- function(features, corr_thresh = 1, group_neg_corr = TRUE,
                           corr_method = "spearman") {
  .Deprecated("get_binary_corr_mat")
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
flatten_corr_mat <- function(cormat) {
  .Deprecated("get_binary_corr_mat")
  ut <- upper.tri(cormat)
  return(data.frame(
    feature1 = rownames(cormat)[row(cormat)[ut]],
    feature2 = rownames(cormat)[col(cormat)[ut]],
    corr = cormat[ut]
  ))
}
