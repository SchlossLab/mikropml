# Author: Begum Topcuoglu
# Date: 2019-12-19
# Updated: 2020-05-06 NL
######################################################################
# Description:
# This script pulls the input data and computes a correlation matrix
######################################################################

# Usage: input file - "data/input_data.csv"
#        outcome - e.g. "dx"
#        cor_value - select correlations above or equal to cor_value
#        p_value - select correlation with value below p_value

#' Title
#'
#' @param cormat TODO
#' @param pmat TODO
#'
#' @return
#' @export
#'
#' @examples
flatten_corr_mat <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor = (cormat)[ut],
    p = pmat[ut]
  )
}

#' Title
#'
#' @param dataset TODO
#' @param outcome_colname TODO
#' @param cor_value TODO
#' @param p_value TODO
#'
#' @return
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#'
compute_corr_mat <-
  function(dataset,
           outcome_colname,
           cor_value = 1,
           p_value = 0.01) {
    # remove outcome, only keep the features
    data_corr <- dataset[, !grepl(outcome_colname, names(dataset))]

    r <- Hmisc::rcorr(as.matrix(data_corr), type = "spearman")

    adjusted <- stats::p.adjust(r$P, method = "holm")
    r$P <- adjusted

    return(
      flatten_corr_mat(r$r, r$P) %>%
        dplyr::filter(cor >= cor_value) %>%
        dplyr::filter(p < p_value)
    )
  }
