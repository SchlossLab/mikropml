# Author: Begum Topcuoglu
# Date: 2019-12-19
# Updated: 2020-05-06 NL
######################################################################
# Description:
# This script pulls the input data and computes a correlation matrix
######################################################################


# Usage: input file - "data/input_data.csv"
#        outcome - e.g. "dx"
#        level - name of modeling experiment
#        cor_value - select correlations above or equal to cor_value
#        p_value - select correlation with value below p_value
#' Title
#'
#' @param input_file
#' @param outcome
#' @param level
#' @param cor_value
#' @param p_value
#'
#' @return
#' @export
#' @author Begüm Topçuoğlu, \email{topcuoglu.begum@@gmail.com}
#'
#' @examples
compute_correlation_matrix <-
  function(input_file,
           outcome,
           level,
           cor_value = 1,
           p_value = 0.01) {
    # TODO: the user should read & tidy the input data; the package functions should just take dataframes and other objects
    ############### READ IN THE INPUT DATA ###############
    data_corr <- readr::read_csv(input_file)
    # remove outcome, only keep the features
    data_corr <- data_corr[, !grepl(outcome, names(data_corr))]


    ########### COMPUTE CORRELATION MATRIX ##################
    r <- Hmisc::rcorr(as.matrix(data_corr), type = "spearman")

    adjusted <- stats::p.adjust(r$P, method = "holm")
    r$P <- adjusted

    flattenCorrMatrix <- function(cormat, pmat) {
      ut <- upper.tri(cormat)
      data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  = (cormat)[ut],
        p = pmat[ut]
      )
    }

    new_r <- flattenCorrMatrix(r$r, r$P) %>%
      dplyr::filter(cor >= cor_value) %>%
      dplyr::filter(p < p_value) %>%
      readr::write_csv(paste0("data/process/sig_flat_corr_matrix_", level, ".csv"))
    # TODO: don't write csv; return df instead
  }
