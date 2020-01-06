# Author: Begum Topcuoglu
# Date: 2019-12-19
######################################################################
# Description:
# This script pulls the input data and computes a correlation matrix
######################################################################

# Usage: input file - "data/input_data.csv"
#        outcome - e.g. "dx"

compute_correlation_matrix <- function(input_file, outcome){

    ############### READ IN THE INPUT DATA ###############
    data_corr <- read.csv(input_file) %>%
        select(-outcome) # remove outcome, only keep the OTUs
    #######################################################



    ########### COMPUTE CORRELATION MATRIX ##################
    r <- rcorr(as.matrix(data_corr), type="spearman")

    adjusted <- p.adjust(r$P, method = "holm")
    r$P <- adjusted

    flattenCorrMatrix <- function(cormat, pmat) {
      ut <- upper.tri(cormat)
      data.frame(
        row = rownames(cormat)[row(cormat)[ut]],
        column = rownames(cormat)[col(cormat)[ut]],
        cor  =(cormat)[ut],
        p = pmat[ut]
      )
    }

    new_r <- flattenCorrMatrix(r$r, r$P) %>%
      filter(cor==1) %>%
      filter(p<0.01) %>%
      write_csv("data/process/sig_flat_corr_matrix.csv")
    ##########################################################
}
