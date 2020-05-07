# Author: Begum Topcuoglu
# Date: 2019-12-19
# Updated: 2020-05-06 NL
######################################################################
# Description:
# This script pulls the input data and computes a correlation matrix
######################################################################


################### IMPORT LIBRARIES and FUNCTIONS ###################
# The dependinces for this script are consolidated in the first part
deps = c("Hmisc", "dplyr", "readr");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE, repos = "http://cran.us.r-project.org", dependencies=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}
######################################################################

# Usage: input file - "data/input_data.csv"
#        outcome - e.g. "dx"
#        level - name of modeling experiment
#        cor_value - select correlations above or equal to cor_value
#        p_value - select correlation with value below p_value
compute_correlation_matrix <- function(input_file, outcome, level, cor_value = 1, p_value = 0.01){

    ############### READ IN THE INPUT DATA ###############
    data_corr <- read_csv(input_file)
    # remove outcome, only keep the features
    data_corr <- data_corr[,!grepl(outcome, names(data_corr))]
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
      filter(cor>=cor_value) %>%
      filter(p<p_value) %>%
      write_csv(paste0("data/process/sig_flat_corr_matrix_", level, ".csv"))
    ##########################################################
}
