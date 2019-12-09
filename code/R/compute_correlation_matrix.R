################### IMPORT LIBRARIES and FUNCTIONS ###################
# The dependinces for this script are consolidated in the first part
deps = c("dplyr", "tictoc", "caret" ,"rpart", "xgboost", "randomForest", "kernlab","LiblineaR", "pROC", "tidyverse");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE, repos = "http://cran.us.r-project.org", dependencies=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}
######################################################################

######################## DATA PREPARATION #############################
# Features: Hemoglobin levels(FIT) and 16S rRNA gene sequences(OTUs) in the stool 
# Labels: - Colorectal lesions of 490 patients. 
#         - Defined as cancer or not.(Cancer here means: SRN)
#                                     SRNs are adv adenomas+carcinomas

# Read in metadata and select only sample Id and diagnosis columns
meta_corr <- read.delim('data/metadata.tsv', header=T, sep='\t') %>%
  select(sample, Dx_Bin, fit_result)
# Read in OTU table and remove label and numOtus columns
shared_corr <- read.delim('data/baxter.0.03.subsample.shared', header=T, sep='\t') %>%
  select(-label, -numOtus)
# Merge metadata and OTU table.
# Group advanced adenomas and cancers together as cancer and normal, high risk normal and non-advanced adenomas as normal
# Then remove the sample ID column
data_corr <- inner_join(meta_corr, shared_corr, by=c("sample"="Group")) %>%
  mutate(dx = case_when(
    Dx_Bin== "Adenoma" ~ "normal",
    Dx_Bin== "Normal" ~ "normal",
    Dx_Bin== "High Risk Normal" ~ "normal",
    Dx_Bin== "adv Adenoma" ~ "cancer",
    Dx_Bin== "Cancer" ~ "cancer"
  )) %>%
  select(-sample, -Dx_Bin, -fit_result, -dx) %>%
  drop_na()

library(Hmisc)
library(RcmdrMisc)

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



