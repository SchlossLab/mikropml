# Author: Begum Topcuoglu
# Date: 2019-12-19
######################################################################
# Description:
# This script prepare the dataset to have outcome and the features
######################################################################



################### IMPORT LIBRARIES and FUNCTIONS ###################
# The dependinces for this script are consolidated in the first part
deps = c("tidyverse", "dplyr", "Hmisc", "RcmdrMisc");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE, repos = "http://cran.us.r-project.org", dependencies=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}
source("code/R/compute_correlation_matrix.R")
######################################################################

######################## DATA PREPARATION #############################
# Features: Hemoglobin levels(FIT) and 16S rRNA gene sequences(OTUs) in the stool
# Labels: - Colorectal lesions of 490 patients.
#         - Defined as cancer or not.(Cancer here means: SRN)
#                                     SRNs are adv adenomas+carcinomas

# Read in metadata and select only sample Id and diagnosis columns
metadata <- read.delim('test/data/metadata.tsv', header=T, sep='\t') %>%
  select(sample, Dx_Bin, fit_result)
# Read in OTU table and remove label and numOtus columns
shared <- read.delim('test/data/baxter.0.03.subsample.shared', header=T, sep='\t') %>%
  select(-label, -numOtus)
# Merge metadata and OTU table.
# Group advanced adenomas and cancers together as cancer and normal, high risk normal and non-advanced adenomas as normal
# Then remove the sample ID column
data <- inner_join(metadata, shared, by=c("sample"="Group")) %>%
  mutate(dx = case_when(
    Dx_Bin== "Adenoma" ~ "normal",
    Dx_Bin== "Normal" ~ "normal",
    Dx_Bin== "High Risk Normal" ~ "normal",
    Dx_Bin== "adv Adenoma" ~ "cancer",
    Dx_Bin== "Cancer" ~ "cancer"
  )) %>%
  select(-sample, -Dx_Bin, -fit_result) %>%
  drop_na() %>%
  select(dx, everything()) %>%
  write_csv("test/data/input_data.csv")


#################### CORRELATION MATRIX PREPARATION ########################
# We make a spearman rank correlation matrix for all the OTUs pairwase
# We then extract the perfecly and significant (p<0.01) correlated (coefficient=1) OTUs and create a file in data/process

compute_correlation_matrix("test/data/input_data.csv", "dx")
