############################################################
# Author: Joshua MA Stough
############################################################

### Allows for this script to take arguments
args <- commandArgs(trailingOnly = TRUE)

### Install R package dependencies if they are not installed
### and load them in.
deps = c("tidyverse", "dplyr", "Hmisc", "RcmdrMisc");
for (dep in deps){
     if (dep %in% installed.packages()[,"Package"] == FALSE){
          install.packages(as.character(dep), quiet=TRUE, repos = "http://cran.us.r-project.org", dependencies=TRUE);
     }
     library(dep, verbose=FALSE, character.only=TRUE)
}
source("code/R/compute_correlation_matrix.R")


input_file = args[1]
outcome = args[2]

compute_correlation_matrix(input_file, outcome)