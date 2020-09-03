source('code/log_smk.R')
library(dplyr)
otu_data <- readr::read_csv(snakemake@input[['csv']]) %>%
    mikRopML::preprocess_data(outcome_colname = 'dx')
saveRDS(otu_data, file = snakemake@output[['rds']])
