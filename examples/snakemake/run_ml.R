library(mikRopML)

otu_data <- readr::read_csv(snakemake@input[['csv']])
method <- snakemake@params[['method']]
seed <- snakemake@params[['seed']]

ml_results <- run_pipeline(otu_data, method, outcome_colname = 'dx',
                           outcome_value = 'cancer', permute = TRUE,
                           nfolds = 5,
                           seed = seed)

saveRDS(ml_results, file = snakemake@output[['rds']])
