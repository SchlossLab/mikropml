
otu_data <- readRDS(snakemake@input[['rds']])
method <- snakemake@params[['method']]
seed <- snakemake@params[['seed']]

ml_results <- mikRopML::run_pipeline(otu_data, method, outcome_colname = 'dx',
                                     outcome_value = 'cancer', permute = TRUE,
                                     nfolds = 5, seed = seed)

saveRDS(ml_results, file = snakemake@output[['rds']])
