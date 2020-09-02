
ml_results <- mikRopML::run_pipeline(dataset = readRDS(snakemake@input[['rds']]),
                                     method = snakemake@params[['method']],
                                     outcome_colname = 'dx',
                                     outcome_value = 'cancer',
                                     permute = TRUE,
                                     nfolds = 5,
                                     seed = snakemake@params[['seed']],
                                     ncores = snakemake@resources[['ncores']])

saveRDS(ml_results, file = snakemake@output[['rds']])
