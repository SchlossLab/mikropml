
ml_results <- mikRopML::run_ml(dataset = readRDS(snakemake@input[['rds']]),
                                     method = snakemake@params[['method']],
                                     outcome_colname = 'dx',
                                     outcome_value = 'cancer',
                                     permute = TRUE,
                                     nfolds = 5,
                                     seed = snakemake@params[['seed']],
                                     ncores = snakemake@resources[['ncores']])

saveRDS(ml_results$trained_model, file = snakemake@output[['model']])
readr::write_csv(ml_results$performance, file = snakemake@output[['perf']])
readr::write_csv(ml_results$feature_importance, file = snakemake@output[['feat']])
