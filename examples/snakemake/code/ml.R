source('code/log_smk.R')
method <- snakemake@params[['method']]
seed <- as.numeric(snakemake@params[['seed']])
data_processed <- readRDS(snakemake@input[['rds']])$dat_transformed
ml_results <- mikRopML::run_ml(dataset = data_processed,
                               method = method,
                               outcome_colname = 'dx',
                               outcome_value = 'cancer',
                               find_feature_importance = TRUE,
                               kfold = 5,
                               seed = seed,
                               ncores = as.numeric(snakemake@resources[['ncores']])
                               )

saveRDS(ml_results$trained_model, file = snakemake@output[['model']])
readr::write_csv(ml_results$performance, file = snakemake@output[['perf']])
readr::write_csv(ml_results$feature_importance, file = snakemake@output[['feat']])
