configfile: 'config/config.yml'

ncores = config['ncores']
ml_methods = config['ml_methods']
seeds = range(config['seeds']['start'], config['seeds']['stop'])

rule targets:
    input:
        expand("results/runs/{method}_{seed}_model.Rds",
                method = ml_methods,
                seed = seeds)
#        expand("results/{type}_results.csv",
#                type = ['performance', 'feature-importance'])

rule preprocess_data:
    input:
        R="code/preproc.R",
        csv="data/raw/otu_medium.csv"
    output:
        rds='data/processed/otu_medium.Rds'
    log:
        "log/preprocess_data.txt"
    benchmark:
        "benchmarks/preprocess_data.txt"
    script:
        "code/preproc.R"

rule run_ml:
    input:
        R="code/ml.R",
        rds=rules.preprocess_data.output.rds
    output:
        model="results/runs/{method}_{seed}_model.Rds",
        perf="results/runs/{method}_{seed}_performance.csv",
        feat="results/runs/{method}_{seed}_feature-importance.csv"
    log:
        "log/runs/run_ml.{method}_{seed}.txt"
    benchmark:
        "benchmarks/runs/run_ml.{method}_{seed}.txt"
    params:
        method="{method}",
        seed="{seed}"
    resources:
        ncores=ncores,
        pmem_gb=2
    script:
        "code/ml.R"

rule merge_results:
    input:
        R="code/merge_results.R",
        csv=expand("results/runs/{method}_{seed}_{{type}}.csv", method = ml_methods, seed = seeds)
    output:
        csv='results/{type}_results.csv'
    log:
        "log/merge_results_{type}.txt"
    benchmark:
        "benchmarks/merge_results_{type}.txt"
    script:
        "code/merge_results.R"


# TODO; plots, report, etc.
