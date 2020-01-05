# Testing the code

Test data: `test/data/small_input_data.csv`

- Has 25 samples.
- All OTUs in the original dataset and 1 column for the outcome (e.g. `dx`).
- Should take 60-150 seconds to run.

### To run the test code:

From the command line:
```
Rscript code/R/main.R --seed 1 --model L2_Logistic_Regression --data  test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx
```

Or you can provide a config file instead (see [config.yml](config.yml)):
```
Rscript code/R/main.R --configfile test/config.yml
```

Use the `--permutation` flag if you'd like to run permutation importance:
```
Rscript code/R/main.R --seed 1 --model L2_Logistic_Regression --data  test/data/small_input_data.csv --hyperparams test/data/hyperparams.csv --outcome dx --permutation
```

Or run the pipeline directly from R:
```R
source("run_model.R")
run_model(1, "L2_Logistic_Regression", "test/data/small_input_data.csv", "test/data/hyperparams.csv", "dx", permutation = FALSE)
```

If the script runs without errors, then you can check that the results are correct with:
```
diff data/temp/best_hp_results_L2_Logistic_Regression_1.csv test/data/results/best_hp_results_L2_Logistic_Regression_1.csv
```

The diff should be blank if they are exactly the same.
