# Set hyperparameters based on ML method and dataset characteristics

For more details see the vignette on [hyperparameter
tuning](http://www.schlosslab.org/mikropml/articles/tuning.md).

## Usage

``` r
get_hyperparams_list(dataset, method)
```

## Arguments

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

- method:

  ML method. Options:
  `c("glmnet", "rf", "rpart2", "svmRadial", "xgbTree")`.

  - glmnet: linear, logistic, or multiclass regression

  - rf: random forest

  - rpart2: decision tree

  - svmRadial: support vector machine

  - xgbTree: xgboost

## Value

Named list of hyperparameters.

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
get_hyperparams_list(otu_mini_bin, "rf")
#> $mtry
#> [1] 2 3 6
#> 
get_hyperparams_list(otu_small, "rf")
#> $mtry
#> [1]  4  8 16
#> 
get_hyperparams_list(otu_mini_bin, "rpart2")
#> $maxdepth
#> [1]  1  2  4  8 16 30
#> 
get_hyperparams_list(otu_small, "rpart2")
#> $maxdepth
#> [1]  1  2  4  8 16 30
#> 
```
