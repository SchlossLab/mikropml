# Generate the tuning grid for tuning hyperparameters

Generate the tuning grid for tuning hyperparameters

## Usage

``` r
get_tuning_grid(hyperparams_list, method)
```

## Arguments

- hyperparams_list:

  Named list of lists of hyperparameters.

- method:

  ML method. Options:
  `c("glmnet", "rf", "rpart2", "svmRadial", "xgbTree")`.

  - glmnet: linear, logistic, or multiclass regression

  - rf: random forest

  - rpart2: decision tree

  - svmRadial: support vector machine

  - xgbTree: xgboost

## Value

The tuning grid.

## Author

Begüm Topçuoğlu, <topcuoglu.begum@gmail.com>

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
ml_method <- "glmnet"
hparams_list <- get_hyperparams_list(otu_small, ml_method)
get_tuning_grid(hparams_list, ml_method)
#>   lambda alpha
#> 1  1e-04     0
#> 2  1e-03     0
#> 3  1e-02     0
#> 4  1e-01     0
#> 5  1e+00     0
#> 6  1e+01     0
```
