# Check if the method is supported. If not, throws error.

Check if the method is supported. If not, throws error.

## Usage

``` r
check_method(method, hyperparameters)
```

## Arguments

- method:

  ML method. Options:
  `c("glmnet", "rf", "rpart2", "svmRadial", "xgbTree")`.

  - glmnet: linear, logistic, or multiclass regression

  - rf: random forest

  - rpart2: decision tree

  - svmRadial: support vector machine

  - xgbTree: xgboost

- hyperparameters:

  Dataframe of hyperparameters (default `NULL`; sensible defaults will
  be chosen automatically).

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
check_method("rf")
} # }
```
