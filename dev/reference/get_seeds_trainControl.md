# Get seeds for `caret::trainControl()`

Adapted from [this Stack Overflow
post](https://stackoverflow.com/a/32598959) and the
[trainControl](https://rdrr.io/pkg/caret/man/trainControl.html)
documentation.

## Usage

``` r
get_seeds_trainControl(hyperparams_list, kfold, cv_times, ncol_train)
```

## Arguments

- hyperparams_list:

  Named list of lists of hyperparameters.

- kfold:

  Fold number for k-fold cross-validation (default: `5`).

- cv_times:

  Number of cross-validation partitions to create (default: `100`).

- ncol_train:

  number of columns in training data

## Value

seeds for
[`caret::trainControl()`](https://rdrr.io/pkg/caret/man/trainControl.html)

## Author

Kelly Sovacool, <sovacool@umich.edu>

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
get_seeds_trainControl(
  get_hyperparams_list(otu_small, "glmnet"),
  5, 100, 60
)
} # }
```
