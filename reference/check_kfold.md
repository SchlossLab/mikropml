# Check that kfold is an integer of reasonable size

Check that kfold is an integer of reasonable size

## Usage

``` r
check_kfold(kfold, dataset)
```

## Arguments

- kfold:

  Fold number for k-fold cross-validation (default: `5`).

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
check_kfold(5, otu_small)
} # }
```
