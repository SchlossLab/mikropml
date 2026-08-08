# Randomize feature order to eliminate any position-dependent effects

Randomize feature order to eliminate any position-dependent effects

## Usage

``` r
randomize_feature_order(dataset, outcome_colname)
```

## Arguments

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

## Value

Dataset with feature order randomized.

## Author

Nick Lesniak, <nlesniak@umich.edu>

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
dat <- data.frame(
  outcome = c("1", "2", "3"),
  a = 4:6, b = 7:9, c = 10:12, d = 13:15
)
randomize_feature_order(dat, "outcome")
#>   outcome  c b a  d
#> 1       1 10 7 4 13
#> 2       2 11 8 5 14
#> 3       3 12 9 6 15
```
