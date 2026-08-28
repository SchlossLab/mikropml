# Split dataset into outcome and features

Split dataset into outcome and features

## Usage

``` r
split_outcome_features(dataset, outcome_colname)
```

## Arguments

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

## Value

list of length two: outcome, features (as dataframes)

## Examples

``` r
if (FALSE) { # \dontrun{
split_outcome_features(mikropml::otu_mini_bin, "dx")
} # }
```
