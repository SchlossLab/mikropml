# Remove missing outcome values

Remove missing outcome values

## Usage

``` r
rm_missing_outcome(dataset, outcome_colname)
```

## Arguments

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

## Value

dataset with no missing outcomes

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
rm_missing_outcome(mikropml::otu_mini_bin, "dx")

test_df <- mikropml::otu_mini_bin
test_df[1:100, "dx"] <- NA
rm_missing_outcome(test_df, "dx")
} # }
```
