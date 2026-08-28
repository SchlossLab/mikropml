# Check that the outcome variable is valid. Pick outcome value if necessary.

Check that the outcome variable is valid. Pick outcome value if
necessary.

## Usage

``` r
check_outcome_value(dataset, outcome_colname)
```

## Arguments

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

## Value

outcome value

## Author

Zena Lapp, <zenalapp@umich.edu>

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
check_outcome_value(otu_small, "dx", "cancer")
} # }
```
