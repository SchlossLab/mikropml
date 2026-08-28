# Check that outcome column exists. Pick outcome column if not specified.

Check that outcome column exists. Pick outcome column if not specified.

## Usage

``` r
check_outcome_column(
  dataset,
  outcome_colname,
  check_values = TRUE,
  show_message = TRUE
)
```

## Arguments

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

- check_values:

  whether to check the outcome values or just get the column
  (default:TRUE)

- show_message:

  whether to show which column is being used as the output column
  (default: TRUE)

## Value

outcome colname

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
check_outcome_column(otu_small, NULL)
check_outcome_column(otu_small, "dx")
} # }
```
