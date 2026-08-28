# Check that the dataset is not empty and has more than 1 column.

Errors if there are no rows or fewer than 2 columns.

## Usage

``` r
check_dataset(dataset)
```

## Arguments

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
check_dataset(otu_small)
} # }
```
