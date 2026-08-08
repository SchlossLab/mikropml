# Check the validity of the training indices

Check the validity of the training indices

## Usage

``` r
check_training_indices(training_inds, dataset)
```

## Arguments

- training_inds:

  vector of integers corresponding to samples for the training set

- dataset:

  data frame containing the entire dataset

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
training_indices <- otu_small %>%
  nrow() %>%
  sample(., size = 160)
check_training_indices(training_indices, otu_small)
} # }
```
