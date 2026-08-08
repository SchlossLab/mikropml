# check that corr_thresh is either NULL or a number between 0 and 1

check that corr_thresh is either NULL or a number between 0 and 1

## Usage

``` r
check_corr_thresh(corr_thresh)
```

## Arguments

- corr_thresh:

  correlation threshold

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
check_corr_thresh(1)
check_corr_thresh(0.8)
check_corr_thresh(2019)
check_corr_thresh(NULL)
} # }
```
