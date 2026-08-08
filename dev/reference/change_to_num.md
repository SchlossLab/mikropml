# Change columns to numeric if possible

Change columns to numeric if possible

## Usage

``` r
change_to_num(features)
```

## Arguments

- features:

  dataframe of features for machine learning

## Value

dataframe with numeric columns where possible

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
class(change_to_num(data.frame(val = c("1", "2", "3")))[[1]])
} # }
```
