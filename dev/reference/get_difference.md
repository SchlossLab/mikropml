# Calculate the difference in the mean of the metric for two groups

Calculate the difference in the mean of the metric for two groups

## Usage

``` r
get_difference(sub_data, group_name, metric)
```

## Arguments

- sub_data:

  subset of the merged performance data frame for two groups

- group_name:

  name of column with group variable

- metric:

  metric to compare

## Value

numeric difference in the average metric between the two groups

## Author

Courtney Armour, <armourc@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
df <- dplyr::tibble(
  condition = c("a", "a", "b", "b"),
  AUC = c(.2, 0.3, 0.8, 0.9)
)
get_difference(df, "condition", "AUC")
} # }
```
