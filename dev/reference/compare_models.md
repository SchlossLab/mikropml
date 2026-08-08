# Perform permutation tests to compare the performance metric across all pairs of a group variable.

A wrapper for
[`permute_p_value()`](http://www.schlosslab.org/mikropml/dev/reference/permute_p_value.md).

## Usage

``` r
compare_models(merged_data, metric, group_name, nperm = 10000)
```

## Arguments

- merged_data:

  the concatenated performance data from `run_ml`

- metric:

  metric to compare, must be numeric

- group_name:

  column with group variables to compare

- nperm:

  number of permutations, default=10000

## Value

a table of p-values for all pairs of group variable

## Author

Courtney R Armour, <armourc@umich.edu>

## Examples

``` r
df <- dplyr::tibble(
  model = c("rf", "rf", "glmnet", "glmnet", "svmRadial", "svmRadial"),
  AUC = c(.2, 0.3, 0.8, 0.9, 0.85, 0.95)
)
set.seed(123)
compare_models(df, "AUC", "model", nperm = 10)
#>   group1    group2   p_value
#> 1 glmnet svmRadial 0.7272727
#> 2     rf    glmnet 0.2727273
#> 3     rf svmRadial 0.5454545
```
