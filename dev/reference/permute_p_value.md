# Calculated a permuted p-value comparing two models

Calculated a permuted p-value comparing two models

## Usage

``` r
permute_p_value(
  merged_data,
  metric,
  group_name,
  group_1,
  group_2,
  nperm = 10000
)
```

## Arguments

- merged_data:

  the concatenated performance data from `run_ml`

- metric:

  metric to compare, must be numeric

- group_name:

  column with group variables to compare

- group_1:

  name of one group to compare

- group_2:

  name of other group to compare

- nperm:

  number of permutations, default=10000

## Value

numeric p-value comparing two models

## Author

Begüm Topçuoğlu, <topcuoglu.begum@gmail.com>

Courtney R Armour, <armourc@umich.edu>

## Examples

``` r
df <- dplyr::tibble(
  model = c("rf", "rf", "glmnet", "glmnet", "svmRadial", "svmRadial"),
  AUC = c(.2, 0.3, 0.8, 0.9, 0.85, 0.95)
)
set.seed(123)
permute_p_value(df, "AUC", "model", "rf", "glmnet", nperm = 100)
#> [1] 0.3663366
```
