# Calculate a bootstrap confidence interval for the performance on a single train/test split

Uses
[`rsample::bootstraps()`](https://rsample.tidymodels.org/reference/bootstraps.html),
[`rsample::int_pctl()`](https://rsample.tidymodels.org/reference/int_pctl.html),
and
[`furrr::future_map()`](https://furrr.futureverse.org/reference/future_map.html)

## Usage

``` r
bootstrap_performance(
  ml_result,
  outcome_colname,
  bootstrap_times = 10000,
  alpha = 0.05
)
```

## Arguments

- ml_result:

  result returned from a single
  [`run_ml()`](http://www.schlosslab.org/mikropml/dev/reference/run_ml.md)
  call

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

- bootstrap_times:

  the number of boostraps to create (default: `10000`)

- alpha:

  the alpha level for the confidence interval (default `0.05` to create
  a 95% confidence interval)

## Value

a data frame with an estimate (`.estimate`), lower bound (`.lower`), and
upper bound (`.upper`) for each performance metric (`term`).

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
bootstrap_performance(otu_mini_bin_results_glmnet, "dx",
  bootstrap_times = 10, alpha = 0.10
)
#> Warning: Recommend at least 1000 non-missing bootstrap resamples for terms `AUC`,
#> `Accuracy`, `Balanced_Accuracy`, `Detection_Rate`, `F1`, `Kappa`,
#> `Neg_Pred_Value`, `Pos_Pred_Value`, `Precision`, `Recall`, `Sensitivity`,
#> `Specificity`, `cv_metric_AUC`, `logLoss`, and `prAUC`.
#> # A tibble: 15 × 6
#>    term              .lower .estimate .upper .alpha .method   
#>    <chr>              <dbl>     <dbl>  <dbl>  <dbl> <chr>     
#>  1 AUC                0.437     0.612  0.761    0.1 percentile
#>  2 Accuracy           0.394     0.567  0.721    0.1 percentile
#>  3 Balanced_Accuracy  0.384     0.563  0.719    0.1 percentile
#>  4 Detection_Rate     0.179     0.277  0.373    0.1 percentile
#>  5 F1                 0.416     0.558  0.706    0.1 percentile
#>  6 Kappa             -0.226     0.126  0.438    0.1 percentile
#>  7 Neg_Pred_Value     0.325     0.559  0.741    0.1 percentile
#>  8 Pos_Pred_Value     0.382     0.569  0.719    0.1 percentile
#>  9 Precision          0.382     0.569  0.719    0.1 percentile
#> 10 Recall             0.419     0.553  0.709    0.1 percentile
#> 11 Sensitivity        0.419     0.553  0.709    0.1 percentile
#> 12 Specificity        0.349     0.574  0.737    0.1 percentile
#> 13 cv_metric_AUC      0.622     0.622  0.622    0.1 percentile
#> 14 logLoss            0.672     0.687  0.705    0.1 percentile
#> 15 prAUC              0.444     0.574  0.690    0.1 percentile
if (FALSE) { # \dontrun{
outcome_colname <- "dx"
run_ml(otu_mini_bin, "rf", outcome_colname = "dx") %>%
  bootstrap_performance(outcome_colname,
    bootstrap_times = 10000,
    alpha = 0.05
  )
} # }
```
