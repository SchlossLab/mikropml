# Generic function to calculate mean performance curves for multiple models

Used by
[`calc_mean_roc()`](http://www.schlosslab.org/mikropml/dev/reference/sensspec.md)
and
[`calc_mean_prc()`](http://www.schlosslab.org/mikropml/dev/reference/sensspec.md).

## Usage

``` r
calc_mean_perf(sensspec_dat, group_var = specificity, sum_var = sensitivity)
```

## Arguments

- sensspec_dat:

  data frame created by concatenating results of
  [`calc_model_sensspec()`](http://www.schlosslab.org/mikropml/dev/reference/sensspec.md)
  for multiple models.

- group_var:

  variable to group by (e.g. specificity or recall).

- sum_var:

  variable to summarize (e.g. sensitivity or precision).

## Value

data frame with mean & standard deviation of `sum_var` summarized over
`group_var`

## Author

Courtney Armour

Kelly Sovacool
