# Plot ROC and PRC curves

Plot ROC and PRC curves

## Usage

``` r
plot_mean_roc(dat, ribbon_fill = "#C6DBEF", line_color = "#08306B")

plot_mean_prc(
  dat,
  baseline_precision = NULL,
  ycol = mean_precision,
  ribbon_fill = "#C7E9C0",
  line_color = "#00441B"
)
```

## Arguments

- dat:

  sensitivity, specificity, and precision data calculated by
  [`calc_mean_roc()`](http://www.schlosslab.org/mikropml/dev/reference/sensspec.md)

- ribbon_fill:

  ribbon fill color (default: "#D9D9D9")

- line_color:

  line color (default: "#000000")

- baseline_precision:

  baseline precision from
  [`calc_baseline_precision()`](http://www.schlosslab.org/mikropml/dev/reference/calc_baseline_precision.md)

- ycol:

  column for the y axis (Default: `mean_precision`)

## Functions

- `plot_mean_roc()`: Plot mean sensitivity over specificity

- `plot_mean_prc()`: Plot mean precision over recall

## Author

Courtney Armour

Kelly Sovacool <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
# get performance for multiple models
get_sensspec_seed <- function(seed) {
  ml_result <- run_ml(otu_mini_bin, "glmnet", seed = seed)
  sensspec <- calc_model_sensspec(
    ml_result$trained_model,
    ml_result$test_data,
    "dx"
  ) %>%
    mutate(seed = seed)
  return(sensspec)
}
sensspec_dat <- purrr::map_dfr(seq(100, 102), get_sensspec_seed)

# plot ROC & PRC
sensspec_dat %>%
  calc_mean_roc() %>%
  plot_mean_roc()
baseline_prec <- calc_baseline_precision(otu_mini_bin, "dx", "cancer")
sensspec_dat %>%
  calc_mean_prc() %>%
  plot_mean_prc(baseline_precision = baseline_prec)
} # }
```
