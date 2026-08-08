# Calculate and summarize performance for ROC and PRC plots

Use these functions to calculate cumulative sensitivity, specificity,
recall, etc. on single models, concatenate the results together from
multiple models, and compute mean ROC and PRC. You can then plot mean
ROC and PRC curves to visualize the results. **Note**: These functions
assume a binary outcome.

## Usage

``` r
calc_model_sensspec(trained_model, test_data, outcome_colname = NULL)

calc_mean_roc(sensspec_dat)

calc_mean_prc(sensspec_dat)
```

## Arguments

- trained_model:

  Trained model from
  [`caret::train()`](https://rdrr.io/pkg/caret/man/train.html).

- test_data:

  Held out test data: dataframe of outcome and features.

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

- sensspec_dat:

  data frame created by concatenating results of `calc_model_sensspec()`
  for multiple models.

## Value

data frame with summarized performance

## Functions

- `calc_model_sensspec()`: Get sensitivity, specificity, and precision
  for a model.

- `calc_mean_roc()`: Calculate mean sensitivity over specificity for
  multiple models

- `calc_mean_prc()`: Calculate mean precision over recall for multiple
  models

## Author

Courtney Armour

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
# get cumulative performance for a single model
sensspec_1 <- calc_model_sensspec(
  otu_mini_bin_results_glmnet$trained_model,
  otu_mini_bin_results_glmnet$test_data,
  "dx"
)
head(sensspec_1)

# get performance for multiple models
get_sensspec_seed <- function(seed) {
  ml_result <- run_ml(otu_mini_bin, "glmnet", seed = seed)
  sensspec <- calc_model_sensspec(
    ml_result$trained_model,
    ml_result$test_data,
    "dx"
  ) %>%
    dplyr::mutate(seed = seed)
  return(sensspec)
}
sensspec_dat <- purrr::map_dfr(seq(100, 102), get_sensspec_seed)

# calculate mean sensitivity over specificity
roc_dat <- calc_mean_roc(sensspec_dat)
head(roc_dat)

# calculate mean precision over recall
prc_dat <- calc_mean_prc(sensspec_dat)
head(prc_dat)

# plot ROC & PRC
roc_dat %>% plot_mean_roc()
baseline_prec <- calc_baseline_precision(otu_mini_bin, "dx", "cancer")
prc_dat %>%
  plot_mean_prc(baseline_precision = baseline_prec)

# balanced precision
prior <- calc_baseline_precision(otu_mini_bin,
  outcome_colname = "dx",
  pos_outcome = "cancer"
)
bprc_dat <- sensspec_dat %>%
  dplyr::mutate(balanced_precision = calc_balanced_precision(precision, prior)) %>%
  dplyr::rename(recall = sensitivity) %>%
  calc_mean_perf(group_var = recall, sum_var = balanced_precision)
bprc_dat %>% plot_mean_prc(ycol = mean_balanced_precision) + ylab("Mean Bal. Precision")
} # }
```
