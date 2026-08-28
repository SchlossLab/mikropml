# Calculate the fraction of positives, i.e. baseline precision for a PRC curve

Calculate the fraction of positives, i.e. baseline precision for a PRC
curve

## Usage

``` r
calc_baseline_precision(dataset, outcome_colname = NULL, pos_outcome = NULL)
```

## Arguments

- dataset:

  Data frame with an outcome variable and other columns as features.
  Alternatively, the input can be in `TreeSummarizedExperiment` format.

- outcome_colname:

  Column name as a string of the outcome variable (default `NULL`; the
  first column will be chosen automatically).

- pos_outcome:

  the positive outcome from `outcome_colname`, e.g. "cancer" for the
  `otu_mini_bin` dataset.

## Value

the baseline precision based on the fraction of positives

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
# calculate the baseline precision
data.frame(y = c("a", "b", "a", "b")) %>%
  calc_baseline_precision(
    outcome_colname = "y",
    pos_outcome = "a"
  )
#> Using 'y' as the outcome column.
#> [1] 0.5


calc_baseline_precision(otu_mini_bin,
  outcome_colname = "dx",
  pos_outcome = "cancer"
)
#> Using 'dx' as the outcome column.
#> [1] 0.49


# if you're not sure which outcome was used as the 'positive' outcome during
# model training, you can access it from the trained model and pass it along:
calc_baseline_precision(otu_mini_bin,
  outcome_colname = "dx",
  pos_outcome = otu_mini_bin_results_glmnet$trained_model$levels[1]
)
#> Using 'dx' as the outcome column.
#> [1] 0.49
```
