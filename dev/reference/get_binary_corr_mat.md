# Identify correlated features as a binary matrix

Identify correlated features as a binary matrix

## Usage

``` r
get_binary_corr_mat(
  features,
  corr_thresh = 1,
  group_neg_corr = TRUE,
  corr_method = "spearman"
)
```

## Arguments

- features:

  a dataframe with each column as a feature for ML

- corr_thresh:

  For feature importance, group correlations above or equal to
  `corr_thresh` (range `0` to `1`; default: `1`).

- group_neg_corr:

  Whether to group negatively correlated features together (e.g. c(0,1)
  and c(1,0)).

- corr_method:

  Correlation method. Options are the same as those supported by
  [`stats::cor`](https://rdrr.io/r/stats/cor.html): spearman, pearson,
  kendall. (default: spearman)

## Value

A binary matrix of correlated features

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
features <- data.frame(
  a = 1:3, b = 2:4, c = c(1, 0, 1),
  d = (5:7), e = c(5, 1, 4)
)
get_binary_corr_mat(features)
} # }
```
