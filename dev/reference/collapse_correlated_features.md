# Collapse correlated features

Collapse correlated features

## Usage

``` r
collapse_correlated_features(
  features,
  group_neg_corr = TRUE,
  corr_method = "spearman",
  corr_thresh = 1,
  progbar = NULL
)
```

## Arguments

- features:

  dataframe of features for machine learning

- group_neg_corr:

  Whether to group negatively correlated features together (e.g. c(0,1)
  and c(1,0)).

- corr_method:

  Correlation method. Options are the same as those supported by
  [`stats::cor`](https://rdrr.io/r/stats/cor.html): spearman, pearson,
  kendall. (default: spearman)

- corr_thresh:

  group correlations above or equal to `corr_thresh` (range `0` to `1`;
  default: `1`).

- progbar:

  optional progress bar (default: `NULL`)

## Value

features where perfectly correlated ones are collapsed

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
collapse_correlated_features(mikropml::otu_small[, 2:ncol(otu_small)])
} # }
```
