# Identify correlated features

Identify correlated features

## Usage

``` r
get_corr_feats(
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

  correlation method. options or the same as those supported by
  [`stats::cor`](https://rdrr.io/r/stats/cor.html): spearman, pearson,
  kendall. (default: spearman)

## Value

Dataframe of correlated features where the columns are feature1,
feature2, and the correlation between those two features (anything
exceeding corr_thresh).

## Author

Begüm Topçuoğlu, <topcuoglu.begum@gmail.com>

Zena Lapp, <zenalapp@umich.edu>
