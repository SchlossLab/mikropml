# Cluster a matrix of correlated features

Cluster a matrix of correlated features

## Usage

``` r
cluster_corr_mat(bin_corr_mat, hclust_method = "single", cut_height = 0)
```

## Arguments

- bin_corr_mat:

  a binary correlation matrix created by
  [`get_binary_corr_mat()`](http://www.schlosslab.org/mikropml/dev/reference/get_binary_corr_mat.md).

- hclust_method:

  the `method` to use in
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html) (default:
  'single').

- cut_height:

  the cut height (`h`) to use in
  [`stats::cutree()`](https://rdrr.io/r/stats/cutree.html) (default: 0).

## Value

a named vector from
[`stats::cutree()`](https://rdrr.io/r/stats/cutree.html). Each element
is a cluster and the name is a feature in that cluster.

## Author

Kelly Sovacool, <sovacool@umich.edu>

Pat Schloss, <pschloss@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
corr_mat <- matrix(
  data = c(1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1),
  nrow = 4,
  dimnames = list(
    c("a", "b", "c", "d"),
    c("a", "b", "c", "d")
  )
)
corr_mat
cluster_corr_mat(corr_mat)
} # }
```
