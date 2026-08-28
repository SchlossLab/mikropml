# Assign features to groups

Assign features to groups

## Usage

``` r
get_groups_from_clusters(cluster_ids)
```

## Arguments

- cluster_ids:

  named vector created by
  [`cluster_corr_mat()`](http://www.schlosslab.org/mikropml/reference/cluster_corr_mat.md).
  Each element is a cluster and the name is a feature in that cluster.

## Value

a vector where each element is a group of correlated features separated
by pipes (`|`)

## Author

Kelly Sovacool, <sovacool@umich.edu>

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
get_groups_from_clusters(cluster_corr_mat(corr_mat))
} # }
```
