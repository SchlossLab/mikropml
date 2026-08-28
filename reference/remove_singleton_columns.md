# Remove columns appearing in only `threshold` row(s) or fewer.

Removes columns which only have non-zero & non-NA values in `threshold`
row(s) or fewer.

## Usage

``` r
remove_singleton_columns(dat, threshold = 1)
```

## Arguments

- dat:

  dataframe

- threshold:

  Number of rows. If a column only has non-zero & non-NA values in
  `threshold` row(s) or fewer, it will be removed.

## Value

dataframe without singleton columns

## Author

Kelly Sovacool, <sovacool@umich.edu>

Courtney Armour

## Examples

``` r
remove_singleton_columns(data.frame(a = 1:3, b = c(0, 1, 0), c = 4:6))
#> $dat
#>   a c
#> 1 1 4
#> 2 2 5
#> 3 3 6
#> 
#> $removed_feats
#> [1] "b"
#> 
remove_singleton_columns(data.frame(a = 1:3, b = c(0, 1, 0), c = 4:6), threshold = 0)
#> $dat
#>   a b c
#> 1 1 0 4
#> 2 2 1 5
#> 3 3 0 6
#> 
#> $removed_feats
#> character(0)
#> 
remove_singleton_columns(data.frame(a = 1:3, b = c(0, 1, NA), c = 4:6))
#> $dat
#>   a c
#> 1 1 4
#> 2 2 5
#> 3 3 6
#> 
#> $removed_feats
#> [1] "b"
#> 
remove_singleton_columns(data.frame(a = 1:3, b = c(1, 1, 1), c = 4:6))
#> $dat
#>   a b c
#> 1 1 1 4
#> 2 2 1 5
#> 3 3 1 6
#> 
#> $removed_feats
#> character(0)
#> 
```
