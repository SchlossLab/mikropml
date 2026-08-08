# Shuffle the rows in a column

Shuffle the rows in a column

## Usage

``` r
shuffle_group(dat, col_name)
```

## Arguments

- dat:

  a data frame containing `col_name`

- col_name:

  column name to shuffle

## Value

`dat` with the rows of `col_name` shuffled

## Author

Courtney R Armour, <armourc@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(123)
df <- dplyr::tibble(
  condition = c("a", "a", "b", "b"),
  AUC = c(.2, 0.3, 0.8, 0.9)
)
shuffle_group(df, "condition")
} # }
```
