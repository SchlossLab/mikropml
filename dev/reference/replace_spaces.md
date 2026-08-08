# Replace spaces in all elements of a character vector with underscores

Replace spaces in all elements of a character vector with underscores

## Usage

``` r
replace_spaces(x, new_char = "_")
```

## Arguments

- x:

  a character vector

- new_char:

  the character to replace spaces (default: `_`)

## Value

character vector with all spaces replaced with `new_char`

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
dat <- data.frame(
  dx = c("outcome 1", "outcome 2", "outcome 1"),
  a = 1:3, b = c(5, 7, 1)
)
dat$dx <- replace_spaces(dat$dx)
dat
#>          dx a b
#> 1 outcome_1 1 5
#> 2 outcome_2 2 7
#> 3 outcome_1 3 1
```
