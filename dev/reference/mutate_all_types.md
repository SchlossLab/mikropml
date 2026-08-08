# Mutate all columns with `utils::type.convert()`.\`

Turns factors into characters and numerics where possible.

## Usage

``` r
mutate_all_types(dat)
```

## Arguments

- dat:

  data.frame to convert

## Value

data.frame with no factors

## Author

Kelly Sovacool, <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- data.frame(
  c1 = as.factor(c("a", "b", "c")),
  c2 = as.factor(1:3)
)
class(dat$c1)
class(dat$c2)
dat <- mutate_all_types(dat)
class(dat$c1)
class(dat$c2)
} # }
```
