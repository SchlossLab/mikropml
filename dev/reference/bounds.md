# Get the lower and upper bounds for an empirical confidence interval

Get the lower and upper bounds for an empirical confidence interval

## Usage

``` r
lower_bound(x, alpha)

upper_bound(x, alpha)
```

## Arguments

- x:

  vector of test statistics, such as from permutation tests or
  bootstraps

- alpha:

  alpha level for the confidence interval (default: `0.05` to obtain a
  95% confidence interval)

## Value

the value of the lower or upper bound for the confidence interval

## Functions

- `lower_bound()`: Get the lower bound for an empirical confidence
  interval

- `upper_bound()`: Get the upper bound for an empirical confidence
  interval

## Examples

``` r
if (FALSE) { # \dontrun{
x <- 1:10000
lower_bound(x, 0.05)
upper_bound(x, 0.05)
} # }
```
