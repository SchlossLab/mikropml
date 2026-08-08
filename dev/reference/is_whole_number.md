# Check whether a numeric vector contains whole numbers.

Because `is.integer` checks for the class, *not* whether the number is
an integer in the mathematical sense. This code was copy-pasted from the
`is.integer` docs.

## Usage

``` r
is_whole_number(x, tol = .Machine$double.eps^0.5)
```

## Arguments

- x:

  numeric vector

- tol:

  tolerance (default: `.Machine$double.eps^0.5`)

## Value

logical vector

## Examples

``` r
if (FALSE) { # \dontrun{
is_whole_number(c(1, 2, 3))
is.integer(c(1, 2, 3))
is_whole_number(c(1.0, 2.0, 3.0))
is_whole_number(1.2)
} # }
```
