# Call `sort()` with `method = 'radix'`

THE BASE SORT FUNCTION USES A DIFFERENT METHOD DEPENDING ON YOUR LOCALE.
However, the order for the radix method is always stable.

## Usage

``` r
radix_sort(...)
```

## Arguments

- ...:

  All arguments forwarded to
  [`sort()`](https://rdrr.io/r/base/sort.html).

## Value

Whatever you passed in, now in a stable sorted order regardless of your
locale.

## Details

see
https://stackoverflow.com/questions/42272119/r-cmd-check-fails-devtoolstest-works-fine

[`stringr::str_sort()`](https://stringr.tidyverse.org/reference/str_order.html)
solves this problem with the `locale` parameter having a default value,
but I don't want to add that as another dependency.

## Author

Kelly Sovacool <sovacool@umich.edu>
