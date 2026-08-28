# Check whether package(s) are installed

Check whether package(s) are installed

## Usage

``` r
check_packages_installed(...)
```

## Arguments

- ...:

  names of packages to check

## Value

named vector with status of each packages; installed (`TRUE`) or not
(`FALSE`)

## Author

Kelly Sovacool <sovacool@umich.edu>

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
check_packages_installed("base")
check_packages_installed("not-a-package-name")
all(check_packages_installed("parallel", "doFuture"))
} # }
```
