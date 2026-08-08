# Throw error if required packages are not installed.

Reports which packages need to be installed and the parent function
name. See
<https://stackoverflow.com/questions/15595478/how-to-get-the-name-of-the-calling-function-inside-the-called-routine>
This is only intended to be used inside a function. It will error
otherwise.

## Usage

``` r
abort_packages_not_installed(...)
```

## Arguments

- ...:

  names of packages to check

## Author

Kelly Sovacool <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
abort_packages_not_installed("base")
abort_packages_not_installed("not-a-package-name", "caret", "dplyr", "non_package")
} # }
```
