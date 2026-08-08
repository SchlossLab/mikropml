# Update progress if the progress bar is not `NULL`.

This allows for flexible code that only initializes a progress bar if
the `progressr` package is installed.

## Usage

``` r
pbtick(pb, message = NULL)
```

## Arguments

- pb:

  a progress bar created with `progressr`.

- message:

  optional message to report (default: `NULL`).

## Author

Kelly Sovacool <sovacool@umich.edu>

## Examples

``` r
if (FALSE) { # \dontrun{
f <- function() {
  if (isTRUE(check_packages_installed("progressr"))) {
    pb <- progressr::progressor(steps = 5, message = "looping")
  } else {
    pb <- NULL
  }
  for (i in 1:5) {
    pbtick(pb)
    Sys.sleep(0.5)
  }
}
progressr::with_progress(
  f()
)
} # }
```
