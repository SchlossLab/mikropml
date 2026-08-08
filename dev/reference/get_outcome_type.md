# Get outcome type.

If the outcome is numeric, the type is continuous. Otherwise, the
outcome type is binary if there are only two outcomes or multiclass if
there are more than two outcomes.

## Usage

``` r
get_outcome_type(outcomes_vec)
```

## Arguments

- outcomes_vec:

  Vector of outcomes.

## Value

Outcome type (continuous, binary, or multiclass).

## Author

Zena Lapp, <zenalapp@umich.edu>

## Examples

``` r
get_outcome_type(c(1, 2, 1))
#> [1] "continuous"
get_outcome_type(c("a", "b", "b"))
#> [1] "binary"
get_outcome_type(c("a", "b", "c"))
#> [1] "multiclass"
```
