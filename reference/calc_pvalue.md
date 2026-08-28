# Calculate the p-value for a permutation test

compute Monte Carlo p-value with correction based on formula from Page
158 of 'Bootstrap methods and their application' By Davison & Hinkley
1997

## Usage

``` r
calc_pvalue(vctr, test_stat)
```

## Arguments

- vctr:

  vector of statistics

- test_stat:

  the test statistic

## Value

the number of observations in `vctr` that are greater than `test_stat`
divided by the number of observations in `vctr`

## Author

Kelly Sovacool <sovacool@umich.edu>
