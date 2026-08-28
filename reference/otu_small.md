# Small OTU abundance dataset

A dataset containing relatives abundances of 60 OTUs for 60 human stool
samples. This is a subset of the data provided in
`extdata/otu_large.csv`, which was used in [Topçuoğlu *et al.*
2020](https://journals.asm.org/doi/10.1128/mbio.00434-20).

## Usage

``` r
otu_small
```

## Format

A data frame with 60 rows and 61 variables. The `dx` column is the
diagnosis: healthy or cancerous (colorectal). All other columns are OTU
relative abundances.
