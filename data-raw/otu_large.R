## code to prepare `otu_large` dataset goes here
otu_large <- read.delim("data-raw/otu_large.csv", sep=",")
usethis::use_data(otu_large, overwrite = TRUE)
