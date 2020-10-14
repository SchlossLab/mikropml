## code to prepare `otu_large` dataset goes here
otu_large_multi <- read.delim("data-raw/otu_large_multi.csv", sep = ",")
usethis::use_data(otu_large_multi, overwrite = TRUE)
