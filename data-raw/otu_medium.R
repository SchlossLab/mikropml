## code to prepare `otu_medium` dataset goes here
otu_medium <- read.delim("data-raw/otu_medium.csv", sep=",")
usethis::use_data(otu_medium, overwrite = TRUE)
