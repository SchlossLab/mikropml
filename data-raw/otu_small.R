## code to prepare `otu_small` dataset
otu_small <- otu_medium[,1:61]
usethis::use_data(otu_small, overwrite = TRUE)
