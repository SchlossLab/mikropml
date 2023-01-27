set.seed(2023)
library(dplyr)
library(usethis)

otu_micro <- otu_mini_bin %>% group_by(dx) %>% slice_sample(n = 10)
