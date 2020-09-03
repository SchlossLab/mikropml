if (!is.null(snakemake@log)) {
    log_filepath <- snakemake@log[1][[1]]
    log <- file(log_filepath, open = "wt")
    sink(log, append = TRUE)
    sink(log, append = TRUE, type = "message")
}
