impute <- function(transformed_cont, n_missing) {
  transformed_cont <- sapply_fn(transformed_cont, function(x) {
    if (class(x) %in% c("integer", "numeric")) {
      m <- is.na(x)
      x[m] <- stats::median(x, na.rm = TRUE)
    }
    return(x)
  }) %>% dplyr::as_tibble()
  message(
    paste0(
      n_missing,
      " missing continuous value(s) were imputed using the median value of the feature."
    )
  )
  return (transformed_cont)
}