impute <- function(transformed_cont) {
  sapply_fn <- select_apply("sapply")
  cl <- sapply_fn(transformed_cont, function(x) {
    class(x)
  })
  missing <-
    is.na(transformed_cont[, cl %in% c("integer", "numeric")])
  n_missing <- sum(missing)
  if (n_missing > 0) {
    transformed_cont <- sapply_fn(transformed_cont, function(x) {
      if (class(x) %in% c("integer", "numeric")) {
        m <- is.na(x)
        x[m] <- stats::median(x, na.rm = TRUE)
      }
      message(typeof(x))
      message(class(x))
      return(x)
    }) %>% dplyr::as_tibble()
    message(
      paste0(
        n_missing,
        " missing continuous value(s) were imputed using the median value of the feature."
      )
    )
  }
  return (transformed_cont)git 
}