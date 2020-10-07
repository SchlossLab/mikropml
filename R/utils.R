#' dplyr pipe
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' rlang data pronoun
#' @importFrom rlang .data
#' @export
rlang::.data

#' caret contr.ltfr
#' @importFrom caret contr.ltfr
#' @export
caret::contr.ltfr


#' @importFrom rlang !!
#' @export
rlang::`!!`

## make R CMD CHECK shut up about the dot `.``
## See: \url{https://github.com/tidyverse/magrittr/issues/29}
utils::globalVariables(c("."))

#' Randomize feature order to eliminate any position-dependent effects
#'
#' @inheritParams run_ml
#'
#' @return dataset with feature order randomized
#' @noRd
#' @author Nick Lesniak, \email{nlesniak@@umich.edu}
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' randomize_feature_order(otu_small, "dx")
randomize_feature_order <- function(dataset, outcome_colname, seed = NA) {
  if (!is.na(seed)) {
    set.seed(seed)
  }
  features <- sample(colnames(dataset[names(dataset) != outcome_colname]))
  dataset <- dplyr::select(
    dataset,
    dplyr::one_of(outcome_colname),
    dplyr::one_of(features)
  )
  return(dataset)
}

#' Split dataset into outcome and features
#'
#' @inheritParams run_ml
#'
#' @return list of length two: outcome, features
#' @export
#'
#' @examples
#' split_outcome_features(mikropml::otu_mini, "dx")
split_outcome_features <- function(dataset, outcome_colname) {
  # input validation
  check_dataset(dataset)
  check_outcome_column(dataset, outcome_colname, show_message = FALSE)
  # split outcome and features
  outcome <- dataset %>% dplyr::select_if(names(dataset) == outcome_colname)
  features <- dataset %>% dplyr::select_if(names(dataset) != outcome_colname)
  return(list(outcome = outcome, features = features))
}

#' Use future apply if available
#'
#' @param fun apply function to use (apply, lapply, sapply, etc.)
#'
#' @return output of apply function
#' @noRd
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' select_apply(fun = "sapply")
select_apply <- function(fun = "apply") {
  pkg <- "base"
  if (check_package_installed("future.apply")) {
    fun <- paste0("future_", fun)
    pkg <- "future.apply"
  }
  return(utils::getFromNamespace(fun, pkg))
}

#' Mutate all columns with type.convert
#'
#' Turns factors into characters and numerics where possible
#'
#' @param dat data.frame to convert
#'
#' @return data.frame with no factors
#' @export
#'
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' dat <- data.frame(
#'   c1 = as.factor(c("a", "b", "c")),
#'   c2 = as.factor(1:3)
#' )
#' class(dat$c1)
#' class(dat$c2)
#' dat <- mutate_all_types(dat)
#' class(dat$c1)
#' class(dat$c2)
mutate_all_types <- function(dat) {
  return(dat %>% dplyr::mutate_all(utils::type.convert, as.is = TRUE))
}
#' Get model performance metrics as a one-row tibble
#'
#' @inheritParams calc_perf_metrics
#' @inheritParams run_ml
#' @inheritParams get_feature_importance
#'
#' @return a one-row tibble with columns `cv_auroc`, `test_auroc`, `test_auprc`, `method`, and `seed`
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
get_performance_tbl <- function(trained_model, test_data, outcome_colname, perf_metric_function, perf_metric_name, seed = NA) {
  test_perf_metrics <- calc_perf_metrics(test_data, trained_model, outcome_colname, perf_metric_function)
  cv_metric = caret::getTrainPerf(trained_model)[[paste0('Train',perf_metric_name)]]
  if(is.null(cv_metric)) warning(paste0('The cv metric provided does not match with that used to train the data. You provided: '), perf_metric_name, '. The options are ',paste0(gsub('Train|method','',names(caret::getTrainPerf(trained_model))),sep=', '))
  all_info <- dplyr::bind_rows(c(
    cv_metric = cv_metric,
    test_perf_metrics,
    method = trained_model$method,
    seed = seed
  ))
  names(all_info)[names(all_info) == 'cv_metric'] <- paste0('cv_metric_',perf_metric_name)
  return(change_to_num(all_info))
}
