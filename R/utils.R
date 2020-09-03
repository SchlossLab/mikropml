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


#' Get the outcome value of interest for AUC calculations
#'
#' Choose the outcome value of interest from the outcome column based on
#' which outcome has fewer rows, or is the first row of the dataframe.
#'
#' @inheritParams run_ml
#' @param method Method to choose outcome value of interest ("fewer", "first")
#'
#' @return outcome value of interest
#' @noRd
#'
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#' @author Zena Lapp, \email{zenalapp@@umich.edu}
#'
#' @examples
#' get_outcome_value(otu_medium, "dx")
#' get_outcome_value(otu_medium, "dx", "first")
pick_outcome_value <- function(dataset, outcome_colname, method = "fewer") {
  if (method == "fewer") {
    outcome_value <- names(which.min(table(dataset[, outcome_colname])))
  } else if (method == "first") {
    outcome_value <- as.character(dataset[1, outcome_colname])
  } else {
    stop(paste(
      "Method", method, "for selecting outcome value not recognized.\n",
      '  Supported methods: "fewer", "first"'
    ))
  }
  return(outcome_value)
}

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
randomize_feature_order <- function(dataset, outcome_colname, seed = NULL) {
  if (!is.null(seed)) {
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
#' split_outcome_features(mikRopML::otu_mini, "dx")
split_outcome_features <- function(dataset, outcome_colname) {
  # input validation
  check_dataset(dataset)
  check_outcome_column(dataset, outcome_colname)
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

#' Register a cluster for parallel processing
#'
#' @inheritParams run_ml
#' @param setup_timeout Setup timeout in seconds. See \link[parallel]{makePSOCKcluster} for details.
#'
#' @return PSOCK cluster, or NULL if only 1 core provided or required packages aren't available
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' para_cluster <- setup_parallel(2)
#' # insert code that uses foreach here
#' stop_parallel(para_cluster)
setup_parallel <- function(ncores, setup_timeout = 0.5) {
  pcluster <- NULL
  if (!is.numeric(ncores) & !is.na(ncores)) {
    warning(paste(
      "`ncores` must be `NA` or a number, but you provided", ncores,
      "\nProceeding with only one process."
    ))
  } else if (!is.na(ncores) & ncores > 1) {
    if (!all(check_package_installed(c("parallel", "doParallel", "foreach")))) {
      warning(paste(
        "The packages `parallel`, `doParallel`, and `foreach` are required for using multiple cores.\n",
        "You specified", ncores, "cores, but one or more of these packages are not installed.\n",
        "Proceeding with only one process."
      ))
    } else {
      cores_avail <- parallel::detectCores()
      if (ncores > cores_avail) {
        warning(paste(
          "You specified", ncores, "cores, but only", cores_avail, "cores are available.",
          "\nProceeding with only one process."
        ))
      } else {
        pcluster <- parallel::makePSOCKcluster(ncores,
          setup_timeout = setup_timeout
        )
        doParallel::registerDoParallel(pcluster)
        message(paste("Using", ncores, "cores for parallel processing."))
      }
    }
  }
  return(pcluster)
}

#' Unregsiter a parallel processing cluster
#'
#' @param pcluster a PSOCK cluster created by `setup_cluster``
#'
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#'
#' @examples
#' para_cluster <- setup_parallel(2)
#' # insert code that uses foreach here
#' stop_parallel(para_cluster)
stop_parallel <- function(pcluster) {
  if (!is.null(pcluster)) {
    parallel::stopCluster(pcluster)
    foreach::registerDoSEQ() # so additional calls to foreach will use sequential processes
  }
}

#' Get model performance metrics as a one-row tibble
#'
#' @inheritParams calc_aucs
#' @param rseed random seed used in `run_ml``
#'
#' @return a one-row tibble with columns `cv_auroc`, `test_auroc`, and `test_auprc`
#' @noRd
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
get_performance_tbl <-  function(trained_model_caret, test_data, outcome_colname, outcome_value, rseed) {
  test_aucs <- calc_aucs(trained_model_caret, test_data, outcome_colname, outcome_value)
  return(dplyr::tibble(
    cv_auroc = caret::getTrainPerf(trained_model_caret)$TrainROC,
    test_auroc = test_aucs$auroc,
    test_auprc = test_aucs$auprc,
    method = trained_model_caret$method,
    seed = rseed
  ))
}
