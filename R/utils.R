#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom rlang .data
#' @export
rlang::.data

#' Get the outcome value of interest for AUC calculations
#'
#' Choose the outcome value of interest from the outcome column based on
#' which outcome has fewer rows, or is the first row of the dataframe.
#'
#' @inheritParams run_pipeline
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
#' @inheritParams run_pipeline
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
#' @inheritParams run_pipeline
#'
#' @return list of length two: outcome, features
#' @export
#'
#' @examples split_outcome_features(mikRopML::otu_mini,'dx')
split_outcome_features <- function(dataset, outcome_colname){
  # input validation
  check_dataset(dataset)
  check_outcome_column(dataset,outcome_colname)
  # split outcome and features
  outcome <- dataset %>% dplyr::select_if(names(dataset) == outcome_colname)
  features <- dataset %>% dplyr::select_if(names(dataset) != outcome_colname)
  return(list(outcome=outcome,features=features))
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
