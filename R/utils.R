#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' Get the outcome value of interest for AUC calculations
#'
#' Choose the outcome value of interest from the outcome column based on
#' which outcome has fewer rows or is the first row of the dataframe.
#'
#' @param dataset Dataframe of input data
#' @param outcome_colname Column name of the outcome
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
get_outcome_value <- function(dataset, outcome_colname, method = "fewer") {
  if (method == "fewer") {
    outcome_value <- names(which.min(table(dataset[, outcome_colname])))
  } else if (method == "first") {
    outcome_value <- as.character(dataset[1, outcome_colname])
  } else {
    stop(paste(
      "Method", method, "for selecting outcome value not recognized.\n",
      'Supported methods: "fewer", "first"'
    ))
  }
  return(outcome_value)
}
