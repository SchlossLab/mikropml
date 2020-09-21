
#' Register a multiprocessing plan with `ncores`
#'
#' @param ncores number of cores for multiprocessing
#'
#' @return whether multiple cores are in use (`TRUE` or `FALSE`)
#' @export
#' @author Kelly Sovacool, \email{sovacool@@umich.edu}
#' @examples
#' \dontrun{
#' setup_parallel(2)
#' }
setup_parallel <- function(ncores) {
    is_parallel = FALSE
    # TODO: break out into more functions to make if/else less confusing
    # TODO: move checks to separate check_ncores function
    if (!is.numeric(ncores) & !is.na(ncores)) {
        warning(paste(
            "`ncores` must be `NA` or a number, but you provided:", class(ncores),
            "\nProceeding with only one process."
        ))
    } else if (!is.na(ncores) & ncores > 1) {
        if (!all(check_package_installed(c("future", "doFuture", "foreach", "parallel")))) {
            warning(paste(
                "The packages `future`, `doFuture`, and `foreach` are required for using multiple cores.\n",
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
                is_parallel = TRUE
                doFuture::registerDoFuture()
                future::plan(future::multiprocess, workers = ncores)
                message(paste("Using", ncores, "cores for parallel processing."))
            }
        }
    }
    return(is_parallel)
}
