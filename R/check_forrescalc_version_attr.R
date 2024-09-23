#' check forrescalc attribute with current forrescalc version
#'
#' This helper function compares the forrescalc version attribute
#' of the given R object (e.g. datasets)
#' and drops an error if they don't have the same version number.
#'
#' @param x R object (dataframe) with attribute 'forrescalc'
#'
#' @return string with version number of forrescalc (attribute)
#'
#' @noRd
#'
#' @importFrom utils packageVersion
#'
check_forrescalc_version_attr <- function(x) {
    if (
      attr(x, "forrescalc") != paste("forrescalc", packageVersion("forrescalc"))
    ) {
      stop(
        "The given dataset is not calculated with the version of forrescalc that is used now, please use the same version of the package for all calculations" #nolint: line_length_linter
      )
    }

  return(attr(x, "forrescalc"))
}
