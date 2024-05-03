#' compare attributes and suggest aggregated attribute
#'
#' This helper function compares the attributes of the given R objects
#' (e.g. datasets)
#' and drops an error if they don't have the same (or compatible) attributes.
#' If they have the same or compatible attributes, the attribute is returned
#' to be used as an attribute for the result of the aggregated calculation.
#'
#' @param x first R object (dataframe) with attributes 'database' and
#' 'forrescalc'
#' @param y second R object (dataframe) with attribute 'database' and
#' 'forrescalc'
#' @param name_x name of first R object as known by user
#' @param name_y name of second R object as known by user
#'
#' @return list of strings that can be used as attributes for the aggregated
#' object
#'
#' @noRd
#'
#' @importFrom utils packageVersion
#'
compare_attributes <- function(x, y, name_x, name_y) {
  if (!is.null(attr(x, "database")) && !is.null(attr(y, "database"))) {
    if (attr(x, "database") == attr(y, "database")) {
      attr_database <- attr(x, "database")
    } else {
      stop(
        sprintf(
          "%1$s and %2$s are not from the same version of the database, please only provide datasets from the same database", #nolint: line_length_linter
          name_x, name_y
        )
      )
    }
  } else {
    attr_database <- NULL
  }
  if (!is.null(attr(x, "forrescalc")) && !is.null(attr(y, "forrescalc"))) {
    if (attr(x, "forrescalc") == attr(y, "forrescalc")) {
      attr_forrescalc <- attr(x, "forrescalc")
    } else {
      stop(
        sprintf(
          "%1$s and %2$s are not calculated with the same version of forrescalc, please use the same version of the package for all calculations", #nolint: line_length_linter
          name_x, name_y
        )
      )
    }
    if (attr_forrescalc != paste("forrescalc", packageVersion("forrescalc"))) {
      stop(
        "The given datasets are not calculated with the version of forrescalc that is used now, please use the same version of the package for all calculations" #nolint: line_length_linter
      )
    }
  } else {
    attr_forrescalc <- NULL
  }

  return(
    list(
      attr_database = attr_database, attr_forrescalc = attr_forrescalc
    )
  )
}
