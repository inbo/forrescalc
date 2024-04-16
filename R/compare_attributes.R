#' compare attributes and suggest aggregated attribute
#'
#' This helper function compares the attributes of the given R objects
#' (e.g. datasets)
#' and drops an error if they don't have the same (or compatible) attributes.
#' If they have the same or compatible attributes, the attribute is returned
#' to be used as an attribute for the result of the aggregated calculation.
#'
#' @param x first R object (dataframe) with attribute 'database'
#' @param y second R object (dataframe) with attribute 'database'
#' @param name_x name of first R object as known by user
#' @param name_y name of second R object as known by user
#'
#' @return string that can be used as attribute for the aggregated object
#'
#' @noRd
#'
compare_attributes <- function(x, y, name_x, name_y) {
  if (attr(x, "database") == attr(y, "database")) {
    attr_database <- attr(x, "database")
  } else {
    stop(
      sprintf(
        "%1$s and %2$s are not from the same version of the database, please only provide datasets from the same database",
        name_x, name_y
      )
    )
  }

  return(list(attr_database = attr_database, attr_date = attr_date))
}
