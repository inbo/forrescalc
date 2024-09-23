#' compare colnames between new table and `forresdat`
#'
#' This helper function checks if the column names of the table to be added to
#' `forresdat` has the same column names as the table in `forresdat`.
#' This code is to be used in `save_results_forresdat()` and
#' `from_access_to_forresdat()`,
#' to avoid having the same code twice.
#'
#' @param table table to be added to `forresdat`
#' @param tablename name of table to be added to `forresdat`
#' @param colnames_forresdat the column names of this table in `forresdat`
#'   (in order of occurrence)
#' @inheritParams save_results_forresdat
#'
#' @return (reworked) table with the column names in the order they should be
#' added to `forresdat` (or an error in case strict = TRUE)
#'
#' @importFrom dplyr %>% select
#'
#' @noRd
#'
compare_colnames_forresdat <-
  function(table, tablename, colnames_forresdat, strict) {
  colnames_results <- colnames(table)
  colnames_old <-
    colnames_forresdat[!colnames_forresdat %in% colnames_results]
  colnames_new <-
    colnames_results[!colnames_results %in% colnames_forresdat]
  if (length(colnames_old) > 0 || length(colnames_new) > 0) {
    if (strict) {
      text <- paste0(
        sprintf(
          "extra in new table: %s",
          paste(colnames_new, sep = ", ")[length(colnames_new) > 0]
        ),
        "; "[length(colnames_new) > 0 && length(colnames_old) > 0],
        sprintf(
          "extra in forresdat: %s",
          paste(colnames_old, sep = ", ")[length(colnames_old) > 0]
        )
      )
      stop(
        sprintf(
          "Table %1$s has different column names than the version on forresdat. (%2$s) Use strict = FALSE if you want to save the new version anyway.", #nolint: line_length_linter
          tablename, text
        )
      )
    } else {
      colnames_forresdat <-
        colnames_forresdat[colnames_forresdat %in% colnames_results]
      colnames_forresdat <- c(colnames_forresdat, colnames_new)
    }
  }
  table <- table %>%
    select(colnames_forresdat)

  return(table)
}
