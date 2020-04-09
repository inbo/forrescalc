#' change dataframe from long to wide format
#'
#' This function changes a dataframe from long to wide format, e.g. to show data from 2 different periods in different columns.
#'
#' @param table_long dataframe with data in long format. It is important that all columns that are not mentioned in the variables \code{column_to_repeat} or \code{columns_for_comparison}, are grouping variables that have the same value for each \code{column_to_repeat} (see 2nd example).
#' @param column_to_repeat name of the column of which the values have to be added to the column headings
#' @param columns_for_comparison (vector with) name(s) of the column(s) you want to repeat for each value of \code{column_to_repeat}
#'
#' @return the dataframe in long format
#'
#' @export
#'
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(dplyr)
#' table_long <-
#'   read_git(tablename = "dendro_status_tree", repo_path = "C:/gitrepo/forresdat") %>%
#'   filter(plot_id < 110) %>%
#'   select(plot_id, species, tree_id, period, dbh_mm, AliveDead)
#' table_wide <-
#'   make_table_wide(table_long, column_to_repeat = "period",
#'                   columns_for_comparison = c("dbh_mm", "AliveDead"))
#' #if dbh_mm is not mentioned in columns_for_comparison, it is considered as a
#' #grouping variable while it has different values for each period.
#' #This gives an unwanted result with still many rows and a lot of NA values:
#' table_wide <-
#'   make_table_wide(table_long, column_to_repeat = "period",
#'                   columns_for_comparison = c("AliveDead"))
#' }
#'
make_table_wide <- function(table_long, column_to_repeat, columns_for_comparison) {
  table_wide <- table_long %>%
    pivot_wider(names_from = column_to_repeat, values_from = columns_for_comparison)

  return(table_wide)
}
