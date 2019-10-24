#' change dataframe from long to wide format
#'
#' This function changes a dataframe from long to wide format, e.g. to show data from 2 different periods in different columns.
#'
#' @param table_long dataframe with data in long format
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
#' library(dplyr)
#' table_long <-
#'   read_git(tablename = "dendro_status_tree", repo_path = "C:/R/bosreservatendb/forresdat") %>%
#'   filter(plot_id < 110) %>%
#'   select(plot_id, species, tree_id, period, DBH_mm, AliveDead)
#' table_wide <-
#'   make_table_wide(table_long, column_to_repeat = "period",
#'                   columns_for_comparison = c("DBH_mm", "AliveDead"))
#'
make_table_wide <- function(table_long, column_to_repeat, columns_for_comparison) {
  table_wide <- table_long %>%
    pivot_wider(names_from = column_to_repeat, values_from = columns_for_comparison)

  return(table_wide)
}
