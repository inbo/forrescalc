#' change dataframe from long to wide format
#'
#' This function changes a dataframe from long to wide format, e.g. to show data
#' from 2 different periods in different columns.
#'
#' @param table_long dataframe with data in long format. It is important that
#' all columns that are not mentioned in the variables \code{column_to_repeat}
#' or \code{columns_for_comparison}, are grouping variables that have the same
#' value for each \code{column_to_repeat} (see 2nd example).
#' @param column_to_repeat name of the column of which the values have to be
#' added to the column headings
#' @param columns_for_comparison (vector with) name(s) of the column(s) you
#' want to repeat for each value of \code{column_to_repeat}
#'
#' @return the dataframe in long format
#'
#' @export
#'
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' library(forrescalc)
#' library(dplyr)
#' table_long <- read_forresdat_table(tablename = "dendro_by_plot_species") %>%
#'   filter(plot_id < 110) %>%
#'   select(plot_id, species, period, number_of_trees_ha, vol_alive_m3_ha)
#' table_wide <-
#'   make_table_wide(
#'     table_long, column_to_repeat = "period",
#'     columns_for_comparison = c("number_of_trees_ha", "vol_alive_m3_ha"))
#' #if number_of_trees_ha is not mentioned in columns_for_comparison, it is
#' #considered as a grouping variable while it has different values for each
#' #period.
#' #This gives an unwanted result with still many rows and a lot of NA values:
#' table_wide <-
#'   make_table_wide(table_long, column_to_repeat = "period",
#'                   columns_for_comparison = c("vol_alive_m3_ha"))
#'
make_table_wide <-
  function(table_long, column_to_repeat, columns_for_comparison) {
  table_wide <- table_long %>%
    pivot_wider(
      names_from = column_to_repeat,
      values_from = columns_for_comparison
    )

  return(table_wide)
}
