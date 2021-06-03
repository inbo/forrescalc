#' @title couple data of each individual tree in different years
#'
#' @description
#' This function creates a unique ID for each tree, that allows to group (f.e. by use of `make_table_wide()`) all given information on the life stages of an individual tree during different measures.
#'
#' @inheritParams calc_variables_tree_level
#'
#' @return a dataset with 1 record per tree measurement, containing the given data of each tree in different years (= data_dendro) and a link to a unique tree_id.
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' create_overview_status(data_dendro)
#' }
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% filter left_join mutate select
#' @importFrom rlang .data
#'
create_overview_status <- function(data_dendro) {
  assert_that(
    max(data_dendro$period) <= 3,
    msg = "The code of create_overview_status is only adapted to 3 measure periods"
  )
  status_tree <- data_dendro %>%
    mutate(
      tree_id =
        ifelse(
          is.na(.data$old_id),
          paste(.data$period, .data$plot_id, .data$tree_measure_id, sep = "_"),
          ifelse(
            .data$period == 2,
            paste(1, .data$plot_id, .data$old_id, sep = "_"),
            NA
          )
        )
    )
  status_tree <- status_tree %>%
    left_join(
      status_tree %>%
        select(
          .data$plot_id, .data$tree_measure_id, .data$period, .data$tree_id
        ) %>%
        filter(.data$period == 2),
      by = c("plot_id", "old_id" = "tree_measure_id"),
      suffix = c("", "_oldid")
    ) %>%
    mutate(
      tree_id =
        ifelse(
          is.na(.data$tree_id) & .data$period == .data$period_oldid + 1,
          .data$tree_id_oldid,
          .data$tree_id
        )
    ) %>%
    select(-.data$period_oldid, -.data$tree_id_oldid)

  return(status_tree)
}
