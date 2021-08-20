#' @title create_unique_tree_id for each individual tree over different years
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
#' create_unique_tree_id(data_dendro)
#' }
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% filter left_join mutate select
#' @importFrom rlang .data
#'
create_unique_tree_id <- function(data_dendro) {
  status_tree <- data_dendro %>%
    mutate(
      tree_id =
        ifelse(
          is.na(.data$old_id),
          paste(.data$period, .data$plot_id, .data$tree_measure_id, sep = "_"),
          NA
        )
    )
  lookup_tree_id <- function(dataset) {
    if (any(is.na(dataset$tree_id))) {
      dataset <- dataset %>%
        left_join(
          dataset %>%
            transmute(
              .data$plot_id, .data$tree_measure_id, .data$tree_id, .data$old_id,
              period = .data$period + 1
            ) %>%
            filter(!is.na(.data$tree_id)) %>%
            distinct(),
          by = c("plot_id", "old_id" = "tree_measure_id", "period"),
          suffix = c("", "_oldid")
        ) %>%
        mutate(
          tree_id =
            ifelse(
              is.na(.data$tree_id),
              .data$tree_id_oldid,
              .data$tree_id
            )
        ) %>%
        select(-.data$tree_id_oldid, -.data$old_id_oldid)
      dataset <- lookup_tree_id(dataset)
    }
    return(dataset)
  }
  status_tree <- lookup_tree_id(status_tree)

  return(status_tree)
}
