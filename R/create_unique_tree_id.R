#' @title create_unique_tree_id for each individual tree over different years
#'
#' @description
#' This function creates a unique ID for each tree, that allows to group
#' (f.e. by use of `make_table_wide()`) all given information on the life stages
#' of an individual tree during different measures.
#'
#' @inheritParams calc_variables_tree_level
#'
#' @return a dataset with 1 record per tree measurement, containing the given
#' data of each tree in different years (= data_dendro) and a link
#' to a unique tree_id.
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' data_dendro <- load_data_dendrometry(path_to_fieldmapdb)
#' create_unique_tree_id(data_dendro)
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>% filter group_by left_join mutate n select ungroup
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
      n_na_dataset <- sum(is.na(dataset$tree_id))
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
      if (sum(is.na(dataset$tree_id)) < n_na_dataset) {
        dataset <- lookup_tree_id(dataset)
      }
    }
    return(dataset)
  }
  status_tree <- lookup_tree_id(status_tree)

  if (any(is.na(status_tree$tree_id))) {
    warning("Some records did not get a tree_id (NA) because the old_id was unknown in the previous period")
  }

  return(status_tree)
}
