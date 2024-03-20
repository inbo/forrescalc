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
#' data_dendro <-
#'   load_data_dendrometry(path_to_fieldmapdb, extra_variables = TRUE)
#' create_unique_tree_id(data_dendro)
#'
#' @export
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% distinct filter group_by left_join mutate n select
#'   transmute ungroup
#' @importFrom rlang .data
#' @importFrom tidyselect ends_with
#'
create_unique_tree_id <- function(data_dendro) {
  if (has_name(data_dendro, "coppice_id")) {
    data_dendro <- data_dendro %>%
      left_join(
        data_dendro %>%
          group_by(.data$plot_id, .data$coppice_id, .data$period) %>%
          mutate(n_records = n()) %>%
          ungroup() %>%
          filter(
            !is.na(.data$coppice_id),
            .data$n_records > 1
          ) %>%
          group_by(.data$plot_id, .data$coppice_id) %>%
          mutate(min_period = min(.data$period)) %>%
          ungroup() %>%
          filter(.data$period == .data$min_period) %>%
          select(
            "plot_id", "coppice_id", "tree_measure_id", "alive_dead", "period",
            "old_id"
          ),
        by = c("plot_id", "coppice_id", "period"),
        suffix = c("", "_coupled"),
        relationship = "many-to-many"
      ) %>%
      filter(
        is.na(.data$tree_measure_id_coupled) |
          .data$tree_measure_id_coupled != .data$tree_measure_id
      ) %>%
      mutate(
        suffix =
          ifelse(
            .data$alive_dead == 11 & .data$alive_dead_coupled == 12 &
              !is.na(.data$alive_dead_coupled),
            "a", ""
          ),
        suffix =
          ifelse(
            .data$alive_dead == 12 & .data$alive_dead_coupled == 11 &
              !is.na(.data$alive_dead_coupled),
            "b", .data$suffix
          ),
        old_id_updated =
          ifelse(
            .data$suffix == "b" & is.na(.data$old_id),
            .data$old_id_coupled,
            .data$old_id
          ),
        tree_measure_id_updated =
          ifelse(
            .data$suffix == "b" & is.na(.data$old_id) &
              !is.na(.data$tree_measure_id_coupled),
            .data$tree_measure_id_coupled,
            .data$tree_measure_id
          )
      ) %>%
      select(-ends_with("_coupled"))
  } else {
    warning("As no coppice_id is given, alive and dead shoots of one tree are considered as different trees.  Use extra_variables = TRUE in load_data_dendrometry() to load data with coppice_id included")  #nolint: line_length_linter
    data_dendro <- data_dendro %>%
      mutate(
        suffix = "",
        old_id_updated = .data$old_id,
        tree_measure_id_updated = .data$tree_measure_id
      )
  }
  status_tree <- data_dendro %>%
    mutate(
      tree_id =
        ifelse(
          is.na(.data$old_id_updated),
          paste(.data$period, .data$plot_id, .data$tree_measure_id_updated,
                .data$suffix, sep = "_"),
          NA
        ),
      tree_id = gsub("^(.*)_$", "\\1", .data$tree_id)
    )
  lookup_tree_id <- function(dataset) {
    if (any(is.na(dataset$tree_id))) {
      n_na_dataset <- sum(is.na(dataset$tree_id))
      dataset <- dataset %>%
        left_join(
          dataset %>%
            transmute(
              .data$plot_id, tree_measure_id_updated = .data$tree_measure_id,
              .data$tree_id, old_id_updated = .data$old_id,
              period = .data$period + 1
            ) %>%
            filter(!is.na(.data$tree_id)) %>%
            distinct(),
          by = c("plot_id", "old_id_updated" = "tree_measure_id_updated",
                 "period"),
          suffix = c("", "_oldid")
        ) %>%
        mutate(
          tree_id =
            ifelse(
              is.na(.data$tree_id) & !is.na(.data$tree_id_oldid),
              ifelse(
                .data$suffix == "", .data$tree_id_oldid,
                paste(.data$tree_id_oldid, .data$suffix, sep = "_")
              ),
              .data$tree_id
            )
        ) %>%
        select(-.data$tree_id_oldid, -.data$old_id_updated_oldid)
      if (sum(is.na(dataset$tree_id)) < n_na_dataset) {
        dataset <- lookup_tree_id(dataset)
      }
    }
    return(dataset)
  }
  status_tree <- lookup_tree_id(status_tree) %>%
    select(-"suffix", -"old_id_updated", -"tree_measure_id_updated")

  if (any(is.na(status_tree$tree_id))) {
    warning(
      "Some records did not get a tree_id (NA) because the old_id was unknown in the previous period" #nolint: line_length_linter
    )
  }

  return(status_tree)
}
