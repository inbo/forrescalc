#' give the statuses of each individual in different years
#'
#' This function groups all information on the life stages of an individual tree during different measures.
#'
#' @inheritParams calculate_dendrometry
#'
#' @return a dataset with 1 record for each tree, containing the decay stage in different years
#'
#' @export
#'
#' @importFrom dplyr %>% filter inner_join mutate rename select
#' @importFrom magrittr %<>%
#' @importFrom rlang .data
#'
summarise_status <- function(data_dendro) { #klopt niet, even terug uitzoeken naarwaar dat verwijst
  data_dendro %<>%
    mutate(
      tree_id_old = ifelse(is.na(.data$OldID), .data$tree_id, .data$OldID),
      year_series = paste0(.data$year, "(", .data$series, ")")
    ) %>%
    select(
      .data$plot_id, .data$series, .data$species, .data$decaystage,
      .data$tree_id, .data$OldID, .data$year_series
    )
  status_tree <- data_dendro %>%
    filter(.data$series == 1) %>%
    select(-.data$series, -.data$OldID) %>%
    inner_join(
      data_dendro %>%
        filter(.data$series == 2) %>%
        select(-.data$series),
      by = c("plot_id", "species", "tree_id" = "OldID"),
      suffix = c("_1", "_2")
    ) %>%
    rename(tree_id_1 = .data$tree_id)

  return(status_tree)
}
