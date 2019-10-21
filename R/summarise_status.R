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
#' @importFrom rlang .data
#'
summarise_status <- function(data_dendro) {
  status_tree <- data_dendro %>%
    mutate(  #dit nog bespreken met Peter, en vooral vragen: hoe gaat dit gebeuren in periode 3?
      tree_id =
        ifelse(
          is.na(.data$OldID),
          paste(.data$period, .data$plot_id, .data$tree_measure_id, sep = "_"),
          paste(1, .data$plot_id, .data$OldID, sep = "_")
        )
    ) %>%
    select(
      .data$plot_id, .data$period, .data$year, .data$species, .data$decaystage,
      .data$tree_id, .data$tree_measure_id, .data$DBH_mm, .data$AliveDead,
      .data$Adjust_Vol_tot_m3
    )

  return(status_tree)
}
