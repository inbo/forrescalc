#' couple data of each individual tree in different years
#'
#' This function groups all given information on the life stages of an individual tree during different measures.
#'
#' @inheritParams calculate_dendrometry
#'
#' @return a dataset with 1 record per measurement, containing the given data of each tree in different years
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' create_overview_status(data_dendro)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% filter inner_join mutate rename select
#' @importFrom rlang .data
#'
create_overview_status <- function(data_dendro) {
  status_tree <- data_dendro %>%
    mutate(  #dit nog bespreken met Peter, en vooral vragen: hoe gaat dit gebeuren in periode 3?
      tree_id =
        ifelse(
          is.na(.data$OldID),
          paste(.data$period, .data$plot_id, .data$tree_measure_id, sep = "_"),
          paste(1, .data$plot_id, .data$OldID, sep = "_")
        )
    )

  return(status_tree)
}
