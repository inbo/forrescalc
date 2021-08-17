#' Combine dendro data and shoot data to give detailed stem data
#'
#' This function replaces in the given dendrometric data (result from function load_data_dendrometry())
#' the diameters, height, decaystage and info on intact/snag from coppice trees
#' by their separate stems given in the shoot data (result from function load_data_shoots()).
#'
#' @inheritParams calculate_dendrometry
#'
#' @return Dataframe with shoot data
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_shoots <-
#'   load_data_shoots("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' compose_stem_data(data_dendro, data_shoots)
#' }
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% bind_rows filter inner_join mutate select
#' @importFrom assertthat has_name
#'
compose_stem_data <- function(data_dendro, data_shoots) {
  #omit data that could be misinterpreted if data on shoot level are added
  data_dendro_relevant <- data_dendro %>%
    select(
      -.data$tree_number, -.data$dbh_class_5cm
    )
  stem_data <- data_dendro_relevant %>%
    filter(.data$ind_sht_cop != 12) %>%
    bind_rows(
      data_dendro_relevant %>%
        select(-.data$dbh_mm, -.data$height_m,
               -.data$intact_snag, -.data$decaystage) %>%
        filter(.data$ind_sht_cop == 12) %>%
        inner_join(data_shoots, by = c("plot_id", "tree_measure_id", "period"))
    ) %>%
    mutate(
      dbh_class_5cm = give_diamclass_5cm(.data$dbh_mm),
      basal_area_m2 = pi * (.data$dbh_mm / 2000) ^ 2
    )

  if (
    has_name(
      stem_data,
      c("iufro_hght", "iufro_vital", "iufro_socia", "iufro_hght_shoots",
        "iufro_vital_shoots", "iufro_socia_shoots")
    )
  ) {
    stem_data <- stem_data %>%
      mutate(
        iufro_hght =
          ifelse(is.na(.data$iufro_hght_shoots),
                 .data$iufro_hght, .data$iufro_hght_shoots),
        iufro_hght_shoots = NULL,
        iufro_vital =
          ifelse(is.na(.data$iufro_vital_shoots),
                 .data$iufro_vital, .data$iufro_vital_shoots),
        iufro_vital_shoots = NULL,
        iufro_socia =
          ifelse(is.na(.data$iufro_socia_shoots),
                 .data$iufro_socia, .data$iufro_socia_shoots),
        iufro_socia_shoots = NULL
      )
  }

  return(stem_data)
}
