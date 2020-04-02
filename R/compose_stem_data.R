#' Combine dendro data and shoot data to give detailed stem data
#'
#' This function replaces in the given dendrometric data (result from function load_data_dendrometry())
#' the diameters from coppice trees by their separate stems given in the shoot data (result from function load_data_shoots()).
#'
#' @inheritParams calculate_dendrometry
#' @param data_shoots dataframe on shoots as given from the function load_data_shoots()
#'
#' @return Dataframe with shoot data
#'
#' @examples
#' \dontrun{
#' #change path before running
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
#' @importFrom dplyr bind_rows mutate
#'
compose_stem_data <- function(data_dendro, data_shoots) {
  #omit data that could be misinterpreted if data on shoot level are added
  data_dendro_relevant <- data_dendro %>%
    select(
      -.data$Adjust_Vol_tot_m3, -.data$AdjustBasalArea_m2, -.data$TreeNumber,
      -.data$Individual, -.data$basal_area_alive_m2_ha,
      -.data$basal_area_snag_m2_ha, -.data$volume_alive_m3_ha,
      -.data$volume_snag_m3_ha, -.data$DBHClass_5cm
    )
  stem_data <- data_dendro_relevant %>%
    filter(.data$IndShtCop != 12) %>%
    bind_rows(
      data_dendro_relevant %>%
        select(-.data$DBH_mm, -.data$Height_m, -.data$decaystage) %>%
        filter(.data$IndShtCop == 12) %>%
        inner_join(data_shoots, by = c("plot_id", "tree_measure_id", "period"))
    ) %>%
    mutate(
      DBHClass_5cm = give_diamclass_5cm(.data$DBH_mm),
      basal_area_m2 = pi * (.data$DBH_mm / 2000) ^ 2
    )

  return(stem_data)
}