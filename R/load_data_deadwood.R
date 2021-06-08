#' retrieves data on logs from fieldmap database
#'
#' This function queries the given database to retrieve data on deadwood (logs) (ready for use in calculate_dendrometry function).
#'
#' @param extra_variables Should additional variables such as
#' remark and common_remark be added?
#' Default is FALSE (no).
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe with data on logs
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% mutate
#' @importFrom stringr str_replace
#' @importFrom lubridate round_date year
#'
load_data_deadwood <-
  function(database, plottype = NA, forest_reserve = NA, extra_variables = FALSE) {
    selection <-
      translate_input_to_selectionquery(database, plottype, forest_reserve)
    selection <-
      ifelse(
        selection == "", selection,
        str_replace(selection, "WHERE", "AND")
      )
    add_fields <-
      ifelse(
        extra_variables,
        ", Deadwood.Remark AS remark, Deadwood.CommonRemark AS common_remark",
        ""
      )
  query_deadwood <-
    "SELECT Plots.ID AS plot_id,
      Plots.Plottype AS plottype,
      IIf(Plots.Area_ha IS NULL, Plots.Area_m2 / 10000, Plots.Area_ha) AS totalplotarea_ha,
      pd.ForestReserve AS forest_reserve,
      pd.Date_Dendro_%1$deSet AS date_dendro,
      pd.rA1 AS r_A1, pd.rA2 AS r_A2, pd.rA3 AS r_A3, pd.rA4 AS r_A4,
      pd.LengthCoreArea_m AS length_core_area_m,
      pd.WidthCoreArea_m AS width_core_area_m,
      pd.Area_ha AS core_area_ha,
      Deadwood.ID AS lying_deadw_id,
      Deadwood.Species AS species,
      Deadwood.DecayStage AS decaystage,
      Deadwood.CalcVolume_m3 AS calc_volume_m3,
      Deadwood.CalcLength_m AS calc_length_m,
      Deadw_Diam.Distance_m AS length_m,
      Deadw_Diam.Diameter_mm AS diam_mm %4$s
    FROM (((Plots INNER JOIN Deadwood%2$s Deadwood ON Plots.ID = Deadwood.IDPlots)
      INNER JOIN PlotDetails_%1$deSet pd ON Plots.ID = pd.IDPlots)
      LEFT JOIN Deadwood%2$s_Diameters Deadw_Diam
        ON Deadwood.ID = Deadw_Diam.IDDeadwood%2$s)
      WHERE Plots.ID = Deadw_Diam.IDPlots %3$s;"

  data_deadwood <-
    query_database(database, query_deadwood,
                   selection = selection, add_fields = add_fields) %>%
    group_by(
      .data$plot_id, .data$plottype, .data$totalplotarea_ha,
      .data$forest_reserve, .data$date_dendro, .data$r_A1, .data$r_A2,
      .data$r_A3, .data$r_A4, .data$length_core_area_m, .data$width_core_area_m,
      .data$core_area_ha, .data$lying_deadw_id, .data$species, .data$decaystage,
      .data$calc_volume_m3, .data$calc_length_m, .data$period
    ) %>%
    summarise(
      max_diam_mm = max(.data$diam_mm),
      min_diam_mm = min(.data$diam_mm),
      total_length_m = sum(.data$length_m)
    ) %>%
    ungroup() %>%
    mutate(
      year = year(round_date(.data$date_dendro, "year")) - 1,
      dbh_class_5cm = give_diamclass_5cm(.data$max_diam_mm),
      plotarea_ha =
        ifelse(
          .data$plottype == 20,
          (pi * .data$r_A4 ^ 2) / 10000,
          NA
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == 30,
          .data$length_core_area_m * .data$width_core_area_m,
          .data$plotarea_ha
        ),
      plotarea_ha =
        ifelse(
          .data$plottype == 30 & is.na(.data$plotarea_ha),
          .data$core_area_ha,
          .data$plotarea_ha
        ),
      plotarea_ha =
        ifelse(
          is.na(.data$plotarea_ha),
          .data$totalplotarea_ha,
          .data$plotarea_ha
        )
    )

  return(data_deadwood)
}
