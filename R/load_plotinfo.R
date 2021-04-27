#' retrieve generic plot data from fieldmap database
#'
#' This function queries the given database to retrieve additional data on plots
#' to save in forresdat and link with the datasets that are saved there.
#'
#' @param database name of fieldmap/access database (with specific fieldmap
#' structure) including path
#'
#' @return Dataframe with columns plot_id, plottype and forest_reserve
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' load_plotinfo("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' }
#'
#' @export
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom dplyr %>% bind_rows distinct filter group_by mutate left_join select summarise ungroup

#' @importFrom rlang .data
#'

load_plotinfo <- function(database) {
  query_plot <-
    "SELECT Plots.ID AS plot_id,
      Plots.Plottype AS plottype,
      pd.ForestReserve AS forest_reserve,
      pd.Survey_Trees_YN AS survey_trees,
      pd.Survey_Deadwood_YN AS survey_deadw,
      pd.Survey_Vegetation_YN AS survey_veg,
      pd.Survey_Regeneration_YN AS survey_reg,
      pd.DataProcessed_YN AS data_processed
    FROM Plots INNER JOIN PlotDetails_%deSet pd ON Plots.ID = pd.IDPlots;"

  con <- odbcConnectAccess2007(database)
  plotinfo <- sqlQuery(con, sprintf(query_plot, 1), stringsAsFactors = FALSE) %>%
    mutate(
      period = 1
    ) %>%
    bind_rows(
      sqlQuery(con, sprintf(query_plot, 2), stringsAsFactors = FALSE) %>%
        mutate(
          period = 2
        )
    ) %>%
    bind_rows(
      sqlQuery(con, sprintf(query_plot, 3), stringsAsFactors = FALSE) %>%
        mutate(
          period = 3
        )
    ) %>%
    distinct()

  plotinfo2 <- plotinfo %>%
    left_join(plotinfo %>%
                filter(.data$survey_trees == 10) %>%
                group_by(.data$plot_id, .data$plottype, .data$forest_reserve, .data$survey_trees) %>%
                summarise(min_period = min(.data$period)) %>%

                ungroup()) %>%
    mutate(survey_number = .data$period - .data$min_period + 1) %>%
    select(-.data$min_period)

  plotinfo <- plotinfo2

  odbcClose(con)

  return(plotinfo)
}
