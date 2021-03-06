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
#' @importFrom dplyr %>% distinct filter group_by mutate left_join select summarise ungroup
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
    FROM Plots INNER JOIN PlotDetails_%1$deSet pd ON Plots.ID = pd.IDPlots;"

  plotinfo <-
    query_database(database, query_plot) %>%
    distinct() %>%
    mutate(survey_trees = ifelse(.data$survey_trees == 10 & !is.na(.data$survey_trees), TRUE, FALSE),
           survey_deadw = ifelse(.data$survey_deadw == 10 & !is.na(.data$survey_deadw), TRUE, FALSE),
           survey_veg = ifelse(.data$survey_veg == 10 & !is.na(.data$survey_veg), TRUE, FALSE),
           survey_reg = ifelse(.data$survey_reg == 10 & !is.na(.data$survey_reg), TRUE, FALSE),
           data_processed = ifelse(.data$data_processed == 10 & !is.na(.data$data_processed), TRUE, FALSE)
           )

  plotinfo <- plotinfo %>%
    left_join(plotinfo %>%
                filter(.data$survey_trees == TRUE) %>%
                group_by(.data$plot_id, .data$plottype, .data$forest_reserve, .data$survey_trees) %>%
                summarise(min_period = min(.data$period)) %>%

                ungroup()) %>%
    mutate(survey_number = .data$period - .data$min_period + 1) %>%
    select(-.data$min_period)

  return(plotinfo)
}
