#' retrieve generic plot data from fieldmap database
#'
#' This function queries the given database to retrieve additional data on plots
#' to save in forresdat and link with the datasets that are saved there.
#'
#' @param database name of fieldmap/access database (with specific fieldmap
#' structure) including path
#'
#' @return Dataframe with columns plot_id, plottype, forest_reserve, period,
#' year of dendrometric survey and information on
#' (1) whether there has been a dendro, deadwood, regeneration and/or
#' vegetation survey and (2) whether the data have been processed or not.
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' load_plotinfo(path_to_fieldmapdb)
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom dplyr %>% distinct filter group_by mutate left_join select
#' @importFrom dplyr summarise ungroup
#' @importFrom lubridate month year
#' @importFrom rlang .data
#'

load_plotinfo <- function(database) {
  query_plot <-
    "SELECT Plots.ID AS plot_id,
      qPlotType.Value3 AS plottype,
      pd.ForestReserve AS forest_reserve,
      pd.Survey_Trees_YN AS survey_trees,
      pd.Survey_Deadwood_YN AS survey_deadw,
      pd.Survey_Vegetation_YN AS survey_veg,
      pd.Survey_Regeneration_YN AS survey_reg,
      pd.Date_Dendro_%1$deSet AS date_dendro,
      pd.GameImpactVegObserved AS game_impact_veg,
      pd.GameImpactRegObserved AS game_impact_reg,
      pd.DataProcessed_YN AS data_processed
    FROM (Plots
      INNER JOIN PlotDetails_%1$deSet pd ON Plots.ID = pd.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  query_plot_1986 <-
    "SELECT Plots.ID AS plot_id,
      qPlotType.Value3 AS plottype,
      pd.ForestReserve AS forest_reserve,
      pd.Survey_Trees_YN AS survey_trees,
      pd.Survey_Deadwood_YN AS survey_deadw,
      pd.Survey_Vegetation_YN AS survey_veg,
      pd.Survey_Regeneration_YN AS survey_reg,
      pd.Date_Dendro_1986 AS date_dendro,
      pd.GameImpactVegObserved AS game_impact_veg,
      pd.GameImpactRegObserved AS game_impact_reg,
      pd.DataProcessed_YN AS data_processed
    FROM (Plots
      INNER JOIN PlotDetails_1986 pd ON Plots.ID = pd.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  con <- connect_to_database(database)
  plotinfo_1986 <- dbGetQuery(con, query_plot_1986) %>%
    mutate(period = 0)
  dbDisconnect(con)

  plotinfo <-
    query_database(database, query_plot)
  if (nrow(plotinfo_1986) > 0) {
    if (inherits(con, "SQLiteConnection")) {
      plotinfo_1986 <- plotinfo_1986 %>%
        mutate(
          date_dendro = as.POSIXct(.data$date_dendro, origin = "1970-01-01")
        )
    }
    plotinfo <- plotinfo %>%
      bind_rows(
        plotinfo_1986
      )
  }
  plotinfo <- plotinfo %>%
    distinct() %>%
    mutate(
      survey_trees = (.data$survey_trees == 10 & !is.na(.data$survey_trees)),
      survey_deadw = (.data$survey_deadw == 10 & !is.na(.data$survey_deadw)),
      survey_veg = (.data$survey_veg == 10 & !is.na(.data$survey_veg)),
      survey_reg = (.data$survey_reg == 10 & !is.na(.data$survey_reg)),
      game_impact_veg = (.data$game_impact_veg == 10
                         & !is.na(.data$game_impact_veg)),
      game_impact_reg = (.data$game_impact_reg == 10
                         & !is.na(.data$game_impact_reg)),

      data_processed =
        (.data$data_processed == 10 & !is.na(.data$data_processed))
    )

  plotinfo <- plotinfo %>%
    left_join(plotinfo %>%
                filter(.data$survey_trees == TRUE) %>%
                group_by(.data$plot_id, .data$plottype,
                         .data$forest_reserve, .data$survey_trees) %>%
                summarise(min_period = min(.data$period)) %>%

                ungroup()) %>%
    mutate(
      survey_number = .data$period - .data$min_period + 1,
      year_dendro = year(.data$date_dendro) - (month(.data$date_dendro) < 5)
    ) %>%
    select(-.data$min_period, -.data$date_dendro)

  return(plotinfo)
}
