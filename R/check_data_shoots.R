#' check table Shoots from fieldmap database for inconsistencies
#'
#' This function retrieves the important fields of table Shoots (of all periods)
#' from the given database and checks for missing data or wrong input.
#'
#' @inheritParams check_data_trees
#'
#' @return Dataframe with inconsistent data with ID's and additional columns
#' `aberrant_field` (which column is wrong) and `anomaly` (what is wrong with
#' the input)
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' check_data_shoots(path_to_fieldmapdb)
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% anti_join bind_rows filter group_by left_join mutate
#'   select summarise ungroup
#' @importFrom tidyr pivot_longer
#'
check_data_shoots <- function(database) {
  query_trees <-
    "SELECT Trees.IDPlots AS plot_id,
      qPlotType.Value3 AS plottype,
      Trees.X_m, Trees.Y_m,
      Trees.ID AS tree_measure_id,
      Trees.Height_m AS tree_height_m,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop
    FROM (Plots INNER JOIN Trees%2$s Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  query_shoots <-
    "SELECT IDPlots As plot_id,
      XTrees%2$s AS XTrees,
      YTrees%2$s AS YTrees,
      IDTrees%2$s AS tree_measure_id,
      ID AS shoot_id,
      DBH_mm AS dbh_mm,
      Height_m AS height_m,
      IntactSnag AS intact_snag,
      DecayStage_Shoots AS decay_stage_shoots,
      IUFROHght AS iufro_hght,
      IUFROVital AS iufro_vital,
      IUFROSocia AS iufro_socia
    FROM Shoots%2$s"

  query_trees_1986 <-
    "SELECT Trees.IDPlots AS plot_id,
      qPlotType.Value3 AS plottype,
      Trees.X_m, Trees.Y_m,
      Trees.ID AS tree_measure_id,
      Trees.Height_m AS tree_height_m,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop
    FROM (Plots INNER JOIN Trees_1986 Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  query_shoots_1986 <-
    "SELECT IDPlots AS plot_id,
      XTrees_1986 AS XTrees,
      YTrees_1986 AS YTrees,
      IDTrees_1986 AS tree_measure_id,
      ID AS shoot_id,
      DBH_mm AS dbh_mm,
      Height_m AS height_m,
      IntactSnag AS intact_snag,
      DecayStage_Shoots AS decay_stage_shoots,
      IUFROHght AS iufro_hght,
      IUFROVital AS iufro_vital,
      IUFROSocia AS iufro_socia
    FROM Shoots_1986"

  data_trees <- query_database(database, query_trees)
  data_shoots <- query_database(database, query_shoots)

  con <- connect_to_database(database)
  data_trees_1986 <- dbGetQuery(con, query_trees_1986) %>%
    mutate(period = 0)
  if (nrow(data_trees_1986) > 0) {
    data_trees <- data_trees %>%
      bind_rows(
        data_trees_1986
      )
  }
  data_shoots_1986 <- dbGetQuery(con, query_shoots_1986) %>%
    mutate(period = 0)
  if (nrow(data_shoots_1986) > 0) {
    data_shoots <- data_shoots %>%
      bind_rows(
        data_shoots_1986
      )
  }
  dbDisconnect(con)

  incorrect_shoots <- data_shoots %>%
    # shoots not correctly linked with trees
    left_join(
      data_shoots %>%
        anti_join(
          data_trees %>%
            filter(.data$ind_sht_cop == 12),
          by = c("plot_id", "XTrees" = "X_m", "YTrees" = "Y_m",
                 "tree_measure_id", "period")
        ) %>%
        select("plot_id", "XTrees", "YTrees", "tree_measure_id", "period") %>%
        mutate(
          link_to_layer_trees = "missing"
        ),
      by = c("plot_id", "XTrees", "YTrees", "tree_measure_id", "period")
    ) %>%
    left_join(
      data_trees %>%
        select(
          "plot_id", "X_m", "Y_m", "tree_measure_id", "period", "tree_height_m"
        ),
      by = c("plot_id", "XTrees" = "X_m", "YTrees" = "Y_m", "tree_measure_id",
             "period")
    ) %>%
    # ratio D/H (don't add item if height_m = NA)
    mutate(
      d_h = .data$dbh_mm * pi / (.data$height_m * 10),
      ratio_dbh_height = ifelse(.data$d_h < 1.5, "too low", NA),
      ratio_dbh_height =
        ifelse(.data$d_h > 15, "too high", .data$ratio_dbh_height),
      field_dbh_mm = ifelse(is.na(.data$dbh_mm), "missing", NA),
      field_dbh_mm =
        ifelse(.data$dbh_mm > 2000, "too high", .data$field_dbh_mm),
      field_height_m =
        ifelse(
          is.na(.data$height_m) & .data$intact_snag == 10 &
            !is.na(.data$tree_height_m),
          "missing", NA
        ),
      field_height_m =
        ifelse(
          !is.na(.data$height_m) & .data$height_m > 50,
          "too high", .data$field_height_m
        ),
      field_height_m =
        ifelse(
          !is.na(.data$height_m) & .data$height_m < 1.3,
          "too low", .data$field_height_m
        ),
      field_intact_snag = ifelse(is.na(.data$intact_snag), "missing", NA),
      field_decaystage_shoots =
        ifelse(is.na(.data$decay_stage_shoots), "missing", NA),
      field_iufro_hght = ifelse(is.na(.data$iufro_hght), "missing", NA),
      field_iufro_vital = ifelse(is.na(.data$iufro_vital), "missing", NA),
      field_iufro_socia = ifelse(is.na(.data$iufro_socia), "missing", NA)
    ) %>%
    pivot_longer(
      cols =
        c("link_to_layer_trees", #"ratio_dbh_height",
          starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    mutate(
      aberrant_field = gsub("^field_", "", .data$aberrant_field)
    ) %>%
    group_by(
      .data$plot_id, .data$tree_measure_id, .data$shoot_id, .data$period
    ) %>%
    summarise(
      aberrant_field = paste0(.data$aberrant_field, collapse = " / "),
      anomaly = paste0(.data$anomaly, collapse = " / ")
    ) %>%
    ungroup()

  return(incorrect_shoots)
}
