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
#' check_data_shoots(path_to_fieldmapdb, forest_reserve = "Everzwijnbad")
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% anti_join bind_rows distinct filter left_join mutate
#'   select
#' @importFrom tidyr pivot_longer
#'
check_data_shoots <- function(database, forest_reserve = "all") {
  selection <-
    ifelse(
      forest_reserve == "all", "",
      paste0("WHERE pd.ForestReserve = '", forest_reserve, "'")
    )
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
    "SELECT shoots.IDPlots As plot_id,
      shoots.XTrees%2$s AS XTrees,
      shoots.YTrees%2$s AS YTrees,
      shoots.IDTrees%2$s AS tree_measure_id,
      shoots.ID AS shoot_id,
      shoots.DBH_mm AS dbh_mm,
      shoots.Height_m AS height_m,
      shoots.IntactSnag AS intact_snag,
      shoots.DecayStage_Shoots AS decay_stage_shoots,
      shoots.IUFROHght AS iufro_hght,
      shoots.IUFROVital AS iufro_vital,
      shoots.IUFROSocia AS iufro_socia
    FROM Shoots%2$s shoots
      INNER JOIN Plotdetails_%1$deSet pd ON shoots.IDPlots = pd.IDPlots
    %3$s;"

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

  query_shoots_1986 <- sprintf(
    "SELECT shoots.IDPlots AS plot_id,
      shoots.XTrees_1986 AS XTrees,
      shoots.YTrees_1986 AS YTrees,
      shoots.IDTrees_1986 AS tree_measure_id,
      shoots.ID AS shoot_id,
      shoots.DBH_mm AS dbh_mm,
      shoots.Height_m AS height_m,
      shoots.IntactSnag AS intact_snag,
      shoots.DecayStage_Shoots AS decay_stage_shoots,
      shoots.IUFROHght AS iufro_hght,
      shoots.IUFROVital AS iufro_vital,
      shoots.IUFROSocia AS iufro_socia
    FROM Shoots_1986 shoots
      INNER JOIN Plotdetails_1986 pd ON shoots.IDPlots = pd.IDPlots
    %1$s;",
    selection
  )

  data_trees <- query_database(database, query_trees)
  data_shoots <- query_database(database, query_shoots, selection)

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
          "plot_id", "X_m", "Y_m", "tree_measure_id", "period", "tree_height_m",
          "alive_dead"
        ),
      by = c("plot_id", "XTrees" = "X_m", "YTrees" = "Y_m", "tree_measure_id",
             "period")
    ) %>%
    # ratio D/H (don't add item if height_m = NA)
    mutate(
      ratio_dbh_height = round(.data$dbh_mm * pi / (.data$height_m * 10), 1),
      field_ratio_dbh_height =
        ifelse(
          .data$ratio_dbh_height < 1.5 & .data$intact_snag == 11,
          "stem too thin and high", NA),
      field_ratio_dbh_height =
        ifelse(
          .data$ratio_dbh_height > 15 & .data$intact_snag == 11,
          "stem too thick and low", .data$field_ratio_dbh_height
        ),
      field_dbh_mm = ifelse(is.na(.data$dbh_mm), "missing", NA),
      field_dbh_mm =
        ifelse(
          !is.na(.data$dbh_mm) & .data$dbh_mm > 2000, "too high",
          .data$field_dbh_mm
        ),
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
      field_intact_snag =
        ifelse(
          !is.na(.data$intact_snag) & !.data$intact_snag %in% c(10, 11),
          "not in lookuplist", .data$field_intact_snag
        ),
      field_decay_stage_shoots =
        ifelse(
          is.na(.data$decay_stage_shoots) & .data$alive_dead == 12, "missing",
          NA
        ),
      field_decay_stage_shoots =
        ifelse(
          !is.na(.data$decay_stage_shoots) &
            !.data$decay_stage_shoots %in% c(10, 11, 12, 13, 14, 15, 16),
          "not in lookuplist",
          .data$field_decay_stage_shoots
        ),
      field_decay_stage_shoots =
        ifelse(
          .data$decay_stage_shoots %in% c(10, 11, 12, 13, 14, 15) &
            .data$alive_dead == 11 & !is.na(.data$decay_stage_shoots),
          "tree alive",
          .data$field_decay_stage_shoots),
      field_decay_stage_shoots =
        ifelse(
          (.data$decay_stage_shoots == 16 | is.na(.data$decay_stage_shoots)) &
            .data$alive_dead == 12 & is.na(.data$field_decay_stage_shoots),
          "tree not alive",
          .data$field_decay_stage_shoots
        ),
      field_iufro_hght = ifelse(is.na(.data$iufro_hght) &
                                  .data$alive_dead == 11 & !period %in% c(0,1),
                                "missing", NA),
      field_iufro_hght =
        ifelse(
          !.data$iufro_hght %in% c(10, 20, 30, 40) & !is.na(.data$iufro_hght),
          "not in lookuplist", .data$field_iufro_hght
        ),
      field_iufro_hght =
        ifelse(
          .data$iufro_hght %in% c(10, 20, 30) & .data$alive_dead == 12 &
            !is.na(.data$iufro_hght),
          "tree not alive",
          .data$field_iufro_hght
        ),
      field_iufro_hght =
        ifelse(
          .data$iufro_hght == 40 & .data$alive_dead == 11 &
            !is.na(.data$iufro_hght),
          "tree alive", .data$field_iufro_hght
        ),
      field_iufro_vital = ifelse(is.na(.data$iufro_vital) &
                                   .data$alive_dead == 11 & !period %in% c(0,1),
                                 "missing", NA),
      field_iufro_vital =
        ifelse(
          !.data$iufro_vital %in% c(10, 20, 30, 40) & !is.na(.data$iufro_vital),
          "not in lookuplist", .data$field_iufro_vital
        ),
      field_iufro_vital =
        ifelse(
          .data$iufro_vital %in% c(10, 20, 30) & .data$alive_dead == 12 &
            !is.na(.data$iufro_vital),
          "tree not alive",
          .data$field_iufro_vital
        ),
      field_iufro_vital =
        ifelse(
          .data$iufro_vital == 40 & .data$alive_dead == 11 &
            !is.na(.data$iufro_vital),
          "tree alive", .data$field_iufro_vital
        ),
      field_iufro_socia = ifelse(is.na(.data$iufro_socia) &
                                   .data$alive_dead == 11 & !period %in% c(0,1),
                                 "missing", NA),
      field_iufro_socia =
        ifelse(
          !.data$iufro_socia %in% c(10, 20, 30, 40) & !is.na(.data$iufro_socia),
          "not in lookuplist", .data$field_iufro_socia
        ),
      field_iufro_socia =
        ifelse(
          .data$iufro_socia %in% c(10, 20, 30) & .data$alive_dead == 12 &
            !is.na(.data$iufro_socia),
          "tree not alive",
          .data$field_iufro_socia
        ),
      field_iufro_socia =
        ifelse(
          .data$iufro_socia == 40 & .data$alive_dead == 11 &
            !is.na(.data$iufro_socia),
          "tree alive", .data$field_iufro_socia
        )
    ) %>%
    pivot_longer(
      cols =
        c("link_to_layer_trees", starts_with("field_")),
      names_to = "aberrant_field",
      values_to = "anomaly",
      values_drop_na = TRUE
    ) %>%
    mutate(
      aberrant_field = gsub("^field_", "", .data$aberrant_field)
    ) %>%
    pivot_longer(
      cols =
        !c("plot_id", "tree_measure_id", "shoot_id", "period", "aberrant_field",
           "anomaly"),
      names_to = "varname",
      values_to = "aberrant_value"
    ) %>%
    filter(
      .data$aberrant_field == .data$varname |
        .data$aberrant_field %in% c("link_to_layer_trees")
    ) %>%
    mutate(
      aberrant_value =
        ifelse(
          .data$aberrant_field %in% c("link_to_layer_trees"),
          NA,
          .data$aberrant_value
        )
    ) %>%
    select(-"varname") %>%
    distinct()

  return(incorrect_shoots)
}
