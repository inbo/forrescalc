#' check table trees from fieldmap database for inconsistencies between periods
#'
#' This function retrieves the important fields of table Trees (of all periods)
#' from the given database and checks for anomalities between periods, such as
#' zombies, shifters, outlier_height, outlier_diameter or walkers.
#'
#' @param database name of fieldmap/access database (with specific fieldmap
#' structure) including path
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
#' check_trees_evolution(path_to_fieldmapdb)
#'
#' @export
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom rlang .data
#' @importFrom dplyr %>% arrange bind_rows desc filter group_by left_join
#'   mutate n reframe select summarise transmute ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom graphics boxplot
#'
check_trees_evolution <- function(database) {
  query_trees <-
    "SELECT Trees.IDPlots AS plot_id,
      qPlotType.Value3 AS plottype,
      Trees.X_m AS x_m, Trees.Y_m AS y_m,
      Trees.ID AS tree_measure_id,
      Trees.DBH_mm AS dbh_mm,
      Trees.Height_m AS height_m,
      Trees.Species AS species,
      Trees.IntactSnag AS intact_snag,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop,
      Trees.CoppiceID AS coppice_id,
      Trees.IUFROHght, Trees.IUFROVital, Trees.IUFROSocia,
      Trees.DecayStage AS decay_stage,
      Trees.Remark AS remark,
      Trees.CommonRemark AS commonremark,
      Trees.TreeNumber AS nr_of_stems,
      Trees.Vol_tot_m3 AS vol_tot_m3,
      Trees.BasalArea_m2 AS basal_area_m2,
      Trees.OldID as old_id
    FROM (Plots INNER JOIN Trees%2$s Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  query_trees_1986 <-
    "SELECT Trees.IDPlots AS plot_id,
      qPlotType.Value3 AS plottype,
      Trees.X_m AS x_m, Trees.Y_m AS y_m,
      Trees.ID AS tree_measure_id,
      Trees.DBH_mm AS dbh_mm,
      Trees.Height_m AS height_m,
      Trees.Species AS species,
      Trees.IntactSnag AS intact_snag,
      Trees.AliveDead AS alive_dead,
      Trees.IndShtCop AS ind_sht_cop,
      Trees.CoppiceID AS coppice_id,
      Trees.IUFROHght, Trees.IUFROVital, Trees.IUFROSocia,
      Trees.DecayStage AS decay_stage,
      Trees.Remark AS remark,
      Trees.CommonRemark AS commonremark,
      Trees.TreeNumber AS nr_of_stems,
      Trees.Vol_tot_m3 AS vol_tot_m3,
      Trees.BasalArea_m2 AS basal_area_m2,
      Trees.OldID AS old_id
    FROM (Plots INNER JOIN Trees_1986 Trees ON Plots.ID = Trees.IDPlots)
      INNER JOIN qPlotType ON Plots.Plottype = qPlotType.ID;"

  data_trees <- query_database(database, query_trees)

  con <- connect_to_database(database)
  data_trees_1986 <- dbGetQuery(con, query_trees_1986) %>%
    mutate(period = 0)
  if (nrow(data_trees_1986) > 0) {
    data_trees <- data_trees %>%
      bind_rows(
        data_trees_1986
      )
  }
  dbDisconnect(con)

  incorrect_trees <- data_trees %>%
    filter(!is.na(.data$old_id)) %>%
    group_by(.data$plot_id, .data$period, .data$old_id) %>%
    mutate(n_records = n()) %>%
    ungroup() %>%
    filter(.data$n_records > 1) %>%
    transmute(
      .data$plot_id, .data$period, .data$tree_measure_id, .data$old_id,
      aberrant_field = "old_id",
      anomaly = paste0(.data$n_records, " times the same old_id")
    )

  withCallingHandlers({
    data_trees <- create_unique_tree_id(data_trees)
  }, warning = function(w) {
    if (
      startsWith(
        conditionMessage(w),
        "Some records did not get a tree_id (NA) because the old_id was unknown in the previous period"
      )
    ) invokeRestart("muffleWarning")
  })

  # is.na(tree_id) -> old_id unknown in the previous period
  incorrect_trees <- incorrect_trees %>%
    bind_rows(
      data_trees %>%
        filter(is.na(.data$tree_id)) %>%
        select("plot_id", "period", "tree_measure_id", "old_id") %>%
        mutate(
          aberrant_field = "old_id",
          anomaly = "unknown id"
        )
    )
  data_trees <- data_trees %>%
    filter(!is.na(.data$tree_id))

  trees_diff <-
    compare_periods_per_plot(
      data_trees %>%
        select(
          "period", "plot_id", "tree_id",
          "species", "alive_dead", "decay_stage", "x_m", "y_m", "dbh_mm",
          "height_m"
        ),
      measure_vars =
        c("species", "alive_dead", "decay_stage", "x_m", "y_m", "dbh_mm",
          "height_m")
    ) %>%
    mutate(
      distance_m_diff = sqrt(.data$x_m_diff ^ 2 + .data$y_m_diff ^ 2)
    ) %>%
    left_join(
      data_trees %>%
        arrange(desc(.data$period)) %>%
        group_by(.data$tree_id) %>%
        summarise(
          species = .data$species[1],
          alive_dead = .data$alive_dead[1]
        ) %>%
        ungroup(),
      by = "tree_id"
    )
  incorrect_trees <- incorrect_trees %>%
    mutate(
      period = as.character(.data$period),
      tree_measure_id = as.character(.data$tree_measure_id)
    ) %>%
    bind_rows(
      trees_diff %>%
        mutate(
          field_species = ifelse(.data$species_diff != 0, "shifter", NA),
          field_alive_dead = ifelse(.data$alive_dead_diff == -1, "zombie", NA),
          field_decay_stage =
            ifelse(
              !(.data$decay_stage_diff >= 0 & .data$decay_stage_diff <= 5) &
                  .data$alive_dead_diff == 0 |
                .data$alive_dead_diff == 1 & !(.data$decay_stage_diff < 0 &
                    .data$decay_stage_diff >= -6),
              "wrong shift",
              NA
            )
        ) %>%
        left_join(
          trees_diff %>%
            reframe(
              distance_m_diff = (boxplot(.data$distance_m_diff))$out
            ) %>%
            distinct() %>%
            mutate(field_coordinates = "walker"),
          by = "distance_m_diff"
        ) %>%
        left_join(
          trees_diff %>%
            filter(
              .data$species != 51,
              .data$alive_dead == 11
            ) %>%
            reframe(
              dbh_mm_diff = (boxplot(.data$dbh_mm_diff))$out
            ) %>%
            distinct() %>%
            mutate(field_dbh_mm = "outlier_diameter_total"),
          by = "dbh_mm_diff"
        ) %>%
        left_join(
          trees_diff %>%
            filter(.data$alive_dead == 11) %>%
            group_by(.data$species) %>%
            reframe(
              dbh_mm_diff = (boxplot(.data$dbh_mm_diff))$out
            ) %>%
            ungroup() %>%
            distinct() %>%
            mutate(field_dbh_mm = "outlier_diameter_species"),
          by = c("dbh_mm_diff", "species")
        ) %>%
        mutate(
          field_dbh_mm =
            ifelse(
              is.na(.data$field_dbh_mm.x),
              ifelse(is.na(.data$field_dbh_mm.y), NA, .data$field_dbh_mm.y),
              ifelse(
                is.na(.data$field_dbh_mm.y), .data$field_dbh_mm.x,
                "outlier_diameter"
              )
            ),
          field_dbh_mm.x = NULL, field_dbh_mm.y = NULL
        ) %>%
        left_join(
          trees_diff %>%
            filter(
              .data$species != 51,
              .data$alive_dead == 11
            ) %>%
            reframe(
              height_m_diff = (boxplot(.data$height_m_diff))$out
            ) %>%
            distinct() %>%
            mutate(field_height_m = "outlier_height_total"),
          by = "height_m_diff"
        ) %>%
        left_join(
          trees_diff %>%
            filter(.data$alive_dead == 11) %>%
            group_by(.data$species) %>%
            reframe(
              height_m_diff = (boxplot(.data$height_m_diff))$out
            ) %>%
            ungroup() %>%
            distinct() %>%
            mutate(field_height_m = "outlier_height_species"),
          by = c("height_m_diff", "species")
        ) %>%
        mutate(
          field_height_m =
            ifelse(
              is.na(.data$field_height_m.x),
              ifelse(is.na(.data$field_height_m.y), NA, .data$field_height_m.y),
              ifelse(
                is.na(.data$field_height_m.y), .data$field_height_m.x,
                "outlier_height"
              )
            ),
          field_height_m.x = NULL, field_height_m.y = NULL
        ) %>%
        select(
          "plot_id", "tree_id", "period" = .data$period_diff, "species",
          starts_with("field_")
        ) %>%
        pivot_longer(
          cols = starts_with("field_"),
          names_to = "aberrant_field",
          values_to = "anomaly",
          values_drop_na = TRUE
        ) %>%
        mutate(
          aberrant_field = gsub("^field_", "", .data$aberrant_field)
        ) %>%
        group_by(.data$plot_id, .data$tree_id, .data$period) %>%
        summarise(
          aberrant_field = paste0(.data$aberrant_field, collapse = " / "),
          anomaly = paste0(.data$anomaly, collapse = " / ")
        ) %>%
        ungroup() %>%
        mutate(
          period_end = as.numeric(substring(.data$period, 1, 1)),
          period_start = as.numeric(substring(.data$period, 3, 3))
        ) %>%
        left_join(
          data_trees %>%
            select("plot_id", "tree_measure_id", "period", "tree_id"),
          by = c("plot_id", "tree_id", "period_end" = "period")
        ) %>%
        left_join(
          data_trees %>%
            select("plot_id", "tree_measure_id", "period", "tree_id"),
          by = c("plot_id", "tree_id", "period_start" = "period"),
          suffix = c("_end", "_start")
        ) %>%
        mutate(
          tree_measure_id = paste(.data$tree_measure_id_end, .data$tree_measure_id_start, sep = "_"),
          period_end = NULL, period_start = NULL,
          tree_measure_id_end = NULL, tree_measure_id_start = NULL
        )
    )

  return(incorrect_trees)
}
