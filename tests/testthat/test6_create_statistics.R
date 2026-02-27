context("test create_statistics()")

library(dplyr)
library(tidyr)

path_to_fieldmapdb <-
  system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")

data_dendro <- load_data_dendrometry(path_to_fieldmapdb)
data_deadwood <- load_data_deadwood(path_to_fieldmapdb)
data_shoots <- load_data_shoots(path_to_fieldmapdb)
height_model <- load_height_models()
plotinfo <- load_plotinfo(path_to_fieldmapdb) |>
  filter(.data$plot_id %in% c(101, 11000))
results_dendrometry <-
  calculate_dendrometry(
    data_dendro, data_deadwood, data_shoots, height_model, plotinfo
  )
dataset <- results_dendrometry[["dendro_by_plot"]] |>
  right_join(plotinfo, by = c("plot_id", "plottype", "period"))
variables_for_statistics <- dataset %>%
  select(contains(c("tree", "vol")), -contains(c("survey", "40"))) |>
  names()

describe("create_statistics", {
  it("check result", {
    expect_equal(
      create_statistics(
        dataset = dataset,
        level = c("period", "forest_reserve"),
        variables = c("number_of_trees_ha", "vol_alive_m3_ha"),
        include_year_range = TRUE
      ),
      dataset |>
        select(
          "period", "forest_reserve", "year", "number_of_trees_ha",
          "vol_alive_m3_ha"
        ) |>
        pivot_longer(
          cols = c("number_of_trees_ha", "vol_alive_m3_ha"),
          names_to = "variable",
          values_to = "mean"
        ) |>
        transmute(
          .data$period, .data$forest_reserve,
          min_year = .data$year,
          max_year = .data$year,
          .data$variable,
          n_obs = 1,
          .data$mean,
          variance = NA_real_,
          lci = NA_real_,
          uci = NA_real_,
          logaritmic = FALSE
        ) |>
        arrange(.data$period, .data$forest_reserve)
    )
    expect_equal(
      create_statistics(
        dataset = dataset,
        level = c("period", "forest_reserve"),
        variables = c("number_of_trees_ha", "vol_alive_m3_ha"),
        include_year_range = TRUE,
        na_rm = TRUE
      ),
      dataset |>
        select(
          "period", "forest_reserve", "year", "number_of_trees_ha",
          "vol_alive_m3_ha"
        ) |>
        pivot_longer(
          cols = c("number_of_trees_ha", "vol_alive_m3_ha"),
          names_to = "variable",
          values_to = "mean"
        ) |>
        transmute(
          .data$period, .data$forest_reserve,
          min_year = .data$year,
          max_year = .data$year,
          .data$variable,
          n_obs = 1,
          .data$mean,
          variance = NA_real_,
          lci = NA_real_,
          uci = NA_real_,
          logaritmic = FALSE
        ) |>
        arrange(.data$period, .data$forest_reserve)
    )
  })
  it("check behaviour if NA", {
    expect_equal(
      create_statistics(
        dataset = dataset,
        level = c("period", "forest_reserve"),
        variables = c("number_of_trees_ha", "vol_deadw_m3_ha"),
        include_year_range = TRUE
      ),
      dataset |>
        select(
          "period", "forest_reserve", "year", "number_of_trees_ha",
          "vol_deadw_m3_ha"
        ) |>
        pivot_longer(
          cols = c("number_of_trees_ha", "vol_deadw_m3_ha"),
          names_to = "variable",
          values_to = "mean"
        ) |>
        transmute(
          .data$period, .data$forest_reserve,
          min_year = .data$year,
          max_year = .data$year,
          .data$variable,
          n_obs = 1,
          .data$mean,
          variance = NA_real_,
          lci = NA_real_,
          uci = NA_real_,
          logaritmic = FALSE
        ) |>
        arrange(.data$period, .data$forest_reserve)
    )
    expect_equal(
      create_statistics(
        dataset = dataset,
        level = c("period", "forest_reserve"),
        variables = c("number_of_trees_ha", "vol_deadw_m3_ha"),
        include_year_range = TRUE,
        na_rm = TRUE
      ),
      dataset |>
        select(
          "period", "forest_reserve", "year", "number_of_trees_ha",
          "vol_deadw_m3_ha"
        ) |>
        pivot_longer(
          cols = c("number_of_trees_ha", "vol_deadw_m3_ha"),
          names_to = "variable",
          values_to = "mean"
        ) |>
        transmute(
          .data$period, .data$forest_reserve,
          min_year = .data$year,
          max_year = .data$year,
          .data$variable,
          n_obs = 1,
          .data$mean,
          variance = NA_real_,
          lci = NA_real_,
          uci = NA_real_,
          logaritmic = FALSE
        ) |>
        arrange(.data$period, .data$forest_reserve)
    )
  })
})
