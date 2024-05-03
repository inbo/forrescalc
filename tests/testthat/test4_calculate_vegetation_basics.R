context("test calculate_vegetation basics")

path_to_fieldmapdb <-
  system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")

data_vegetation <- load_data_vegetation(path_to_fieldmapdb)
data_herblayer <- load_data_herblayer(path_to_fieldmapdb)
results_vegetation <- calculate_vegetation(data_vegetation, data_herblayer)

describe("vegetation_by_plot", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_vegetation[["vegetation_by_plot"]]),
      c("plottype", "plot_id", "subplot_id",
        "period", "year_main_survey", "date_vegetation",
        "number_of_species", "cumm_herb_coverage_class_average_perc",
        "moss_cover_min", "moss_cover_max", "moss_cover_mid",
        "herb_cover_min", "herb_cover_max", "herb_cover_mid",
        "shrub_cover_min", "shrub_cover_max", "shrub_cover_mid",
        "tree_cover_min", "tree_cover_max", "tree_cover_mid",
        "waterlayer_cover_min", "waterlayer_cover_max", "waterlayer_cover_mid",
        "soildisturbance_game_cover_min", "soildisturbance_game_cover_max",
        "soildisturbance_game_cover_mid",
        "cumulated_canopy_cover_min", "cumulated_canopy_cover_max",
        "cumulated_canopy_cover_mid")
    )
  })
})

describe("vegetation_by_core_area_species", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_vegetation[["vegetation_by_core_area_species"]]),
      c("plottype", "plot_id", "period", "year", "species",
        "number_of_subplots_with_vegetation", "perc_of_subplots",
        "number_of_subplots_browsed", "number_of_subplots_seriously_browsed",
        "perc_of_subplots_browsed", "perc_of_subplots_seriously_browsed",
        "mean_coverage_class_average_perc")
    )
  })
})
