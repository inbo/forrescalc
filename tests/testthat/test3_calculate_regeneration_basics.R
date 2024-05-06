context("test calculate_regeneration basics")

path_to_fieldmapdb <-
  system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")

data_regeneration <- load_data_regeneration(path_to_fieldmapdb)
results_regeneration <- calculate_regeneration(data_regeneration)

describe("regeneration_by_plot", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_regeneration[["regeneration_by_plot"]]),
      c("plottype", "plot_id", "subplot_id", "period", "year",
        "number_of_tree_species", "nr_of_tree_species_established",
        "approx_nr_established_ha", "approx_nr_seedlings_ha",
        "approx_rubbing_damage_perc_established",
        "approx_rubbing_damage_perc_seedlings",
        "mean_number_established_ha",
        "lci_number_established_ha", "uci_number_established_ha",
        "mean_number_seedlings_ha",
        "lci_number_seedlings_ha", "uci_number_seedlings_ha",
        "mean_rubbing_damage_perc_established",
        "lci_rubbing_damage_perc_established",
        "uci_rubbing_damage_perc_established",
        "mean_rubbing_damage_perc_seedlings",
        "lci_rubbing_damage_perc_seedlings",
        "uci_rubbing_damage_perc_seedlings")
    )
  })
})

describe("regeneration_by_plot_height", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_regeneration[["regeneration_by_plot_height"]]),
      c("plottype", "plot_id", "subplot_id", "period", "year",
        "height_class", "number_of_tree_species",
        "approx_nr_regeneration_ha", "approx_rubbing_damage_perc",
        "mean_number_of_regeneration_ha",
        "lci_number_of_regeneration_ha", "uci_number_of_regeneration_ha")
    )
  })
})

describe("regeneration_by_plot_species", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_regeneration[["regeneration_by_plot_species"]]),
      c("plottype", "plot_id", "subplot_id", "period", "year", "species",
        "approx_nr_established_ha", "approx_nr_seedlings_ha",
        "approx_rubbing_damage_perc_established",
        "approx_rubbing_damage_perc_seedlings",
        "mean_number_established_ha",
        "lci_number_established_ha", "uci_number_established_ha",
        "mean_number_seedlings_ha",
        "lci_number_seedlings_ha", "uci_number_seedlings_ha",
        "mean_rubbing_damage_perc_established",
        "lci_rubbing_damage_perc_established",
        "uci_rubbing_damage_perc_established",
        "mean_rubbing_damage_perc_seedlings",
        "lci_rubbing_damage_perc_seedlings",
        "uci_rubbing_damage_perc_seedlings")
    )
  })
})

describe("regeneration_by_plot_height_species", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_regeneration[["regeneration_by_plot_height_species"]]),
      c("plottype", "plot_id", "subplot_id", "period", "year",
        "height_class", "species",
        "approx_nr_regeneration_ha", "approx_rubbing_damage_perc",
        "mean_number_of_regeneration_ha",
        "lci_number_of_regeneration_ha", "uci_number_of_regeneration_ha")
    )
  })
})

describe("regeneration_by_core_area_species", {
  it("output columns are correct", {
    expect_equal(
      colnames(results_regeneration[["regeneration_by_core_area_species"]]),
      c("plottype", "plot_id", "period", "year", "species",
        "nr_of_subplots_with_regeneration",
        "perc_subplots_with_regeneration",
        "approx_nr_established_ha", "approx_nr_seedlings_ha",
        "approx_rubbing_damage_perc_established",
        "approx_rubbing_damage_perc_seedlings",
        "mean_number_established_ha",
        "lci_number_established_ha", "uci_number_established_ha",
        "mean_number_seedlings_ha",
        "lci_number_seedlings_ha", "uci_number_seedlings_ha",
        "mean_rubbing_damage_perc_established",
        "lci_rubbing_damage_perc_established",
        "uci_rubbing_damage_perc_established",
        "mean_rubbing_damage_perc_seedlings",
        "lci_rubbing_damage_perc_seedlings",
        "uci_rubbing_damage_perc_seedlings")
    )
  })
})

describe("regeneration_by_core_area_height_species", {
  it("output columns are correct", {
    expect_equal(
      colnames(
        results_regeneration[["regeneration_by_core_area_height_species"]]
      ),
      c("plottype", "plot_id", "period", "year", "height_class", "species",
        "nr_of_subplots_with_regeneration",
        "perc_subplots_with_regeneration",
        "approx_nr_regeneration_ha", "approx_rubbing_damage_perc",
        "mean_number_of_regeneration_ha",
        "lci_number_of_regeneration_ha", "uci_number_of_regeneration_ha")
    )
  })
})
