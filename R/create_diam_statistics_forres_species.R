#' Give diameter statistics by forest reserve, species and year
#'
#' This function calculates the diameter distribution on the level of
#' forest reserve, species and year
#'
#' @inheritParams calc_variables_stem_level
#' @inheritParams calc_variables_tree_level
#'
#' @return dataframe with columns forest_reserve, species, year and measures on
#' diameter distribution (min, max, mean, median, Q1, Q3)
#'
#' @examples
#' library(forrescalc)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' data_dendro <- load_data_dendrometry(path_to_fieldmapdb)
#' data_shoots <- load_data_shoots(path_to_fieldmapdb)
#' data_stems <- compose_stem_data(data_dendro, data_shoots)
#' calc_diam_statistics_species(data_stems)
#'
#' @export
#'
#' @importFrom dplyr %>% group_by summarise ungroup
#' @importFrom rlang .data
#' @importFrom stats median quantile
#'
calc_diam_statistics_species <- function(data_stems) {
  diam_by_forres_species <- data_stems %>%
    group_by(
      .data$forest_reserve, .data$year, .data$period, .data$species
    ) %>%
    summarise(
      dbh_min_mm = min(.data$dbh_mm),
      dbh_max_mm = max(.data$dbh_mm),
      dbh_mean_mm = mean(.data$dbh_mm),
      dbh_median_mm = median(.data$dbh_mm),
      dbh_Q1_mm = quantile(.data$dbh_mm, probs = 0.25),
      dbh_Q3_mm = quantile(.data$dbh_mm, probs = 0.75)
    ) %>%
    ungroup()

  return(diam_by_forres_species)
}
