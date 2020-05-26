#' Give diameter statistics by forest reserve, species and year
#'
#' This function calculates the diameter distribution on the level of forest reserve, species and year some values per diameter class and hectare: number of trees, basal area, and volume.
#'
#' @inheritParams calculate_dendrometry
#'
#' @return dataframe with columns forres, species, year and measures on diameter distribution (min, max, mean, median, Q1, Q3)
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' create_diam_statistics_forres_species(data_dendro)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% group_by summarise ungroup
#' @importFrom rlang .data
#' @importFrom stats median quantile
#'
create_diam_statistics_forres_species <- function(data_dendro) {
  diam_by_forres_species <- data_dendro %>%
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
