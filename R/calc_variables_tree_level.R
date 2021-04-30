#' Calculate additional variables on tree level
#'
#' This function calculates additional variables based on measurements, such as
#' \itemize{
#'  \item tree_number: the number of shoots in the tree (> 1 when coppice)
#'  \item individual: true if secondary shoot is present, false for individual tree and coppice shoot
#'  \item calc_height_m: calculated height based on `dbh_mm` and a model
#'  \item basal_area_m2
#'  \item vol_tot_m3
#'  \item vol_stem_m3
#'  \item vol_crown_m3
#'  \item dbh_mm for coppice trees
#' }
#'
#' @inheritParams calculate_dendrometry
#' @param data_shoots dataframe on shoots as given from the function load_data_shoots()
#' @param height_model dataframe with coeficients `P1` and `P2` to calculate height model
#' for each combination of `species`, `forest_reserve`, `period` and `plot_type`
#'
#' @return Dataframe with ...
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_shoots <-
#'   load_data_shoots("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' data_stems <- compose_stem_data(data_dendro, data_shoots)
#' calc_variables_tree_level(data_dendro, data_shoots, data_stems)
#' }
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom dplyr %>% group_by left_join mutate n summarise ungroup
#'
calc_variables_tree_level <-
  function(data_dendro, data_shoots, data_stems, height_model) {
  test <- data_dendro %>%
    left_join(
      data_stems %>%
        group_by(.data$plot_id, .data$tree_measure_id, .data$period) %>%
        summarise(tree_number = n()) %>%
        ungroup(),
      by = c("plot_id", "tree_measure_id", "period")
    ) %>%
    mutate(
      individual = (.data$ind_sht_cop == 11)
    ) %>%
    left_join(
      height_model,
      by = c("species", "forest_reserve", "period", "plottype")
    ) %>%
    mutate(
      calc_height_r = 1.3 + .data$P1 + .data$P2 * log(.data$dbh_mm / 10),
      calc_height_m =
        ifelse(is.na(.data$calc_height_r), .data$calc_height_m, .data$calc_height_r)
    )

  return(data_dendro)
}
