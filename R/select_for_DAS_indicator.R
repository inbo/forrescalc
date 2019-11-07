#' @title select relevant plots/reserves to calculate DAS indicator
#'
#' @description
#' This function does the first step(s) of the calculation of the DAS indicator, namely selecting the relevant forest reserves or plots based on the following criteria:
#'
#' @template selection_criteria_for_DAS
#'
#' @param data_to_select dataframe with at least all grouping_vars and the variables DBH_mm, species, basal_area_alive_m2_ha, and as records preferably only living trees (they are not filtered out here but they should not be in here to meet the requirements of the DAS indicator)
#' @param grouping_vars vector with variables that should be grouped on during the selection steps.
#'
#' @return dataframe with the grouping vars in which records are removed that do not meet the above described criteria
#'
#' @examples
#' \dontrun{
#' #change path before running
#' data_dendro <-
#'   load_data_dendrometry("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
#' select_for_DAS_indicator(data_dendro)
#' }
#'
#' @export
#'
#' @importFrom dplyr %>% distinct filter group_by_at inner_join left_join select_at summarise ungroup
#' @importFrom readr read_delim
#' @importFrom rlang .data
#'
select_for_DAS_indicator <- function(data_to_select, grouping_vars) {
  selected_groups <- data_to_select  %>%
    group_by_at(grouping_vars) %>%
    summarise(
      DBH_mm_average = mean(.data$DBH_mm)
    ) %>%
    ungroup() %>%
    filter(.data$DBH_mm_average > 200 & .data$DBH_mm_average < 700) %>%
    select_at(grouping_vars) %>%
    distinct() %>%
    inner_join(data_to_select, by = grouping_vars) %>%
    left_join(
      read_delim(
        system.file("extdata/DAS_tree_groups.csv", package = "forrescalc"),
        delim = ";", col_types = "cd"
      ),
      by = "species"
    ) %>%
    group_by_at(c(grouping_vars, "group", "plot_id")) %>%
    summarise(
      basal_area_m2_ha = sum(.data$basal_area_alive_m2_ha)
    ) %>%
    ungroup() %>%
    group_by_at(c(grouping_vars, "group")) %>%
    summarise(
      basal_area_m2_ha = mean(.data$basal_area_m2_ha)
    ) %>%
    ungroup() %>%
    group_by_at(grouping_vars) %>%
    mutate(
      basal_area_proportion =
        .data$basal_area_m2_ha / sum(.data$basal_area_m2_ha)
    ) %>%
    ungroup() %>%
    filter(
      !is.na(.data$group) | .data$basal_area_proportion <= 0.95,
      is.na(.data$group) | .data$basal_area_proportion <= 0.98
    ) %>%
    group_by_at(grouping_vars) %>%
    summarise(
      total_proportion = sum(.data$basal_area_proportion)
    ) %>%
    ungroup() %>%
    filter(.data$total_proportion > 0.9) %>% #by writing == 1, some results are dropped due to rounding
    select_at(grouping_vars)

  return(selected_groups)
}
