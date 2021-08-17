#' Add records with value zero for missing variable combinations in a given dataset
#'
#' @description
#' Datasets for which this package has been developed, typically contain measurements of observations.
#' Absence is often not reported explicitly (e.g. there exists no record of a species that is not observed in a plot), while it can be important to include these zero values in an analysis (e.g. mean coverage per species in a certain forest reserve; mean stem number per diameter class in a forest reserve).
#' This function automatically adds missing combinations with value zero to the dataset for each combination of values of the variables given in `comb_vars` (within each value of `grouping_vars`).
#' All variables that are not mentioned in `comb_vars` or `grouping_vars`, are considered to be numerical variables and will get value 0 (zero).
#' Note that if a certain value is not present in the dataset (or in one of the subsets defined by `grouping_vars`), it will not be added automatically; at least one record should be added manually for this value (e.g. a plot or diameterclass that doesn't exist in the given dataset, but has to be included in the output).
#'
#'
#' @param dataset data.frame in which records should be added
#' @param comb_vars variables (given as a vector of strings) of which all combinations of their values should have a record in the dataset.
#' @param grouping_vars one or more variables for which the combination of values of the variables given in `comb_vars` should be made for each value, e.g. if grouping_vars = "forest_reserve" and comb_vars = c("plot", "species"), all combinations of the values in "plot" and "species" are made within each value of "forest_reserve".
#'
#' @return dataframe based on `dataset` to which records are added with value 0 (zero) for each measurement.
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' library(tidyverse)
#' dendro_by_plot_species <-
#'   read_forresdat(tablename = "dendro_by_plot_species", repo_path = "C:/gitrepo/forresdat") %>%
#'   select(-year, -plottype)
#' add_zeros(
#'   dataset = dendro_by_plot_species,
#'   comb_vars = c("plot_id", "species"),
#'   grouping_vars = c("forest_reserve", "period")
#' )
#' }
#'
#' @export
#'
#' @importFrom plyr .
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% distinct inner_join mutate mutate_at right_join select vars
#' @importFrom tidyselect all_of matches
#' @importFrom rlang .data
#'
add_zeros <-
  function(
    dataset, comb_vars, grouping_vars
  ) {

  assert_that(
    has_name(dataset, comb_vars),
    msg =  "dataset should contain all variables from comb_vars as column names"
  )
  assert_that(
    has_name(dataset, grouping_vars),
    msg =  "dataset should contain all variables from grouping_vars as column names"
  )
  if (!all(sapply(dataset %>%
                  select(-all_of(c(comb_vars, grouping_vars))), is.numeric))) {
    stop("All dataset columns whose names are not added to comb_vars or grouping_vars, should be numeric")  #nolint
  }

  if (length(comb_vars) >= 1) {
    complete_table <- dataset %>%
      select(all_of(c(grouping_vars, comb_vars[1]))) %>%
      distinct()
  }
  if (length(comb_vars) > 1) {
    for (i in 2:length(comb_vars)) {
      complete_table <- dataset %>%
        select(all_of(c(grouping_vars, comb_vars[i]))) %>%
        distinct() %>%
        inner_join(complete_table, by = grouping_vars)
    }
  }

  #helper function to replace NA by 0 for non dataset records
  replace_na_zero <- function(x, ds_record) {
    ifelse(is.na(x) & is.na(ds_record), 0, x)
  }

  complete_table <- dataset %>%
    mutate(ds_record = TRUE) %>%
    right_join(
      complete_table,
      by = c(grouping_vars, comb_vars)
    ) %>%
    mutate_at(
      vars(!matches(c(grouping_vars, comb_vars, "ds_record"))),
      ~replace_na_zero(., .data$ds_record)
    ) %>%
    select(-.data$ds_record)

  return(complete_table)
}
