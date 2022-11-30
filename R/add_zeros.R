#' Add records with value zero for missing variable combinations in a
#' given dataset
#'
#' @description
#' Datasets for which this package has been developed, typically contain
#' measurements of observations.
#' Absence is often not reported explicitly (e.g. there exists no record of
#' a species that is not observed in a plot),
#' while it can be important to include these zero values in an analysis
#' (e.g. mean coverage per species in a certain forest reserve; mean stem number
#' per diameter class in a forest reserve).
#' This function automatically adds missing combinations with value zero to
#' the dataset for each combination of values of the variables given
#' in `comb_vars` (within each value of `grouping_vars`).
#' All variables that are not mentioned in `comb_vars` or `grouping_vars`,
#' are considered to be numerical variables and will get value 0 (zero).
#' Note that if a certain value is not present in the dataset
#' (or in one of the subsets defined by `grouping_vars`), it will not be
#' added automatically;
#' at least one record should be added manually for this value
#' (e.g. a plot or diameterclass that doesn't exist in the given dataset,
#' but has to be included in the output).
#'
#'
#' @param dataset data.frame in which records should be added
#' @param comb_vars variables (given as a vector of strings) of which all
#' combinations of their values should have a record in the dataset.
#' @param grouping_vars one or more variables for which the combination of
#' values of the variables given in `comb_vars` should be made for each value,
#' e.g. if grouping_vars = "forest_reserve" and comb_vars = c("plot", "species"),
#' all combinations of the values in "plot" and "species" are made
#' within each value of "forest_reserve".
#' @param add_zero_no_na variable indicating which records of the grouping_vars
#' should get a zero value (variable should be TRUE) or a NA value (variable
#' should be FALSE).
#' E.g. a variable indicating whether or not observations are done.
#' If no variable name is given (default NA), all added records get zero values.
#'
#' @return dataframe based on `dataset` to which records are added with
#' value 0 (zero) for each measurement.
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' library(tidyverse)
#' dendro_by_plot_species <-
#'   read_forresdat(tablename = "dendro_by_plot_species", repo_path = "C:/gitrepo/forresdat") %>%
#'   select(-year, -plottype, -starts_with("survey_"), -data_processed, -starts_with("game_"))
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
#' @importFrom dplyr %>% distinct inner_join mutate mutate_at right_join select
#' vars
#' @importFrom tidyselect all_of matches
#' @importFrom rlang .data ensyms
#'
add_zeros <-
  function(
    dataset, comb_vars, grouping_vars, add_zero_no_na = NA
  ) {

  assert_that(
    all(has_name(dataset, comb_vars)),
    msg =  "dataset should contain all variables from comb_vars as column names"
  )
  assert_that(
    all(has_name(dataset, grouping_vars)),
    msg =  "dataset should contain all variables from grouping_vars as column names" # nolint
  )
  if (!is.na(add_zero_no_na)) {
    assert_that(
      length(add_zero_no_na) == 1,
      msg = "variable add_zero_no_na should only contain one string, no vector."
    )
    assert_that(
      has_name(dataset, add_zero_no_na),
      msg =
        "dataset should contain the variable from add_zero_no_na as column name"
    )
    assert_that(
      is.logical(dataset[, add_zero_no_na]),
      msg = "variable add_zero_no_na should have a logical value (in dataset)"
    )
    grouping_with_zeros <-
      dataset %>%
        select(all_of(c(grouping_vars, add_zero_no_na))) %>%
        distinct()
    assert_that(
      nrow(grouping_with_zeros) ==
        nrow(
          dataset %>%
            select(all_of(grouping_vars)) %>%
            distinct()
        ),
      msg = "variable add_zero_no_na must have one unique value for each combination of grouping_vars" #nolint
    )
  }
  if (!all(sapply(dataset %>%
                  select(-all_of(na.omit(c(comb_vars, grouping_vars,
                                           add_zero_no_na)))), is.numeric))) {
    stop("All dataset columns whose names are not added to comb_vars, grouping_vars or add_zero_no_na, should be numeric")  #nolint
  }

  if (length(comb_vars) >= 1) {
    complete_table <- dataset %>%
      select(all_of(c(grouping_vars, comb_vars[1]))) %>%
      distinct()
  }
  if (!is.na(add_zero_no_na)) {
    complete_table <- complete_table %>%
      left_join(grouping_with_zeros, by = grouping_vars)
    dataset <- dataset %>%
      select(-all_of(add_zero_no_na))
  } else {
    add_zero_no_na <- "to_be_removed"
    complete_table <- complete_table %>%
      mutate(to_be_removed = NA)
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
  replace_na_zero <- function(x, ds_record, add_zero_no_na) {
    ifelse(
      is.na(add_zero_no_na),
      ifelse(is.na(x) & is.na(ds_record), 0, x),
      ifelse(is.na(x) & is.na(ds_record) & add_zero_no_na, 0, x)
    )
  }

  complete_table <- dataset %>%
    mutate(ds_record = TRUE) %>%
    right_join(
      complete_table,
      by = c(grouping_vars, comb_vars)
    ) %>%
    mutate_at(
      vars(!matches(na.omit(c(grouping_vars, comb_vars, "ds_record",
                              add_zero_no_na)))),
      ~replace_na_zero(., .data$ds_record, !!!ensyms(add_zero_no_na))
    ) %>%
    select(-.data$ds_record)

  if (add_zero_no_na == "to_be_removed") {
    complete_table <- complete_table %>%
      select(-.data$to_be_removed)
  }

  return(complete_table)
}
