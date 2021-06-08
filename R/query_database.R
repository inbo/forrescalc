#' @title query all periods from fieldmap database
#'
#' @description
#' This helper function retrieves data of all periods from the given database
#' by using the given query and adding in the period dependent parameters.
#' Reason for this function is to avoid repetition in the load functions.
#'
#' @param query query that is adapted to be used in different periods,
#' in which the following parameters can be used: \itemize{
#'   \item `\%1$d` : replaced by the period number,
#'   \item `\%2$s` : replaced by an empty string in period 1 and by '_?eSet' in all other periods (with ? the period)
#'   \item `\%3$s` : replaced by the string given in argument selection
#'   \item `\%4$s` : replaced by the string given in argument add_fields
#'   \item `\%5$s` : replaced by the string given in argument conjunction
#' }
#' @param selection string that will be added to `\%3$s` in the query
#' @param add_fields string that will be added to `\%4$s` in the query
#' @param conjunction string that will be added to `\%5$s` in the query
#' @param n_periods highest period number that is present in the database
#' (change default here when a new period is added to the database)
#'
#' @inheritParams load_data_dendrometry
#'
#' @return Dataframe containing the result of the query and an additional column
#' period
#'
#' @importFrom RODBC odbcClose odbcConnectAccess2007 sqlQuery
#' @importFrom dplyr %>% bind_rows mutate

#' @importFrom rlang .data
#'

query_database <-
  function(database, query, selection = "", add_fields = "", conjunction = "",
           n_periods = 3) {

  con <- odbcConnectAccess2007(database)
  dataset <-
    sqlQuery(
      con,
      sprintf(query, 1, "", selection, add_fields, conjunction),
      stringsAsFactors = FALSE
    ) %>%
    mutate(
      period = 1
    )

  if (n_periods >= 2) {
    for (n in 2:n_periods) {
      dataset <- dataset %>%
        bind_rows(
          sqlQuery(
            con,
            sprintf(query, n, paste0("_", n, "eSet"), selection, add_fields, conjunction),
            stringsAsFactors = FALSE
          ) %>%
            mutate(
              period = n
            )
        )
    }
  }

  odbcClose(con)

  return(dataset)
}
