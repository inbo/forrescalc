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
#'   \item `\%2$s` : replaced by an empty string in period 1 and by '_?eSet'
#'   in all other periods (with ? the period)
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
#' @importFrom DBI dbDisconnect dbGetQuery
#' @importFrom dplyr %>% across bind_rows mutate
#' @importFrom rlang .data
#' @importFrom tidyselect starts_with
#'

query_database <-
  function(database, query, selection = "", add_fields = "", conjunction = "",
           n_periods = 3) {

  #code to avoid warning in sprintf due to absence of %x in string
  present <- regmatches(query, gregexec("\\%(\\d)\\$[d|s]", query))[[1]][2,]
  absent <- as.character(1:5)[!as.character(1:5) %in% present]
  to_insert <- paste(paste0("%", absent, "$s"), collapse = " ")
  query <- paste0(query, to_insert)
  no_neset <- !"2" %in% present
  no_n <- !"1" %in% present

  con <- connect_to_database(database)

  n_corr <- ifelse(no_n, "", 1)
  dataset <-
    dbGetQuery(
      con,
      sprintf(query, n_corr, "", selection, add_fields, conjunction)
    ) %>%
    mutate(
      period = 1
    )

  if (n_periods >= 2) {
    for (n in 2:n_periods) {
      n_eset <- ifelse(no_neset, "", paste0("_", n, "eSet"))
      n_corr <- ifelse(no_n, "", n)
      data_period_n <-
        dbGetQuery(
          con,
          sprintf(
            query, n_corr, n_eset, selection, add_fields, conjunction
          )
        ) %>%
          mutate(
            period = n
          )
      if (nrow(data_period_n) > 0) {
        if (nrow(dataset) > 0) {
          dataset <- dataset %>%
            bind_rows(data_period_n)
        } else {
          dataset <- data_period_n
        }
      }
    }
  }

  if (inherits(con, "SQLiteConnection")) {
    dataset <- dataset %>%
      mutate(
        across(starts_with("date_"), ~ as.POSIXct(.x, origin = "1970-01-01"))
      )
  }
  # starts_with("date_") could in future be replaced with where(as.Date) of
  # packages tidyselect and lubridate

  dbDisconnect(con)

  return(dataset)
}
