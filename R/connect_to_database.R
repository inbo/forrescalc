#' @title connect to fieldmap database
#'
#' @description
#' This helper function returns a connection to the given database (path).
#' Reason for this function is to avoid repetition of this information.
#'
#' @inheritParams load_data_dendrometry
#'
#' @return DBI connection that can be used to connect to the database
#'
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @importFrom RSQLite SQLite
#'

connect_to_database <-
  function(database) {

  if (grepl(".accdb$", database) | grepl(".mdb$", database)) {
    con <-
      dbConnect(
        odbc(),
        .connection_string =
          paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                 database)
      )
  } else if (grepl(".sqlite$", database)) {
    con <- dbConnect(SQLite(), database)
  } else if (grepl(".fdb$", database) | grepl(".gdb", database)) {
    con <-
      dbConnect(
        odbc(),
        .connection_string =
          paste0(
            "Driver={Firebird/InterBase(r) driver};UID=SYSDBA;PWD=masterkey; DBNAME=", #nolint: line_length_linter
            database
          )
      )
  } else {
    stop(
      "This database type is not supported, please use .mdb, .accdb, .fdb, .gdb or .sqlite" #nolint: line_length_linter
    )
  }

  return(con)
}
