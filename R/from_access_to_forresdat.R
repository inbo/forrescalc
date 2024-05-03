#' copy table(s) from access db to git repository forresdat
#'
#' This function loads one or more tables from the access database
#' (or an SQLite database) and saves them in the git repository forresdat.
#'
#' @param tables vector with table names of tables that should be moved
#' @inheritParams load_data_dendrometry
#' @inheritParams save_results_forresdat
#'
#' @return No value is returned, the tables are saved in the git repository.
#'
#' @export
#'
#' @importFrom assertthat has_name
#' @importFrom dplyr arrange bind_rows left_join
#' @importFrom git2r add branches checkout commit pull push repository
#' @importFrom DBI dbDisconnect dbReadTable
#' @importFrom frictionless add_resource create_schema get_schema read_package
#'   read_resource remove_resource resources write_package
#' @importFrom purrr imap
#' @importFrom readxl excel_sheets read_xlsx
#'
#' @examples
#' \dontrun{
#' #make a local clone of forresdat and change path before running
#' library(forrescalc)
#' # add path to your local clone of forresdat
#' path_to_forresdat <- "xxx/forresdat"
#' # if you don't have a local clone yet, make it:
#' git2r::clone("https://github.com/inbo/forresdat.git", path_to_forresdat)
#' # (add path to your own fieldmap database here)
#' path_to_fieldmapdb <-
#'   system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
#' # add path to metadata here
#' temp <- tempfile(fileext = ".xlsx")
#' dl <- googledrive::drive_download(
#'          googledrive::as_id("17M_TfOyjpqLzsFqQ_w1DXitzI7tnULR6"),
#'          path = temp, overwrite = TRUE
#'        )
#'
#' from_access_to_forresdat(
#'   database = path_to_fieldmapdb,
#'   tables = c("qCoverHerbs", "qtotalCover"),
#'   repo_path = path_to_forresdat,
#'   metadata_path = temp
#' )
#' }
#'
from_access_to_forresdat <-
  function(
    database, tables, repo_path, metadata_path, push = FALSE, strict = TRUE,
    branch = "develop"
  ) {
  repo <- repository(repo_path)
  if (!has_name(branches(repo), branch)) {
    stop(
      sprintf(
        "Branch %s doesn't exist in forresdat. Add this branch and try again",
        branch
      )
    )
  }
  checkout(repo, branch)
  pull(repo, credentials = get_cred(repo))
  metadata_tables <- read_xlsx(metadata_path, sheet = "Content")
  package <- read_package(file.path(repo_path, "data", "datapackage.json"))
  con <- connect_to_database(database)
  for (tablename_fm in tables) {
    table <- dbReadTable(con, tablename_fm)
    if (has_name(table, "ID")) {
      table <- table %>%
        arrange(.data$ID)
    }
    tablename_fd <-
      gsub("^(q?)_", "\\1", tolower(gsub("([A-Z])", "_\\1", tablename_fm)))
    if (tablename_fd %in% resources(package)) {
      colnames_forresdat <- colnames(read_resource(package, tablename_fd))
      table <-
        compare_colnames_forresdat(
          table, tablename_fd, colnames_forresdat, strict
        )
      schema_forresdat <- get_schema(package, tablename_fd)
      package <- package %>%
        remove_resource(tablename_fd)
      file.remove(file.path(repo_path, "data", paste0(tablename_fd, ".csv")))
    }
    schema_table <- create_schema(table)
    if (!tablename_fd %in% metadata_tables$Table) {
      warning(
        sprintf(
          "Table %s has no metadata in tab 'Content' in the metadata file",
          tablename_fd
        )
      )
    }
    if (!tablename_fd %in% excel_sheets(metadata_path)) {
      warning(
        sprintf(
          "There is no tab %s with metadata in the metadata file",
          tablename_fd
        )
      )
    } else {
      metadata_columns <- read_xlsx(metadata_path, sheet = tablename_fd)
      metadata_columns_ordered <-
        bind_rows(
          imap(
            schema_table$fields, ~data.frame(index = .y, name = .x[["name"]])
          )
        ) %>%
        left_join(metadata_columns, by = c("name" = "Field Name"))
      schema_table$fields <-
        imap(
          schema_table$fields,
          ~c(.x, description = metadata_columns_ordered$Description[.y],
             extra_info = metadata_columns_ordered$`Extra info`[.y])
        )
    }
    if (strict && exists("schema_forresdat")) {
      tryCatch(
        all.equal(schema_table, schema_forresdat),
        error = function(e) {
          stop(
            paste(
              "Differences in metadata with the version on forresdat:",
              e
            )
          )
        },
        finally = sprintf("(Error refers to table %s", tablename_fd)
      )
    }
    package <- package %>%
      add_resource(
        resource_name = tablename_fd,
        data = table,
        schema = schema_table,
        description =
          metadata_tables[
            !is.na(metadata_tables$Table) &
              metadata_tables$Table == tablename_fd,
          ]$Description,
        extra_info =
          metadata_tables[
            !is.na(metadata_tables$Table) &
              metadata_tables$Table == tablename_fd,
          ]$`Extra info`,
        source_database = sub("^.*\\/(.*)\\/.*\\.\\w*$", "\\1", database)
      )
  }
  dbDisconnect(con)
  package$resources <-
    package$resources[order(sapply(package$resources, "[[", 1))]
  write_package(package, file.path(repo_path, "data"))
  add(repo, path = "*")
  tryCatch(
    commit(
      repo, message = "scripted commit: copy from fieldmap", session = TRUE
    ),
    error = function(e) {
      val <- withCallingHandlers(e)
      if (
        startsWith(
          val[["message"]], "Error in 'git2r_commit': Nothing added to commit"
        )
      ) {
        stop(
          "Tables in database and git-repository are identical, so no commit added", # nolint
          call. = FALSE
        )
      }
      stop(e)
    }
  )
  if (push) {
    push(repo, credentials = get_cred(repo))
  }
}
