#' save results of calculations in git repository
#'
#' This function saves the results from calculations by the forrescalc package
#' (or any other named list with dataframes) in git repository forresdat.
#' List item names will be used to name each of the tables, which contain
#' as a content the different dataframes.
#'
#' @param results results from calculations in package forrescalc as a
#' named list
#' @param repo_path name and path of local git repository in which results
#' should be saved
#' @param metadata_path path including .xlsx file in which the metadata are
#' stored
#' @param push push commits directly to the remote on github?
#' Default is FALSE (no). (This option can only be used with SSH.)
#' @param strict keep default TRUE to update data without structural changes,
#' change to FALSE only if tables are structurally changed
#' (e.g. additional column, change in sorting order,...)
#' @param branch branch from repository forresdat to which the new version
#' should be committed.
#' Default is 'develop'.
#'
#' @return No value is returned, data are saved in the specified git repository
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
#' data_regeneration <- load_data_regeneration(path_to_fieldmapdb)
#' result_regeneration <- calculate_regeneration(data_regeneration)
#' save_results_git(
#'   results = result_regeneration,
#'   repo_path = path_to_forresdat,
#'   metadata_path = temp
#' )
#' }
#'
#' @export
#'
#' @importFrom dplyr across arrange bind_rows left_join
#' @importFrom git2r add checkout commit pull push repository
#' @importFrom readxl read_xlsx
#' @importFrom frictionless add_resource create_schema get_schema read_package
#'   read_resource resources write_package
#' @importFrom purrr imap
#' @importFrom tidyselect all_of
#'
save_results_git <-
  function(
    results, repo_path, metadata_path, push = FALSE, strict = TRUE,
    branch = "develop"
  ) {
  repo <- repository(repo_path)
  checkout(repo, branch)
  pull(repo, credentials = get_cred(repo))
  sorting_max <-
    c("period", "year", "plot_id", "dbh_class_5cm", "decaystage", "subplot_id",
      "tree_measure_id", "height_class", "species")
  metadata_tables <- read_xlsx(metadata_path, sheet = "Content")
  package <- read_package(file.path(repo_path, "datapackage.json"))
  for (tablename in names(results)) {
    table_results <- results[[tablename]]
    sorting <- sorting_max[sorting_max %in% colnames(table_results)]
    table_results <- table_results %>%
      arrange(across(all_of(sorting)))
    if (tablename %in% resources(package)) {
      colnames_forresdat <- colnames(read_resource(package, tablename))
      colnames_results <- colnames(table_results)
      colnames_old <-
        colnames_forresdat[!colnames_forresdat %in% colnames_results]
      colnames_new <-
        colnames_results[!colnames_results %in% colnames_forresdat]
      if (length(colnames_old) > 0 || length(colnames_new) > 0) {
        if (strict) {
          text <- paste0(
            sprintf(
              "extra in new table: %s",
              paste(colnames_new, sep = ", ")[length(colnames_new) > 0]
            ),
            "; "[length(colnames_new) > 0 && length(colnames_old) > 0],
            sprintf(
              "extra in forresdat: %s",
              paste(colnames_old, sep = ", ")[length(colnames_old) > 0]
            )
          )
          stop(
            sprintf(
              "Table %s has different column names than the version on forresdat. (%s) Use strict = FALSE if you want to save the new version anyway.", #nolint: line_length_linter
              tablename, text
            )
          )
        } else {
          colnames_forresdat <-
            colnames_forresdat[colnames_forresdat %in% colnames_results]
          colnames_forresdat <- c(colnames_forresdat, colnames_new)
        }
      }
      table_results <- table_results %>%
        select(colnames_forresdat)
      schema_forresdat <- get_schema(package, tablename)
      package <- package %>%
        remove_resource(tablename)
    }
    schema_results <- create_schema(table_results)
    if (!tablename %in% metadata_tables$Table) {
      warning(
        sprintf(
          "Table %s has no metadata in tab 'Content' in the metadata file",
          tablename
        )
      )
    } else {
      metadata_columns <- read_xlsx(metadata_path, sheet = tablename)
      metadata_columns_ordered <-
        bind_rows(
          imap(
            schema_results$fields, ~data.frame(index = .y, name = .x[["name"]])
          )
        ) %>%
        left_join(metadata_columns, by = c("name" = "Field Name"))
      schema_results$fields <-
        imap(
          schema_results$fields,
          ~c(.x, description = metadata_columns_ordered$Description[.y])
        )
    }
    if (strict && exists("schema_forresdat")) {
      tryCatch(
        all.equal(schema_results, schema_forresdat),
        error = function(e)
          stop(
            paste(
              "Differences in metadata with the version on forresdat:",
              e
            )
          ),
        finally = sprintf("(Error refers to table %s", tablename)
      )
    }
    package <- package %>%
      add_resource(
        resource_name = tablename,
        data = table_results,
        schema = schema_results,
        description =
          metadata_tables[
            !is.na(metadata_tables$Table) & metadata_tables$Table == tablename,
          ]$Description
      )
  }
  write_package(package, repo_path)
  add(repo, path = "*")
  tryCatch(
    commit(repo, message = "scripted commit from forrescalc", session = TRUE),
    error = function(e) {
      val <- withCallingHandlers(e)
      if (
        startsWith(
          val[["message"]], "Error in 'git2r_commit': Nothing added to commit"
        )
      ) {
        stop(
          "New tables are identical to tables in git-repository, so no commit added",  #nolint: line_length_linter
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
