#' save results of calculations in git repository `forresdat`
#'
#' This function saves the results from calculations by the forrescalc package
#' (or any other named list with dataframes) in git repository `forresdat`.
#' List item names will be used to name each of the tables, which contain
#' as a content the different dataframes.
#'
#' @param results results from calculations in package forrescalc as a
#' named list of dataframes
#' @param repo_path name and path of local `forresdat` repository in which
#' results/tables should be saved
#' @param metadata_path path including .xlsx file in which the metadata are
#' stored
#' @param push push commits directly to the remote on GitHub?
#' Default is FALSE (no). (This option can only be used with SSH.)
#' @param strict keep default TRUE to update data without structural changes,
#' change to FALSE only if tables are structurally changed
#' (e.g. additional column, change in sorting order,...)
#' @param branch branch from repository `forresdat` to which the new version
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
#' save_results_forresdat(
#'   results = result_regeneration,
#'   repo_path = path_to_forresdat,
#'   metadata_path = temp
#' )
#' }
#'
#' @export
#'
#' @importFrom assertthat has_name
#' @importFrom dplyr %>% across arrange bind_rows left_join
#' @importFrom git2r add checkout commit branches pull push repository
#' @importFrom readxl excel_sheets read_xlsx
#' @importFrom frictionless add_resource create_schema get_schema read_package
#'   read_resource remove_resource resources write_package
#' @importFrom purrr imap
#' @importFrom tidyselect all_of
#'
save_results_forresdat <-
  function(
    results, repo_path, metadata_path, push = FALSE, strict = TRUE,
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
  sorting_max <-
    c("period", "year", "plot_id", "dbh_class_5cm", "decaystage", "subplot_id",
      "tree_measure_id", "height_class", "species")
  metadata_tables <- read_xlsx(metadata_path, sheet = "Content")
  package <- read_package(file.path(repo_path, "data", "datapackage.json"))
  for (tablename in names(results)) {
    table_results <- results[[tablename]]
    sorting <- sorting_max[sorting_max %in% colnames(table_results)]
    table_results <- table_results %>%
      arrange(across(all_of(sorting)))
    if (tablename %in% resources(package)) {
      colnames_forresdat <- colnames(read_resource(package, tablename))
      table_results <-
        compare_colnames_forresdat(
          table_results, tablename, colnames_forresdat, strict
        )
      schema_forresdat <- get_schema(package, tablename)
      package <- package %>%
        remove_resource(tablename)
      file.remove(file.path(repo_path, "data", paste0(tablename, ".csv")))
    }
    schema_results <- create_schema(table_results)
    if (!tablename %in% metadata_tables$Table) {
      warning(
        sprintf(
          "Table %s has no metadata in tab 'Content' in the metadata file",
          tablename
        )
      )
    }
    if (!tablename %in% excel_sheets(metadata_path)) {
      warning(
        sprintf(
          "There is no tab %s with metadata in the metadata file",
          tablename
        )
      )
    } else {
      metadata_columns <- read_xlsx(metadata_path, sheet = tablename)
      metadata_columns_ordered <- # nolint: object_usage_linter
        bind_rows(
          imap(
            schema_results$fields, ~data.frame(index = .y, name = .x[["name"]])
          )
        ) %>%
        left_join(metadata_columns, by = c("name" = "Field Name"))
      schema_results$fields <-
        imap(
          schema_results$fields,
          ~c(.x, description = metadata_columns_ordered$Description[.y],
             extra_info = metadata_columns_ordered$`Extra info`[.y])
        )
    }
    if (strict && exists("schema_forresdat")) {
      tryCatch(
        all.equal(schema_results, schema_forresdat),
        error = function(e) {
          stop(
            paste(
              "Differences in metadata with the version on forresdat:",
              e
            )
          )
        },
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
          ]$Description,
        extra_info =
          metadata_tables[
            !is.na(metadata_tables$Table) & metadata_tables$Table == tablename,
          ]$`Extra info`,
        source_database = attr(table_results, "database"),
        calculation = attr(table_results, "forrescalc"),
        height_model = attr(table_results, "heightmodels")
      )
    if (has_name(metadata_tables, "Attention")) {
      package$resources[[which(resources(package) == tablename)]] <-
        append(
          package$resources[[which(resources(package) == tablename)]],
          c(attention = metadata_tables[
              !is.na(metadata_tables$Table) &
                metadata_tables$Table == tablename,
            ]$Attention),
          after = 9
        )
    }
  }
  package$resources <-
    package$resources[order(sapply(package$resources, "[[", 1))]
  write_package(package, file.path(repo_path, "data"))
  add(repo, path = "*")
  tryCatch(
    commit(
      repo, message = "scripted commit: results added/updated", session = TRUE
    ),
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
