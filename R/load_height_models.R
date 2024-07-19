#' retrieve height model data from git repository `forresheights`
#'
#' This function groups the information on height models from the `.csv` files
#' in the git repository
#' [`forresheights`](https://github.com/inbo/forresheights)
#' together in one dataframe.
#'
#' @param example_dataset Should a (limited) example dataset be loaded?
#'   Defaults to FALSE, loading the whole dataset from the git repository.
#'   If TRUE, only height models needed for the example database will be loaded
#'   (to be used in the examples).
#'
#' @return Dataframe with height model data
#'
#' @importFrom dplyr %>% distinct mutate relocate select transmute
#' @importFrom httr content GET stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom rlang .data
#' @importFrom stringr str_extract str_split
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom readr locale read_csv2
#' @importFrom utils packageVersion
#'
#' @examples
#' \dontrun{
#' # example ignored during checks due to high elapsed time
#' library(forrescalc)
#' load_height_models()
#' }
#'
#' @export
#'
load_height_models <- function(example_dataset = FALSE) {
  if (example_dataset == TRUE) {
    height_models <-
      suppressMessages(
        read_csv2(
          system.file(
            "example/database/height_models.csv", package = "forrescalc"
          )
        )
      )
    return(height_models)
  }
  req <-
    GET(
      "https://api.github.com/repos/inbo/forresheights/git/trees/main?recursive=1" #nolint: line_length_linter
    )
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = FALSE)
  tablelist <-
    sub("data/(.*)\\.csv", "\\1", filelist[grepl("data/.*\\.csv", filelist)])
  path_to_height_models <-
    "https://raw.githubusercontent.com/inbo/forresheights/main/data/%s.csv"
  heightmodels <-
    data.frame(
      filename = tablelist
    ) %>%
    mutate(
      x = str_split(.data$filename, "_"),
      plottype = sapply(.data$x, `[`, 3),
      period = as.integer(sapply(.data$x, `[`, 4)),
      path_file = sprintf(path_to_height_models, .data$filename)
    ) %>%
    select(-"x") %>%
    mutate(
      data = map(.data$path_file, add_models)
    ) %>%
    unnest(cols = c("data")) %>%
    select(-"filename", -"path_file") %>%
    distinct() %>%
    relocate("forest_reserve", .before = "plottype")
  if (nrow(heightmodels) == 0) {
    warning("No height models (.xlsx files) found on the given path.")
  }

  attr(heightmodels, "forrescalc") <-
    paste("forrescalc", packageVersion("forrescalc"))
  commit <- fromJSON("https://api.github.com/repos/inbo/forresheights/commits?")
  attr(heightmodels, "heightmodels") <-
    paste("forresheights commit", commit$sha[1])

  return(heightmodels)
}


add_models <- function(path_file) {
  read_csv2(
    path_file, show_col_types = FALSE,
    locale = locale(decimal_mark = ",", grouping_mark = ".")
  ) %>%
    transmute(
      forest_reserve = .data$BR,
      species =
        ifelse(
          is.na(.data$Species) | .data$Species == "<ALL>", -Inf, .data$Species
        ),
      species = as.numeric(.data$species),
      species = ifelse(.data$species == -Inf, NA_real_, .data$species),
      model = .data$Model,
      .data$P1, .data$P2
    )
}
