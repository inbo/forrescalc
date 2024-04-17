#' retrieve height model data from xlsx files
#'
#' This function groups the information on height models from the `.xlsx` files
#' in the given folder together in one dataframe.
#'
#' @param path_to_height_models path to folder where height models are stored
#'
#' @return Dataframe with height model data
#'
#' @importFrom dplyr %>% distinct mutate select transmute
#' @importFrom rlang .data
#' @importFrom stringr str_detect str_extract str_split
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom readxl read_xlsx
#' @importFrom utils packageVersion
#'
#' @examples
#' #change path before running
#' library(forrescalc)
#' # (add path to your height models here)
#' path_to_height_models <-
#'   system.file("example/height_models", package = "forrescalc")
#' load_height_models(path_to_height_models)
#'
#' @export
#'
load_height_models <- function(path_to_height_models) {
  path_to_height_models <-
    ifelse(
      str_detect(path_to_height_models, "^(.+)\\/$"),
      path_to_height_models,
      paste0(path_to_height_models, "/")
    )
  heightmodels <-
    data.frame(
      filename = list.files(path = path_to_height_models, pattern = "xlsx")
    ) %>%
    mutate(
      no_extension = str_extract(.data$filename, "^(.+)(?=\\.)"),
      x = str_split(.data$no_extension, "_"),
      plottype = sapply(.data$x, `[`, 3),
      period = as.integer(sapply(.data$x, `[`, 4)),
      path_file = paste0(path_to_height_models, .data$filename)
    ) %>%
    select(-"no_extension", -"x") %>%
    mutate(
      data = map(.data$path_file, add_models)
    ) %>%
    unnest(cols = c(data)) %>%
    select(-"filename", -"path_file") %>%
    distinct()
  if (nrow(heightmodels) == 0) {
    warning("No height models (.xlsx files) found on the given path.")
  }

  attr(heightmodels, "forrescalc") <-
    paste("forrescalc", packageVersion("forrescalc"))

  return(heightmodels)
}


add_models <- function(path_file) {
  read_xlsx(path_file) %>%
    transmute(
      species =
        ifelse(
          is.na(.data$Species) | .data$Species == "<ALL>", -Inf, .data$Species
        ),
      species = as.numeric(.data$species),
      species = ifelse(.data$species == -Inf, NA_real_, .data$species),
      model = .data$Model,
      .data$P1, .data$P2,
      forest_reserve = .data$BR
    )
}
