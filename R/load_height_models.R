#' retrieve height model data from xlsx files
#'
#' This function groups the information on height models from the `.xlsx` files
#' in the given folder together in one dataframe.
#'
#' @param path_to_height_models path to folder where height models are stored
#'
#' @return Dataframe with height model data
#'
#' @importFrom dplyr %>% mutate select transmute
#' @importFrom rlang .data
#' @importFrom stringr str_detect str_extract str_split
#' @importFrom tidyr nest unnest
#' @importFrom purrr map
#' @importFrom readxl read_xlsx
#'
#' @examples
#' \dontrun{
#' #change path before running
#' library(forrescalc)
#' load_height_models("C:/bosreservaten/Hoogtemodellen/")
#' }
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
  heigthtmodels <-
    data.frame(
      filename = list.files(path = path_to_height_models, pattern = "xlsx")
    ) %>%
    mutate(
      no_extension = str_extract(.data$filename, "^(.+)(?=\\.)"),
      x = str_split(.data$no_extension, "_"),
      forest_reserve = unlist(.data$x)[2],
      plottype = unlist(.data$x)[3],
      period = unlist(.data$x)[4],
      path_file = paste0(path_to_height_models, .data$filename)
    ) %>%
    select(-.data$no_extension, -.data$x) %>%
    nest(data = c(.data$path_file)) %>%
    mutate(
      data = map(.data$data, add_models)
    ) %>%
    unnest(cols = c(.data$data))

  return(heigthtmodels)
}


add_models <- function(data) {
  read_xlsx(data$path_file) %>%
    transmute(
      plot_id = .data$PlotID,
      species = as.numeric(.data$Species),
      model = .data$Model,
      .data$P1, .data$P2
    )
}
