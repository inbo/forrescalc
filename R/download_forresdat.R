#' Download data package `forresdat` to local temp
#'
#' This internal helper function checks first if the local temp folder already
#' contains a downloaded version of `forresdat`,
#' and if this version is still the same as the latest release version on
#' GitHub.
#' If this is not the case, it downloads the latest version of data package
#' `forresdat`.
#' This function is used in `read_forresdat()` and `read_forresdat_table()`
#' to avoid unnecessarily downloading and to keep the path (and preparation
#' steps) in one function.
#'
#' @return The path to the local version of the data package `forresdat` as a
#' string, with attribute 'version' giving the version number of the release.
#'
#' @noRd
#'
#' @importFrom httr content GET
#' @importFrom stringr str_subset
#' @importFrom tools R_user_dir
#' @importFrom utils download.file unzip
#'
download_forresdat <- function() {
  # OS independent function to look up system temp drive or make one
  datadir <- R_user_dir("forrescalc", "data")
  if (!dir.exists(datadir)) {
    dir.create(datadir, recursive = TRUE)
  }

  # look up latest release and derive output path (even if not exists yet)
  # (to avoid repeating this code for each return)
  latest_release <-
    GET("https://api.github.com/repos/inbo/forresdat/releases/latest")
  version_latest <- (content(latest_release))$tag_name
  path_to_forresdat <- file.path(datadir, "forresdat", "datapackage")
  attr(path_to_forresdat, "version") <- version_latest

  # check what is already present in the local system temp and react accordingly
  if (!file.exists(file.path(datadir, "forresdat"))) {
    dir.create(file.path(datadir, "forresdat"))
    file.create(file.path(datadir, "forresdat", "version.txt"))
  } else {
    version_local <-
      readLines(file.path(datadir, "forresdat", "version.txt"))
    if (version_latest == version_local) {
      return(path_to_forresdat)
    } else {
      unlink(path_to_forresdat, recursive = TRUE)
    }
  }

  # write or overwrite the version number in the txt
  writeLines(version_latest, file.path(datadir, "forresdat", "version.txt"))

  # download the datapackage and move to the right folder
  # (move is to avoid an automatically generated folder name that differs)
  zippath <- file.path(datadir, "forresdat", "datapackage.zip")
  download.file((content(latest_release))$zipball_url, zippath, mode = "wb")
  zip_contents <- unzip(zippath, list = TRUE)
  path_to_data <- str_subset(zip_contents$Name, "/data/")
  unzip(zippath, files = path_to_data, exdir = path_to_forresdat)

  files_to_move <-
    list.files(file.path(path_to_forresdat, path_to_data), full.names = TRUE)
  file.copy(files_to_move, path_to_forresdat)
  folder_to_move <-
    gsub("^(.*)/data/$", "\\1", str_subset(zip_contents$Name, "/data/$"))
  extracted_subfolder <- file.path(path_to_forresdat, folder_to_move)
  unlink(extracted_subfolder, recursive = TRUE)

  return(path_to_forresdat)
}
