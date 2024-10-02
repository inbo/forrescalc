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
#' @param git_ref_type Which type of reference is given in `git_reference`, a
#' release, a branch or a commit (hash)?
#' Defaults to "release".
#' @param git_reference The forresdat version (release), branch or commit
#' (give hash) that should be given.
#' Make sure git_ref_type mentions the reference type that has been given.
#' Defaults to "latest" (release).
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
download_forresdat <- function(git_ref_type = c("release", "branch", "commit"), git_reference = "latest") {

  match.arg(git_ref_type)

  # OS independent function to look up system temp drive or make one
  datadir <- R_user_dir("forrescalc", "data")
  if (!dir.exists(datadir)) {
    dir.create(datadir, recursive = TRUE)
  }

  # look up version/commit and derive output path (even if not exists yet)
  # (to avoid repeating this code for each return)
  link <-
    sprintf(
      "https://api.github.com/repos/inbo/forresdat/%ss/%s",
      ifelse(git_ref_type == "branch", "branche", git_ref_type),
      git_reference
    )
  info <- GET(link)
  if (!is.null((content(info))$status) && (content(info))$status == "404") {
    stop(
      sprintf(
        "%s %s not found in repository forresdat, please check if it exists",
        git_ref_type, git_reference
      )
    )
  }
  if (git_ref_type == "release") {
    version_download <- (content(info))$tag_name
    zip_download <- (content(info))$zipball_url
    version_map <- "release"
  } else {
    version_download <- (content(info))$commit$sha
    zip_download <-
      sprintf(
        "https://github.com/inbo/forresdat/archive/%s.zip",
        version_download
      )
    version_map <- "commit"
  }

  path_to_forresdat <- file.path(datadir, "forresdat", version_map, "datapackage")
  if (!is.null(version_download)) {
    attr(path_to_forresdat, "version") <- paste(version_map, version_download)
  }

  # check what is already present in the local system temp and react accordingly
  if (!file.exists(file.path(datadir, "forresdat", version_map))) {
    if (!file.exists(file.path(datadir, "forresdat"))) {
      dir.create(file.path(datadir, "forresdat"))
    }
    dir.create(file.path(datadir, "forresdat", version_map))
    file.create(file.path(datadir, "forresdat", version_map, "version.txt"))
  } else {
    version_local <-
      readLines(file.path(datadir, "forresdat", version_map, "version.txt"))
    if (is.null(version_download) || version_download == version_local) {
      return(path_to_forresdat)
    } else {
      unlink(path_to_forresdat, recursive = TRUE)
    }
  }

  # write or overwrite the version number in the txt
  writeLines(version_download, file.path(datadir, "forresdat", version_map, "version.txt"))

  # download the datapackage and move to the right folder
  # (move is to avoid an automatically generated folder name that differs)
  zippath <- file.path(datadir, "forresdat", version_map, "datapackage.zip")
  download.file(zip_download, zippath, mode = "wb")
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
