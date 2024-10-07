#' @title load data package from git repository `forresdat`
#'
#' @description
#' This function reads the data package from git repository `forresdat`
#' (and saves the `forresdat` data to a local temp directory to avoid unneeded
#' downloading in the future).
#' This data package contains both data and metadata and can be explored using
#' functions of the [frictionless](https://docs.ropensci.org/frictionless/)
#' package.
#'
#' Data available in `forresdat` only contain observations, so no records with
#' zero values are added for for instance species that were not observed and
#' hence absent.
#' These zero value records can easily be added by using the function
#' `add_zeros()`.
#'
#' The different tables of this dataset contain data that are collected
#' using 2 different methods (plot types):
#' circular plots (CP) and core areas (CA).
#' It is advised to only use one of them for analyses, as the data are likely
#' to differ due to method related differences.
#'
#' General information on the plot level is available in table `plotinfo`,
#' which can easily be joined to other tables on `plot_id` and `period`
#' (or only `plot_id` if `period` is absent).
#'
#' @param git_ref_type Which type of reference is given in `git_reference`, a
#' release, a branch or a commit (hash)?
#' Defaults to "release".
#' @param git_reference The forresdat version (release), branch or commit
#' (give hash) that should be given.
#' Make sure git_ref_type mentions the reference type that has been given.
#' Defaults to "latest" (release).
#'
#' @return A `frictionless` data package with all tables and metadata from
#' GitHub repository `forresdat`, which can be explored using package
#' [`frictionless`](https://docs.ropensci.org/frictionless/).
#' To be able to recall the version of the data, this data package contains
#' an attribute with the version number of the release of `forresdat` from which
#' the data are taken.
#'
#' @examples
#' library(forrescalc)
#' datapackage <- read_forresdat()
#' frictionless::resources(datapackage)
#' attr(datapackage, "forresdat")
#'
#' @export
#'
#' @importFrom frictionless read_package
#'
read_forresdat <-
  function(
    git_ref_type = c("release", "branch", "commit"), git_reference = "latest") {

  git_ref_type <- match.arg(git_ref_type)
  path_to_forresdat <-
    download_forresdat(
      git_ref_type = git_ref_type, git_reference = git_reference
    )
  dataset <- read_package(file.path(path_to_forresdat, "datapackage.json"))

  warning("Tables contain data of 2 different methods (plottypes, CP and CA): select only one of them to do reliable analyses") #nolint: line_lenght_linter
  warning("The dataset only contains presence data and lacks zero observations (except for 1 observation per plot_id and period to indicate that observations are done).  Please use function add_zeros() to add zero observations when needed.") #nolint: line_length_linter

  attr(dataset, "forresdat") <-
    paste("forresdat", attr(path_to_forresdat, "version"))

  return(dataset)
}
