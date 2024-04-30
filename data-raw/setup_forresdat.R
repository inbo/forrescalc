# As functions to save data in forresdat, e.g. save_results_forresdat() and
# from_access_to_forresdat(), read and adapt the existing datapackage in
# forresdat,
# a github repository forresdat with datapackage should be made once before
# additional dataframes can be saved using the forrescalc functions.
# This script documents the steps needed to setup such git repository

# step 1: setup a git repository with branch 'main'
# and add a licence, a readme and a folder data,
# and you might want to add a .gitignore and .Rproj as well

# step 2: run this code to add the first version of the datapackage to a
# branch develop (that will be added by the script)

library(tidyverse)
library(git2r)
library(frictionless)
library(DBI)
library(forrescalc)

path_to_forresdat <- "xxx/forresdat"
path_to_fieldmapdb <-
  system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")

repo <- repository(path_to_forresdat)
checkout(repo, "main")
pull(repo)
con <- forrescalc:::connect_to_database(path_to_fieldmapdb)
q_plottype <- dbReadTable(con, "qPlotType")
dbDisconnect(con)

forresdat <- create_package() %>%
  add_resource(
    resource_name = "q_plot_type",
    data = q_plottype,
    schema = create_schema(q_plottype)
  )
write_package(forresdat, file.path(path_to_forresdat, "data"))
add(repo, path = "*")
commit1 <- commit(
  repo,
  message =
    "add datapackage with dummy table q_plot_type (to be updated with metadata)"
)
branch_create(commit1, name = "develop")

# After this setup is done, the package can be used following examples in
# the documentation (or Main script)
