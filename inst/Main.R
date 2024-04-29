library(tidyverse)
library(forrescalc)

path_to_fieldmap <-
  system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
path_to_git_forresdat <- "C:/R/bosreservatendb/forresdat"
path_to_height_models <-
  system.file("example/height_models", package = "forrescalc")

# only when q-tables in Fieldmap have changed
# (and only mention the changed table)
temp <- tempfile(fileext = ".xlsx")
dl <- googledrive::drive_download(
  googledrive::as_id("17M_TfOyjpqLzsFqQ_w1DXitzI7tnULR6"),
  path = temp, overwrite = TRUE
)
from_access_to_forresdat(
  database = path_to_fieldmap,
  tables = c("qAliveDead", "qSpecies", "qHeightClass_regeneration",
             "qnumber_regeneration_classes", "qdecaystage"),
  repo_path = path_to_git_forresdat,
  metadata_path = temp
)

# download .xlsx with metadata of table plotinfo to (path) 'temp'
temp <- tempfile(fileext = ".xlsx")
dl <- googledrive::drive_download(
  googledrive::as_id("1tUNtlwcSnlVXnri235gqnhIHaBNf1Z2W"), path = temp,
  overwrite = TRUE
)

plotinfo <- load_plotinfo(database = path_to_fieldmap)
save_results_forresdat(
  results = list(plotinfo = plotinfo),
  repo_path = path_to_git_forresdat,
  metadata_path = temp
)

# DENDROMETRY

data_dendro <-
  load_data_dendrometry(
    database = path_to_fieldmap
  )
data_deadwood <-
  load_data_deadwood(
    database = path_to_fieldmap
  )
data_shoots <-
  load_data_shoots(
    database = path_to_fieldmap
  )
height_model <- load_height_models(path_to_height_models)

dendro <- calculate_dendrometry(data_dendro, data_deadwood, data_shoots,
                                height_model, plotinfo)

# download .xlsx with metadata of dendrometry tables to (path) 'temp'
temp <- tempfile(fileext = ".xlsx")
dl <- googledrive::drive_download(
  googledrive::as_id("1M8vTjaPst9LXaDrN1vPVUrsgjaZtAbRi"), path = temp,
  overwrite = TRUE
)

save_results_forresdat(
  results = dendro,
  repo_path = path_to_git_forresdat,
  metadata_path = temp
)

# REGENERATION

data_regeneration <-
  load_data_regeneration(
    database = path_to_fieldmap
  )

regeneration <- calculate_regeneration(data_regeneration)

# download .xlsx with metadata of regeneration tables to (path) 'temp'
temp <- tempfile(fileext = ".xlsx")
dl <- googledrive::drive_download(
  googledrive::as_id("17M_TfOyjpqLzsFqQ_w1DXitzI7tnULR6"), path = temp,
  overwrite = TRUE
)

save_results_forresdat(
  results = regeneration,
  repo_path = path_to_git_forresdat,
  metadata_path = temp
)

# VEGETATION

data_vegetation <-
  load_data_vegetation(
    database = path_to_fieldmap
  )
data_herblayer <-
  load_data_herblayer(
    database = path_to_fieldmap
  )

vegetation <- calculate_vegetation(data_vegetation, data_herblayer)

# download .xlsx with metadata of vegetation tables to (path) 'temp'
temp <- tempfile(fileext = ".xlsx")
dl <- googledrive::drive_download(
  googledrive::as_id("1Ywwgb0WKGWv9OqJd_rXjZRYDJVpW_KOJ"), path = temp,
  overwrite = TRUE
)

save_results_forresdat(
  results = vegetation,
  repo_path = path_to_git_forresdat,
  metadata_path = temp
)
