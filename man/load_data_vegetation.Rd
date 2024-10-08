% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_data_vegetation.R
\name{load_data_vegetation}
\alias{load_data_vegetation}
\title{retrieve vegetation data from \code{Fieldmap} database}
\usage{
load_data_vegetation(
  database,
  plottype = NA,
  forest_reserve = NA,
  processed = TRUE
)
}
\arguments{
\item{database}{name of \code{Fieldmap}/Access database (with specific \code{Fieldmap}
structure) including path}

\item{plottype}{possibility to select only data for a certain plot type, e.g.
'CP' for Circular plot or 'CA' for Core area
(the default NA means that data from all plots are retrieved)}

\item{forest_reserve}{possibility to select only data for 1 forest reserve
by giving the name of the forest reserve (the default NA means that data
from all plots are retrieved)}

\item{processed}{Should only processed and surveyed data be added?
Defaults to TRUE (yes).}
}
\value{
Dataframe with vegetation data, containing columns as
\code{total_herb_cover}, \code{total_shrub_cover}, \code{total_tree_cover},
\code{total_soildisturbance_game},
\code{date_vegetation} (= date of vegetation survey),
\code{year_main_survey} (= year of vegetation survey), ....
}
\description{
This function queries the given database to retrieve data on vegetation
(ready for use in calculate_vegetation function).
}
\examples{
library(forrescalc)
# (add path to your own fieldmap database here)
path_to_fieldmapdb <-
  system.file("example/database/mdb_bosres.sqlite", package = "forrescalc")
load_data_vegetation(path_to_fieldmapdb)

}
