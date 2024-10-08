% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_forresdat.R
\name{read_forresdat}
\alias{read_forresdat}
\title{load data package from git repository \code{forresdat}}
\usage{
read_forresdat()
}
\value{
A \code{frictionless} data package with all tables and metadata from
GitHub repository \code{forresdat}, which can be explored using package
\href{https://docs.ropensci.org/frictionless/}{\code{frictionless}}.
To be able to recall the version of the data, this data package contains
an attribute with the version number of the release of \code{forresdat} from which
the data are taken.
}
\description{
This function reads the data package from git repository \code{forresdat}
(and saves the \code{forresdat} data to a local temp directory to avoid unneeded
downloading in the future).
This data package contains both data and metadata and can be explored using
functions of the \href{https://docs.ropensci.org/frictionless/}{frictionless}
package.

Data available in \code{forresdat} only contain observations, so no records with
zero values are added for for instance species that were not observed and
hence absent.
These zero value records can easily be added by using the function
\code{add_zeros()}.

The different tables of this dataset contain data that are collected
using 2 different methods (plot types):
circular plots (CP) and core areas (CA).
It is advised to only use one of them for analyses, as the data are likely
to differ due to method related differences.

General information on the plot level is available in table \code{plotinfo},
which can easily be joined to other tables on \code{plot_id} and \code{period}
(or only \code{plot_id} if \code{period} is absent).
}
\examples{
library(forrescalc)
datapackage <- read_forresdat()
frictionless::resources(datapackage)
attr(datapackage, "forresdat")

}
