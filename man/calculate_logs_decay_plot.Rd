% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_logs_decay_plot.R
\name{calculate_logs_decay_plot}
\alias{calculate_logs_decay_plot}
\title{aggregate parameters by decay stage, plot and year}
\usage{
calculate_logs_decay_plot(data_deadwood)
}
\arguments{
\item{data_deadwood}{dataframe on logs with variables plot_id, plottype, date_dendro, species, decaystage, calc_volume_m3, period and year (output of function load_data_deadwood())}
}
\value{
dataframe with columns plot, year, decaystage, vol_log_m3_ha
}
\description{
This function calculates for each plot and year the volume logs per hectare and per decay stage.
}
\examples{
\dontrun{
#change path before running
library(forrescalc)
data_deadwood <-
  load_data_deadwood("C:/MDB_BOSRES_selectieEls/FieldMapData_MDB_BOSRES_selectieEls.accdb")
calculate_logs_decay_plot(data_deadwood)
}

}
