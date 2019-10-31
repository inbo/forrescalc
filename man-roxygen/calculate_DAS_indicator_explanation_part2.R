#' @description
#' Second, within the forest reserves, relevant plots are selected following the above criteria (so same criteria but different level).
#'
#' And finally, forest reserves with a total studied area of less than 0.3 ha are removed. Considering A3 plots with an area of 0.0254 ha as the main studied area, forest reserves are removed if less than 12 plots are left.
#'
#' For the selected forest reserves and plots, the following calculations are made:
#' \itemize{
#'  \item arithmetric average of basal area and basal area proportion of the tree species groups for each forest reserve
#'  \item yearly change of basal area for each forest reserve: \eqn{d = (BA_t1 / BA_t0)^(1 / (t1 - t0)) - 1} with d = yearly change in \%, BA_t1 = average basal area at time 1, BA_t0 = average basal area at time 0 and t1 - t0 = number of years between time 1 and time 0
#'  \item yearly change of basal area proportion for each forest reserve: \eqn{d = (pBA_t1 / pBA_t0)^(1 / (t1 - t0)) - 1}  with d = yearly change in \%, pBA_t1 = average proportional basal area at time 1, BA_t0 = average proportional basal area at time 0 and t1 - t0 = number of years between time 1 and time 0
#' }
