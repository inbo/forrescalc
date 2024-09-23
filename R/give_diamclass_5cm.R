#' Calculate the diameter class of diameter measure data
#'
#' This function returns a factor with diameter classes of 5 cm for a given
#' vector with diameter data in mm.
#'
#' @param diameterdata vector with diameter data in millimetre
#'
#' @return vector with factors in diameter classes of 5 cm
#'
#' @export
#'
#' @examples
#' library(forrescalc)
#' give_diamclass_5cm(c(80, 1512, 2222))
#'

give_diamclass_5cm <- function(diameterdata) {
  diameterclass <-
    factor(
      ifelse(
        diameterdata >= 2500,
        49,
        floor(diameterdata / 50)
      ),
      levels = 1:49,
      labels =
        c(paste(seq(5, 240, 5), "-", seq(10, 245, 5), "cm"), "245 cm +")
    )
  return(diameterclass)
}
