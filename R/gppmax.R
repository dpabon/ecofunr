


#' Title
#'
#' @param x
#' @param max.ft
#' @param timing
#' @param cyclic
#'
#' @return
#' @export
#'
#' @examples
gppmax <- function(x, max.ft = 5, timing=TRUE, cyclic = T) {
  # checking inputs
  if (is.vector(x) == F) {
    warning("x should be a vector")
  }
  if (max.ft > length(x)) {
    warning("max.ft should be less than x length")
  }
  if (is.logical(timing) == F) {
    warning("timing should be logical")
  }

  ts <- fft(x)
  cyc <- as.integer(which.max(abs(ts)[2:max.ft]))
  ny <- length(ts)
  ts[c(1,5:(ny-3))] <- 0
  yfiltered <- fft(ts,inverse = T)

  maximas <- which.max(yfiltered)[1:cyc]
  size <- 90 / cyc
  for (i in 1:cyc) {
    if (cyclic == T){

    }

  }



}
