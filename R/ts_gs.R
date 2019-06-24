
#' Determine the number of growing seasons in a temporal series.
#'
#'
#'
#' @param x a vector that represent a temporal serie
#' @param max.ft the maximum coeffient that will be used to trace the number of growing seassons
#'
#' @return
#' @export
#'
#' @examples
ts_gs <- function(x, max.ft=5){
  y <- as.integer(which.max(abs(fft(x))[2:max.ft]))

  return(y)
}

