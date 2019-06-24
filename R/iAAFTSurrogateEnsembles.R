#' Get random ensemble time series
#'
#' @param x input time series
#' @param nsamples number of surrogate time series
#'
#' @return
#' @export
#'
#' @examples
#'
#' @author
#' Fabian Gans, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de
.iAAFTSurrogateEnsemble <- function(x, nsamples = 10) {
  l <- length(x)
  a <- array(data = rep(x, nsamples), dim = c(l, nsamples))
  if (sd(x) == 0) {
    warning("Could not generate surrogate of constant time series")
    return(t(a))
  }
  ens <- aaply(.data = a, .margins = 2, .fun = .iAAFT, tolerance = 0.001, rel.convergence = TRUE)
  ## value<< Array containing a ensemble of time series with same distribution an
  ## spectrum as the input
  if (nsamples > 1)
    return(t(ens))
  return(ens)
}
