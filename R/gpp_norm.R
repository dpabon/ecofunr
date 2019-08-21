#' Normalized GPP
#'
#' @param GPP GPP time series (half-hourly). Object of the class "vector"
#' @param fparc fpar normalized at (by default) 0.99
#' @param fpar fpar time series (daily). Object of the class "vector"
#' @param theta zenith angle during the solstice
#' @param dates A vector with half-hourly dates of class "Date" or class "numeric" (ISO timestamp). In FLUXNET correspond to the column "TIMESTAMP_START"
#' @param na.rm
#'
#' @return A vector of class "numeric".
#'
#' @details
#' \strong{Normalized Canopy photosythesis} computed as in \insertCite{kergoat_nitrogen_2008-1}{ecofunr}:
#'
#'\deqn{GPP* = GPPmax*\frac{fPAR* cos(\theta = 0)}{fParc * cos(\theta_{s})}}{GPP* = GPPmax * (fPARc * cos(\theta = 0)) / (fPAR * cos(\theta_{s}))}
#'
#'Where:
#'
#'GPPmax: Quantile 0.985-0.99 of the half-hourly GPP for the period of interest (per day).
#'fPAR: Fraction of absorbed PAR.
#'fPARc: Reference fPAR ofr closed canopy conditions (by default 0.95).
#'\theta_{s}: The sun elevation at solstice.
#'
#'
#' @export
#'
#' @examples
#'
#' #' @references
#' \insertAllCited{}
#'
#'
gpp_normalized <- function(GPP,
                           fparc = 0.99,
                           fpar,
                           theta,
                           dates,
                           na.rm = T){
  GPP <- as.vector(GPP)
  fpar <- as.vector(fpar)
  if (length(GPP) != length(fpar)) {
    stop("GPP and fpar don't have the same length")
  }else{
    if (is.numeric(dates)) {
      dates <- lubridate::ymd_hm(dates)
    }
    dates <- as.Date(dates, "%Y%m%d")

    GPPmax <- tapply(GPP, INDEX = as.character(dates), FUN = quantile, prob = 0.95)

    gpp.norm <- GPPmax * ((fparc/fpar)*(cos(0)/cos(theta)))

    return(gpp.norm)
  }
}


