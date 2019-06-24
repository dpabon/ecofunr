#' Normalized GPP
#'
#' @param GPP GPP time series. Object of the class "vector" 
#' @param fparc fpar normalized at 0.95 (units are missing)
#' @param fpar fpar time series. Object of the class "fpar"
#' @param theta zenith angle during the solsctice
#' @param na.rm 
#'
#' @return
#' @export
#'
#' @examples
gpp_normalized <- function(GPP,
                           fparc = 0.95,
                           fpar,
                           theta,
                           na.rm = T){
  # GPP: GPP time series. Object of class "vector".
  # fparc: fpar normalized at 0.95
  # theta: zenith angle during the solstice
  # na.rm : parameter for the max gpp and fpar
  GPP <- as.vector(GPP)
  fpar <- as.vector(fpar)
  if (length(GPP) != length(fpar)) {
    stop("GPP and fpar don't have the same length")
  }else{
    gpp.norm = (max(GPP, na.rm = T)) * ((fparc/fpar)*(cos(0)/cos(theta)))
    return(gpp.norm)
  }
}


