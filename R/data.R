#' Sample of half-hourly eddy covariance data used for EFPs calclutaion.
#'
#' The dataset contains two years of measurements: 2015-01-01 to 2018-02-28.
#'
#' @format A data frame with 39408 rows and 26 variables:
#' \describe{
#'   \item{year}{Year of measurement}
#'   \item{month}{Month of measurement}
#'   \item{doy}{Day of year (1 - 366)}
#'   \item{hour}{Hour (0 - 23.5)}
#'   \item{Tair}{Air temperature (degC)}
#'   \item{PPFD}{Photosynthetic photon flux density (umol m-2 s-1)}
#'   \item{VPD}{Vapor pressure deficit (kPa)}
#'   \item{pressure}{Atmospheric pressure (kPa)}
#'   \item{precip}{Precipitation (mm)}
#'   \item{ustar}{Friction velocity (m s-1)}
#'   \item{wind}{Horizontal wind velocity (m s-1)}
#'   \item{Ca}{CO2 concentration (ppm)}
#'   \item{LW_up}{Upward longwave radiation (W m-2)}
#'   \item{LW_down}{Downward longwave radiation (W m-2)}
#'   \item{Rn}{Net radiation (W m-2)}
#'   \item{LE}{Latent heat flux (W m-2)}
#'   \item{LE_qc}{Quality control of LE}
#'   \item{H}{Sensible heat flux (W m-2)}
#'   \item{H_qc}{Quality control of H}
#'   \item{G}{Ground heat flux (W m-2)}
#'   \item{NEE}{Net ecosystem exchange (umol m-2 s-1)}
#'   \item{NEE_qc}{Quality control of NEE}
#'   \item{GPP}{Gross primary productivity from nighttime partitioning (umol m-2 s-1)}
#'   \item{GPP_qc}{Quality control of GPP}
#'   \item{Reco}{Ecosystem respiration from nighttime partitioning (umol m-2 s-1)}
#'   \item{date}{date as POSIXct}
#' }
#' @source \url{https://zenodo.org/record/1314194#.Xee-WuhKiF5} \doi{10.5281/zenodo.1314194}
"ec_hh"

#' Sample of daily eddy covariance data used for EFPs calclutaion.
#'
#' The dataset contains two years of measurements: 2015-01-01 to 2018-02-28.
#'
#' @format A data frame with 821 rows and 25 variables:
#' \describe{
#'   \item{year}{Year of measurement}
#'   \item{month}{Month of measurement}
#'   \item{doy}{Day of year (1 - 366)}
#'   \item{Tair}{Air temperature (degC)}
#'   \item{PPFD}{Photosynthetic photon flux density (umol m-2 s-1)}
#'   \item{VPD}{Vapor pressure deficit (kPa)}
#'   \item{pressure}{Atmospheric pressure (kPa)}
#'   \item{precip}{Precipitation (mm)}
#'   \item{ustar}{Friction velocity (m s-1)}
#'   \item{wind}{Horizontal wind velocity (m s-1)}
#'   \item{Ca}{CO2 concentration (ppm)}
#'   \item{LW_up}{Upward longwave radiation (W m-2)}
#'   \item{LW_down}{Downward longwave radiation (W m-2)}
#'   \item{Rn}{Net radiation (W m-2)}
#'   \item{LE}{Latent heat flux (W m-2)}
#'   \item{LE_qc}{Quality control of LE}
#'   \item{H}{Sensible heat flux (W m-2)}
#'   \item{H_qc}{Quality control of H}
#'   \item{G}{Ground heat flux (W m-2)}
#'   \item{NEE}{Net ecosystem exchange (umol m-2 s-1)}
#'   \item{NEE_qc}{Quality control of NEE}
#'   \item{GPP}{Gross primary productivity from nighttime partitioning (umol m-2 s-1)}
#'   \item{GPP_qc}{Quality control of GPP}
#'   \item{Reco}{Ecosystem respiration from nighttime partitioning (umol m-2 s-1)}
#'   \item{date}{date as POSIXct}
#' }
#' @source \url{https://zenodo.org/record/1314194#.Xee-WuhKiF5} \doi{10.5281/zenodo.1314194}
"ec_dd"
