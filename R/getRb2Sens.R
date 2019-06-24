# R-Code to calculate Q10-value based on SCAPE Copyright (C) 2013 Fabian Gans,
# Miguel Mahecha This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your option)
# any later version.  This program is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
# Public License for more details.  You should have received a copy of the GNU
# General Public License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

#' Calculate basal respiration
#'
#' @description
#'Calculate basal respiration from decomposed signal and Q10 value
#'
#' @param tau_lf numeric vector: low frequency component of normalized temperature time series (T - Tref)/gamma
#' @param rho_lf numeric vector: low frequency component of logarithmic respiration time series log(respiration)
#' @param tau numeric vector: normalized temperature time series e.g. (T - Tref)/gamma
#' @param rho numeric vector: logarithmic respiration time series log(respiration)
#' @param S numeric: estimated Q10 value or
#'
#' @details
#' This function can be useful to calculate predicted Rb for surrogate and time-lagged SCAPE output data
#' @return
#' @export
#'
#' @examples
#' @author
#' Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
#'
getRb2Sens <- function(tau_lf, rho_lf, tau, rho, S) {

  # print(paste('The param value is: ', Q10, sep = ''))
  rho_lf_tau <- (tau_lf + mean(tau)) * S

  ## value<< time series of estimated basal respiration
  return(exp(rho_lf + mean(rho) - rho_lf_tau))
}
