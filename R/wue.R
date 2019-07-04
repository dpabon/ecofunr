#' Water Use Efficiency
#'
#' @importFrom Rdpack reprompt
#' @param GPP  time series. Object of class "vector".
#' @param VPD  time series. Object of class "vector".
#' @param ET  time series. Object of class "vector". See: \code{\link[bigleaf]{LE.to.ET}}.
#' @param Gs bulk surface conductance. Object of class "vector"
#' @param method WUE index. By default "wue". Options: "wue", "iwue", "Iwue", "uwue", "all". See Details for further explanaition.
#' @param dates a vector of class "Date" of the same length of GPP.
#' @param aggregation.time Can be "NULL", "day", "month", "year", growing season "gs". if it's
#' a number it's considered as a time window in number of days. If "optim" the optimum number of days will be estimate to reduce the sd of each chunck. See \code{\link[ecofunr]{aggreg}}.
#' @param aggregation.metric Can be "mean", "max", "min", "median", and "quant". if "quant" a number between 0 and 1 need to be provided in prob parameter
#' @param overlapping Can be "NULL" or a number. If it's a number equivale to the parameter by of the \cite{\link[zoo]{rollapply}} function.
#' @param prob Only used if aggregation.metric is "quant", a number between 0 and 1. By default 0.9.
#' @return An object of type "data.frame" if wue.v = "all". Otherwise an object of the class "vector".
#'
#' @description
#' This function estimate different Water use efficiency index.
#' Water use efficiency
#'
#' @details
#' \strong{Water Use Efficiency (wue) \insertCite{law_environmental_2002}{ecofunr}}
#'
#' \deqn{WUE = \frac{GPP (gCm^{-2}s^{-1})}{ET (kg H_{2}Om^{-2}s^{-1})}}{WUE = GPP / ET}
#'
#' \strong{Intrinsic Water use efficiency (iwue) See: \insertCite{beer_temporal_2009-1}{ecofunr}}
#'
#' \deqn{iWUE = \frac{GPP}{G_{s}}}{iWUE = GPP / Gs}
#'
#' where Gs is the bulk surface conductance. See \code{\link[bigleaf]{surface.conductance}}
#'
#' \strong{Inherent Water Use Efficiency (Iwue) \insertCite{beer_temporal_2009-1}{ecofunr}}
#'
#' \deqn{IWUE = \frac{GPP*VPD}{ET}}{IWUE = GPP * VPD / ET}
#'
#' \strong{Underlyng Water Use Efficiency (uwue)} \insertCite{zhou_effect_2014}{ecofunr}
#'
#' \deqn{uWUE = \frac{GPP * \sqrt{VPD}{ET}}{uWUE = GPP * sqrt(VPD) / ET}
#'
#' \strong{Undelying Water Use Efficiency using transpiration from TEA algorithm (teauWUE)} \insertCite{nelson_coupling_2018-1}{ecofunr}
#'
#' \deqn{teauWUE = \frac{GPP * \sqrt{VPD}}{T}}{teauWUE = GPP * sqrt(VPD) / T}
#'
#' where T is Transpiration derived from TEA algorithm \insertCite{nelson_coupling_2018-1}{ecofunr}
#'
#'
#' @export
#'
#' @examples
#'
#' @references
#' \insertAllCited{}
#'
wue <- function(GPP, VPD, ET, Gs, method = "wue", dates, aggregation.time = "NULL", aggregation.metric, overlapping = F, prob = 0.9) {
 # to check arguments and program error messages
  # three options for wue.v = wue (water use efficiency), iwue (intrinsic water use efficiency), Iwue (inherent water use efficiency), uwue (underlying water use efficiency)

  # To check the length of the objects
  Check <- ArgumentCheck::newArgCheck()
  test <- c(length(GPP), length(VPD), length(ET), length(Gs))
  if (length(unique(test)) != 0) {
    ArgumentCheck::addError(msg = "Vectors don't have the same length", argcheck = Check)
  }

  ArgumentCheck::finishArgCheck(Check)

  if (method == "wue") {
    wue <- GPP / ET
    if (is.null(aggregation.time)) {
      return(wue)
    }else{
      return(aggreg(wue, aggregation.time, aggregation.metric, dates, overlapping, prob))
    }
  }
  else if (method == "iwue") {
    wue <- GPP / Gs
    if (is.null(aggregation.time)) {
      return(wue)
    }else{
      return(aggreg(wue, aggregation.time, aggregation.metric, dates, overlapping, prob))
    }
  }
  else if (method == "Iwue") {
    wue <- GPP * VPD / ET
    if (is.null(aggregation.time)) {
      return(wue)
    }else{
      return(aggreg(wue, aggregation.time, aggregation.metric, dates, overlapping, prob))
    }
  }
  else if (method == "uwue") {
    wue <- GPP * sqrt(VPD) / ET
    if (is.null(aggregation.time)) {
      return(wue)
    }else{
      return(aggreg(wue, aggregation.time, aggregation.metric, dates, overlapping, prob))
    }
  }
  else if (method == "all") {
    wue <- GPP / ET
    iwue <- GPP / Gs
    Iwue <- GPP * VPD / ET
    uwue <- GPP * sqrt(VPD) / ET
    if (is.null(aggregation.time)) {
      return(data.frame(wue = wue, intrins.wue = iwue, inher.wue = wuei, under.wue = uwue))
    }else{
      wue <- aggreg(wue, aggregation.time, aggregation.metric, dates, overlapping, prob)
      iwue <- aggreg(iwue, aggregation.time, aggregation.metric, dates, overlapping, prob)
      Iwue <- aggreg(Iwue, aggregation.time, aggregation.metric, dates, overlapping, prob)
      uwue <- aggreg(uwue, aggregation.time, aggregation.metric, dates, overlapping, prob)
      return(data.frame(wue = wue, intrinc.wue = iwue, inher.wue = wuei, under.wue = uwue))
    }
  }
}
