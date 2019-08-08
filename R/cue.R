#' Carbon Use Efficiency
#'
#' @importFrom Rdpack reprompt
#'
#' @param GPP Gross Primary Production (units)
#' @param NEP Net Ecosystem Productivity (units)
#' @param NPP Net Primary Productivity
#' @param Rb Basal Respiration (See Reddyproc function XXX)
#' @param method Can be "plants", "ecosystem", "apparent", "all". See details.
#' @param dates a vector of class "Date" with the same length of GPP.
#' @param aggregation.time Can be "NULL", "day", "month", "year", growing season "gs". if it's
#' a number it's considered as a time window in number of days. If "optim" the optimum number of days will be estimate to reduce the sd of each chunck. See \code{\link[ecofunr]{aggreg}}.
#' @param aggregation.metric Can be "mean", "max", "min", "median", and "quant". if "quant" a number between 0 and 1 need to be provided for prob parameter.
#' @param overlapping Can be "NULL" or a number. If it's a number equivale to the parameter by of the \cite{\link[zoo]{rollapply}} function.
#' @param prob Only used if aggregation.metric is "quant", a number between 0 and 1. By default 0.9.
#'
#' @return An object of type "data.frame" if method = "all". Otherwise a numeric vector.
#'
#' @export
#'
#' @description
#' This function estimate different metrics of Carbon Use Efficiency
#' @section Methods:
#'
#' \strong{plants}
#' Carbon Use efficiency (CUE) provides a measure of what fraction of total carbon assimilation becomes
#' incorported into new tissues. \insertCite{chambers_respiration_2004}{ecofunr}
#'
#' \deqn{CUE = \frac{NPP}{GPP}}{CUE = NPP / GPP}
#'
#' \strong{ecosystem} \insertCite{fernandez-martinez_nutrient_2014}{ecofunr}:
#'
#' \deqn{CUE = \frac{NEP}{GPP}}{CUE = NEP / GPP}
#'
#' \strong{apparent} (Reference is missing)
#'
#' \deqn{CUE = 1 - \frac{Rb}{GPP}}{CUE = 1 - (Rb / GPP)}
#'
#' For a complete review about all CUE metrics at different spatial and temporal scales check \insertCite{manzoni_reviews_2018}{ecofunr}
#' @references
#' \insertAllCited{}
#'
#' @examples
cue <- function(GPP, NEP, NPP, Rb, method = "plants", dates, aggregation.time = "NULL", aggregation.metric, overlapping = F, prob = 0.9) {
  # parameters controls
  if (method == "plants") {
    cue <-  NPP / GPP
    if (is.null(aggregation.time)) {
      return(cue)
    }else{
      return(aggreg(cue, aggregation.time, aggregation.metric, dates, overlapping, prob))
    }
  }
  else if (method == "ecosystem") {
    cue <- NEP / GPP
    if (is.null(aggregation.time)) {
      return(cue)
    }else{
      return(aggreg(cue, aggregation.time, aggregation.metric, dates, overlapping, prob))
    }
  }
  else if (method == "apparent") {
    cue <- 1 - (Rb / GPP)
    if (is.null(cue)) {
      return(cue)
    }else{
      return(aggreg(cue, aggregation.time, aggregation.metric, dates, overlapping, prob))
    }
  }
  else if (method == "all") {
    pcue <- NPP / GPP
    ecue <- NEP / GPP
    acue <- 1 - (Rb / GPP)
    if (is.null(aggregation.time)) {
      return(data.frame(plant.cue = pcue, eco.cue = ecue, appa.cue = acue))
    }else{
      pcue <- aggreg(pcue, aggregation.time, aggregation.metric, dates, overlapping, prob)
      ecue <- aggreg(ecue, aggregation.time, aggregation.metric, dates, overlapping, prob)
      acue <- aggreg(acue, aggregation.time, aggregation.metric, dates, overlapping, prob)
      return(data.frame(plant.cue = pcue, eco.cue = ecue, appa.cue = acue))
    }
  }
}
