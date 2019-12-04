#' Aggregation function for Ecosystem Functional Properties
#'
#' @param x EFPs to be aggregated.
#' @param aggregation.time Can be "day", "month", "year", growing season "gs". if it's
#' a number it's considered as a time window in number of days. If "optim" the optimum number of days will be
#' estimate to reduce the sd of each chunck.
#' @param aggregation.metric Can be "mean", "max", "min", "median", and "quant". if "quant" a number between 0 and 1 need to be provided in prob parameter.
#' @param dates A vector of class "Date" of the same length as x.
#' @param overlapping Can be "NULL" or a number. If it's a number equivale to the parameter by of the \cite{\link[zoo]{rollapply}} function.
#' @param prob Only used if aggregation.metric is "quant", a number between 0 and 1. By default 0.9.
#'
#' @return
#' @export
#'
#' @examples
aggreg <- function(x, aggregation.time, aggregation.metric, dates, overlapping = NULL, probs){
  if (is.null(aggregation.time) == F) {
    if (is.numeric(aggregation.time) == F) {
      if (aggregation.time == "month") {
        ind <- format(dates, "%Y-%m")
      } else if (aggregation.time == "year") {
        ind <- lubridate::year(dates)
      } else if (aggregation.time == "day") {
        ind <- format(dates, "%Y-%m-%d")
      }
      return(tapply(x, INDEX = ind, FUN = aggregation.metric, probs = probs, na.rm = T))
    }
  }else{
    if (is.null(overlapping)) {
      width <- by <- aggregation.time
    }else{
      width <- aggregation.time
      by <- overlapping
    }
    return(zoo::rollapply(x, width = width, FUN = aggregation.metric, by = by, probs = probs, na.rm = T))
  }

}
