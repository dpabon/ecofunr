#' Evaporative Fraction
#'
#' Evaporative fraction is the ration between Latent heat and the sum of sensible heat and latent heat.....
#'
#' @param LE Latent heat (units).
#' @param H Sensible heat (units).
#' @param dates Dates vector.
#' @param aggregation.time
#' @param aggregation.metric
#' @param overlapping
#' @param prob
#'
#' @description
#'
#' XXXXX. \insertCite{nichols_evaluation_1993}.
#'
#' @return
#' @export
#'
#'
#' @references
#' \insertAllCited{}

#' @examples
#'
#'
ef <- function(LE, H, dates, aggregation.time = "NULL", aggregation.metric, overlapping = F, prob = 0.9) {
  EF <- LE / (LE + H)
  if (is.null(aggregation.time)) {
    return(EF)
  }else{
    return(aggreg(EF, aggregation.time, aggregation.metric, dates, overlapping, prob))
  }
}
