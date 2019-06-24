#' EFPs temporal series
#'
#' Generate a temporal series.
#' @param data.day A number. number of data per day.
#' @param width A number or a vector. Temporal window width. See Details.
#' @param overlap Specify if the moving window is overlaped or not could be TRUE or FALSE. See Details
#' @param method method used to generated the temporal series could be "center" "right" "left". See Details
#' @param fun EFPs function.
#' @details ..., is a dataframe with n columns where each column represent the variables necesaries to
#' calculate the EFPs.

timeseries <- function(..., data.day=48, width = 5, overlap = T, method = "center") {
  x <- as.vector(x)
  x <- matrix(x, ncol = length(x)/data.day)
  if (overlap == T) {
    out <- rep(NA, times = ncol(x))
    if (method == "center") {
      first_k <- 1 + trunc(width/2)
      last_k <- ncol(x) - trunc(width/2)
      half_k <- trunc(width/2)

      for (i in first_k:last_k) {
        min_arr <- i - half_k
        max_arr <- i + half_k
        out[i] <- mean(x[,min_arr:max_arr])
      }
      return(out)
    }
    if (method == "right") {
      first_k <- 1 + width
      last_k <- ncol(x)

      for (i in first_k:last_k) {
        min_arr <- i - width
        max_arr <- i - 1
        out[i] <- mean(x[,min_arr:max_arr])
      }
      return(out)
    }
    if (method == "left") {
      first_k <- 1
      last_k <- ncol(x) - width
      half_k <- trunc(width/2)

      for (i in first_k:last_k) {
        min_arr <- i + 1
        max_arr <- i + width
        out[i] <- mean(x[,min_arr:max_arr])
      }
      return(out)
    }
  } else {
    if (length(width) == 1) {
      out <- rep(NA, times = ncol(x) / width)
      cont <- 1
      for (i in 1:(ncol(x) / width)) {
        out[i] <- mean(x[,cont:(width*i)])
        cont <- cont + width
      }
      return(out)
    } else {
      if (tail(width, n = 1) != ncol(x)) {
        width <- c(width, ncol(x))
      }
      if (head(width, n = 1) == 1) {
        width <- width[-1]
      }
      out <- rep(NA, times = length(width))
      cont <- 1
      for (i in 1:length(width)) {
        out[i] <- mean(x[,cont:width[i]])
        cont <- width[i] + 1
      }
      return(out)
    }
  }
}
