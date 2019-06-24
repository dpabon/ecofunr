#' LUE
#'
#' Light use effiency function.
#'
#' @param GPP Object of the class 'vector'. Matrix and dataframes are coerced as vectors.
#' @param APAR Object of class 'vector'. Matrix and dataframes are coerced as vectors.
#' @param method Could be 'max', 'mean' or 'quantile'. By default 'max'
#' @param probs if method = 'quantile' a probability need be provided. A number between 0 and 1.
#' @param ts output time series. Could be TRUE or FALSE. By default TRUE
#' @param overlap if ts = TRUE, overlap must be provided. Could be TRUE or FALSE. By default TRUE
#' @param window_method if overlap = TRUE, a window method must be provided. Could be 'right', 'center', 'left'. By default 'center' See Details.
#' @param length_day if ts = TRUE. lenght day must be provided. Number of measures per day. By default 48.
#' @param width window width (days). Must be an integer number.

#'
#' @return
#' @export
#'
#' @examples
lue <- function(GPP, APAR, method = "max", probs = 0.9, ts = T, overlap = T, length_day = 48,
  width = 5, window_method = "center") {
  GPP <- as.vector(GPP)
  APAR <- as.vector(APAR)

  if (length(GPP) != length(APAR)) {
    stop("GPP and APAR have differents lengths")
  }

  lue_temp <- function(GPP, APAR, method = method, min_arr, max_arr) {
    LUE <- as.vector(GPP[, min_arr:max_arr])/as.vector(APAR[, min_arr/max_arr])
    if (method == "max") {
      LUE <- max(LUE, na.rm = T)
      return(LUE)
    } else {
      LUE <- quantile(LUE, probs = 0.9)
      return(LUE)
    }
  }

  if (ts == TRUE) {
    GPP <- matrix(GPP, ncol = (length(GPP)/length_day))
    LUE <- matrix(LUE, ncol = (length(LUE)/length_day))

    if (overlap == TRUE) {
      out <- vector(NA, length = ncol(GPP))

      if (window_method == "center") {
        first_k <- 1 + trunc(width/2)
        last_k <- ncol(x) - trunc(width/2)
        half_k <- trunc(width/2)
        for (i in first_k:last_k) {
          min_arr <- i - half_k
          max_arr <- i + half_k
          out[i] <- lue_temp(GPP = GPP, APAR = APAR, method = method, min_arr = min_arr,
          max_arr = max_arr)
        }
        return(out)
      }

      if (window_method == "right") {
        first_k <- 1 + width
        last_k <- ncol(x)
        for (i in first_k:last_k) {
          min_arr <- i - width
          max_arr <- i - 1
          out[i] <- lue_temp(GPP = GPP, APAR = APAR, method = method, min_arr = min_arr,
          max_arr = max_arr)
        }
        return(out)
      }

      if (window_method == "left") {
        first_k <- 1
        last_k <- ncol(x) - width
        half_k <- trunc(width/2)
        for (i in first_k:last_k) {
          min_arr <- i + 1
          max_arr <- i + width
          out[i] <- lue_temp(GPP = GPP, APAR = APAR, method = method, min_arr = min_arr,
          max_arr)
        }
        return(out)
      }
    } else {
      LUE <- GPP/APAR
      if (method == "max") {
        LUE <- max(LUE, na.rm = T)
        return(LUE)
      }
      if (method == "quantile") {
        LUE <- quantile(LUE, probs = probs, na.rm = T)
        return(LUE)
      }
      if (method == "mean") {
        LUE <- mean(LUE, na.rm = T)
      }
    }

  }

}
