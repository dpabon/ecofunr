#' GPPsat
#'
#' Ecosystem Photosynthetic Capacity with light saturated
#'
#'
#' @param GPP GPP time series. Object of class "vector".
#' @param Radiation Radiation (PAR or APAR) time series. Object of class "vector".
#' @param method Light response curve model. "NRHLR" or "RHLR". See Details.
#' @param method_optim See Details.
#' @param saturation Saturation of the radiation. By default 1500. See Details.
#' @param Amax Plateau of the light response. Could be "quantile" to use GPP quantile or a number.
#' @param probs If Amax is "quantile" probability must be provied (Number between 0-1). By default 0.90.
#' @param alfa Inital slope of the light response curve. By default 0.5.
#' @param Rd Non-linear response curve intercept. By default 0.
#' @param conv Curvature paramenter. Ranging from 0 to 1. By default 0.1
#' @param modelling_effiency Quality control for the model evaluation. A number between 0 and 1. By default 0.4
#' @param ts Time series output. True or False. By default TRUE. See Details.
#' @param overlap Rolling moving window type. Could be TRUE or FALSE. See Details.
#' @param length_day Number of measures per day. By default 48
#' @param width Window width (days). Object of class "number" or "vector". See Details.
#' @param window_method Method used to generate the time series. Could be "left", "center", "right". See details.
#'
#' @return If ts = TRUE the result is a vector. If ts = FALSE the result is a single value.
#' @details
#'The Ecosystem Photosynthetic Capacity represents the ecosystem potential to uptake CO2 from ecosystem.
#'
#'PAR can be estimated as SWIN * 2.11  (Britton & Dodd, 1976)
#'
#' To include (Musavi et al., 2016)
#'
#'@examples
#'
#'
#'
#'
NRHRF <- function(theta, ppfd){

  Amax <- theta[1]
  alfa <- theta[2]
  Rd <- theta[3]
  conv <- theta[4]

  tmp <- (alfa*ppfd + Amax) - sqrt(((alfa*ppfd + Amax)^2) - (4*alfa*ppfd*conv*Amax))
  sim <- (tmp/(2*conv)) + Rd

  return(sim)

}


gppsat <- function(GPP,
                   Radiation,
                   saturation = 1500,
                   method = "NRHLR",
                   method_optim = "L-BFGS-B",
                   Amax = "quantile",
                   probs=0.90,
                   alfa = 0.5,
                   Rd = 0,
                   conv = 0.1,
                   modelling_effiency = 0.4,
                   ts = T,
                   overlap = T,
                   window_method = "center",
                   length_day = 48,
                   width = 5) {
  # GPP : a temporal serie of GPP
  # Radiation: a temporal serie could be "APAR", "PAR" with the same length of GPP
  # window_size : temporal window size. by default 5
  # length_day: number of data per day. by default 48 that means measures each half hour.
  # saturation : Radiation saturation to recalculate GPPsat, by default 1500
  # Amax: Could be "quantile" or a constant. If "quantile" is selected is necesary especify probs (quantile probability)
  # alfa : initial slope  of the light response curve to be optimized (by default is 0.50).
  # Rd : ... to be optimized (by default 0).
  # conv : (THETA) curvature parameter to be optimized (ranging from 0 to 1, by default 0.1).
  # saving the parameters

  GPP <- as.vector(GPP)
  Radiation <- as.vector(Radiation)

  if (length(GPP) != length(Radiation)) {
    stop("GPP and Radiation don't have the same length")
  }
  if (Amax != "quantile" & is.numeric(Amax) == FALSE) {
    stop("Amax must be 'quantile' or a number")
  }
  if (Amax == "quantile" & (probs > 1 || probs < 0)) {
    stop("probs must be a number between 0 and 1")
  }
  if (conv > 1 || conv < 0) {
    stop("conv must be a number between 0 an 1")
  }
  if (is.numeric(modelling_effiency) == F) {
    stop("modelling effiency must be a number")
  }
  if (modelling_effiency > 1 || modelling_effiency < 0) {
    stop("modelling effiency must be a number between 0 and 1")
  }
  if (is.logical(ts) == FALSE) {
    stop("ts must be TRUE or FALSE")
  }
  if (is.logical(overlap) == FALSE) {
    stop("overlap must be TRUE or FALSE")
  }
  if (length_day > length(GPP) || length_day < 1) {
    stop("lenght day can't be greather than GPP lenght or less than 1")
  }
  if (width > length(GPP)) {
    stop("width can't be greather than GPP lenght")
  }

  if (ts == T) {
    GPP <- matrix(GPP, ncol = (length(GPP) / length_day))
    Radiation <- matrix(Radiation, ncol = (length(Radiation) / length_day))
    if (overlap == T) {
      out <- rep(NA, length = ncol(GPP))
      gppsat_t <- function(GPP, Radiation, saturation, Amax, probs, alfa, Rd, conv, modelling_effiency, min_arr, max_arr) {
        subdat <- data.frame(GPP = as.vector(GPP[,min_arr:max_arr]),
                             Radiation = as.vector(Radiation[,min_arr:max_arr]))

        subdat <- na.omit(subdat)

        if (nrow(subdat) < 10) {
          warning("The temporal window contain less than 10 values, please consider use a bigger size window", inmediate = T, noBreaks. = T)
        }
        if (Amax == "quantile") {
          theta <- c(quantile(as.vector(subdat$GPP), probs = probs, na.rm = T)[[1]], alfa, Rd, conv)
        } else {
          theta <- c(Amax, alfa, Rd, conv)
        }
        if (method == "NRHRF") {

        } else {

        }
        try(res.optim <- optim(theta, function(theta, ppfd, Fc) {
          # NonRectangular Hyperbolic light Response

          # see Musavi et al., (2016) eq 1. (Gilmanov et al., 2003). However this function incorporate
          # the current GPP values (FC)

          Amax <- theta[1]
          # Amax: the plateau of the light response curve (GPP quantile 0.90)
          alfa <- theta[2]
          # alfa: initial slope  of the light response curve. by default is 0.50
          Rd <- theta[3]
          # Rd: ... . By default is 0
          conv <- theta[4]
          # conv: (THETA) is the curvature parameter (ranging from 0 to 1) . By default is 0.1

          # ppfd: (Q) the incoming radiation used to drive the model (PAR or APAR)
          # Fc: Current GPP without NAs and when Rg is greather than 0

          tmp <- (alfa * ppfd + Amax) - sqrt(((alfa * ppfd + Amax)^2) - (4 * alfa * ppfd * conv * Amax))
          sim <- (tmp / (2 * conv)) + Rd
          MAD <- sum(abs(sim - Fc))
          # MAD don't have any use

          RSS <- sum((sim - Fc)^2)
          # RSS: Residual sum square (function cost). the optimization of this values are the input of the NRHRF function
          RMSE <- sqrt(sum((sim - Fc)^2) / length(Fc))
          # RMSE don't have any use

          if (conv > 1 | conv < 0) {
            (RSS <- RSS*100)
          }
          if (alfa < 0) {
            (RSS <- RSS * 100)
          }
          return(RSS)
        },
        ppfd = subdat$Radiation,
        Fc = subdat$GPP,
        method = method_optim,
        lower = c(0, 0, -10, 0.0001),
        upper = c(200, 20, 50, 1)))

        if (exists("res.optim") == TRUE) {
          # require sirad added
          try(outstats <- sirad::modeval(NRHRF(res.optim$par,subdat$Radiation),subdat$GPP))
          # evaluate the model effiency.
          # In this case res.optim$par is the four values of the theta object (see NRHHRF_function.R file) optimized

          if (exists("outstats") == TRUE) {
            outstats.EF <- outstats$EF
            #if the model effiency is less than 0.40 the output is NA.
            if (outstats.EF <= modelling_effiency | is.na(outstats.EF)) {
              warning(paste("The model effiency is less than", modelling_effiency, "between", min_arr, ":", max_arr, "values. NA returned"),
                      call. = T,
                      noBreaks. = T)
              output <- NA
            }else {
              # extracting GPP at 1500 par or apar. res.optim$par NOT MEAN THAT THE FUNCTION USE par.
              NRHRF <- function(theta, ppfd){

                Amax <- theta[1]
                alfa <- theta[2]
                Rd <- theta[3]
                conv <- theta[4]

                tmp <- (alfa*ppfd + Amax) - sqrt(((alfa*ppfd + Amax)^2) - (4*alfa*ppfd*conv*Amax))
                sim <- (tmp/(2*conv)) + Rd

                return(sim)

              }

              RHRF <- function(theta, ppfd){
                Fmax <- theta[1]
                alfa <- theta[2]
                Reco <- theta[3]
                sim <- (Fmax*alfa*ppfd)/(alfa*ppfd + Fmax) + Reco
                return(sim)
              }

              GPPsat <- NRHRF(res.optim$par,saturation)
              # If the model effiency is greather than 0.4 apply the NRHRF function and estimate the GPPsat.
              output <- GPPsat
            }
          } else {
            output <- NA
          }

        } else {
          warning(paste("The optimization was not possible between", min_arr, ":", max_arr, "NA returned", sep = " " ) , immediate. = T)
          output <- NA
        }
        return(output)
      }
      if (window_method == "center") {
        first_k <- 1 + trunc(width/2)
        last_k <- ncol(GPP) - trunc(width/2)
        half_k <- trunc(width/2)
        for (i in first_k:last_k) {
          min_arr <- i - half_k
          max_arr <- i + half_k
          out[i] <- gppsat_t(GPP = GPP, Radiation = Radiation, saturation = saturation, Amax = Amax, probs = probs, alfa = alfa, Rd = Rd, conv = conv, modelling_effiency = modelling_effiency, min_arr = min_arr, max_arr = max_arr)
        }
        return(out)
      }

      if (window_method == "right") {
        first_k <- 1 + width
        last_k <- ncol(x)
        for (i in first_k:last_k) {
          min_arr <- i - width
          max_arr <- i - 1
          out[i] <- gppsat_t(GPP = GPP, Radiation = Radiation, saturation = saturation, Amax = Amax, probs = probs, alfa = alfa, Rd = Rd, conv = conv, modelling_effiency = modelling_effiency, min_arr = min_arr, max_arr = max_arr)
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
          out[i] <- gppsat_t(GPP = GPP, Radiation = Radiation, saturation = saturation, Amax = Amax, probs = probs, alfa = alfa, Rd = Rd, conv = conv, modelling_effiency = modelling_effiency, min_arr = min_arr, max_arr = max_arr)
        }
        return(out)
      }
    } else {
      if (length(width) == 1) {
        out <- rep(NA, times = ncol(x) / width)
        cont <- 1
        for (i in 1:(ncol(x) / width)) {
          out[i] <- gppsat_t(GPP = GPP, Radiation = Radiation, saturation = saturation, Amax = Amax, probs = probs, alfa = alfa, Rd = Rd, conv = conv, modelling_effiency = modelling_effiency, min_arr = cont, max_arr = width*i)
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
          out[i] <- gppsat_t(GPP = GPP, Radiation = Radiation, saturation = saturation, Amax = Amax, probs = probs, alfa = alfa, Rd = Rd, conv = conv, modelling_effiency = modelling_effiency, min_arr = cont, max_arr = width*i)
          cont <- width[i] + 1
        }
        return(out)
      }
    }
  }else {
      subdat <- data.frame(GPP = as.vector(GPP), Radiation = as.vector(Radiation))
      subdat <- na.omit(subdat)

      if (nrow(subdat) > 10) {
        warning("The temporal window contain less than 10 values, please consider use a bigger size window", inmediate = T, noBreaks. = T)
      }
      if (Amax == "quantile") {
        theta <- c(quantile(as.vector(subdat$GPP), probs = probs, na.rm = T)[[1]], alfa, Rd, conv)
      } else {
        theta <- c(Amax, alfa, Rd, conv)
      }
      try(res.optim <- optim(theta, function(theta, ppfd, Fc) {
        # NonRectangular Hyperbolic light Response

        # see Musavi et al., (2016) eq 1. (Gilmanov et al., 2003). However this function incorporate
        # the current GPP values (FC)

        Amax <- theta[1]
        # Amax: the plateau of the light response curve (GPP quantile 0.90)
        alfa <- theta[2]
        # alfa: initial slope  of the light response curve. by default is 0.50
        Rd <- theta[3]
        # Rd: ... . By default is 0
        conv <- theta[4]
        # conv: (THETA) is the curvature parameter (ranging from 0 to 1) . By default is 0.1

        # ppfd: (Q) the incoming radiation used to drive the model (PAR or APAR)
        # Fc: Current GPP without NAs and when Rg is greather than 0

        tmp <- (alfa * ppfd + Amax) - sqrt(((alfa * ppfd + Amax)^2) - (4 * alfa * ppfd * conv * Amax))
        sim <- (tmp / (2 * conv)) + Rd
        MAD <- sum(abs(sim - Fc))
        # MAD don't have any use

        RSS <- sum((sim - Fc)^2)
        # RSS: Residual sum square (function cost). the optimization of this values are the input of the NRHRF function
        RMSE <- sqrt(sum((sim - Fc)^2) / length(Fc))
        # RMSE don't have any use

        if (conv > 1 | conv < 0) {
          (RSS <- RSS*100)
        }
        if (alfa < 0) {
          (RSS <- RSS * 100)
        }
        return(RSS)
      },
      ppfd = subdat$Radiation,
      Fc = subdat$GPP,
      method = method_optim,
      lower = c(0, 0, -10, 0.0001),
      upper = c(200, 20, 50, 1)))

      if (exists("res.optim") == TRUE) {
        # require sirad added
        try(outstats <- sirad::modeval(NRHRF(res.optim$par,subdat$APAR),subdat$GPP))
        # evaluate the model effiency.
        # In this case res.optim$par is the four values of the theta object (see NRHHRF_function.R file) optimized

        if (exists("outstats") == TRUE) {
          outstats.EF <- outstats$EF
          #if the model effiency is less than 0.40 the output is NA.
          if (outstats.EF <= modelling_effiency) {
            warning(paste("The model effiency is less than", modelling_effiency, "value. NA returned"),
                    call. = T,
                    noBreaks. = T)
            output <- NA
          }else {
            # extracting GPP at 1500 par or apar. res.optim$par NOT MEAN THAT THE FUNCTION USE par.
            NRHRF <- function(theta, ppfd){

              Amax <- theta[1]
              alfa <- theta[2]
              Rd <- theta[3]
              conv <- theta[4]

              tmp <- (alfa*ppfd + Amax) - sqrt(((alfa*ppfd + Amax)^2) - (4*alfa*ppfd*conv*Amax))
              sim <- (tmp/(2*conv)) + Rd

              return(sim)

            }
            GPPsat <- NRHRF(res.optim$par,saturation)
            # If the model effiency is greather than 0.4 apply the NRHRF function and estimate the GPPsat.
            output <- GPPsat
          }
        } else {
          output <- NA
        }

      } else {
        warning(paste("The optimization was not possible NA returned", sep = " " ) , immediate. = T)
        output <- NA
      }
      return(output)
  }
}
