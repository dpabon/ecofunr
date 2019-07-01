gs <- function(x, dates) {
  yrs <- lubridate::year(dates)
  for (i in 1:length(yrs)) {
    salida <- time.serie[st][[1]][[1]][which(gpp.nt.vut.ref.time == u.years[y])]
    if (lubridate::leap_year(u.years[y]) == T) {
      salida  <- salida[1:365]
    } else{
      #
    }
    all.years[,y]  <- salida
  }
  # mean seasonal cycle
  msc  <- apply(X = all.years, MARGIN = 1, FUN = mean, na.rm = T)
  if (any(is.nan(msc))) {
    msc[which(is.nan(msc))] <- 0
  }
}
