#' Title
#'
#' @param GPP 
#' @param VPD 
#' @param ET 
#' @param Gs 
#' @param wue.v 
#'
#' @return
#' @export
#'
#' @examples
WUE <- function(GPP, VPD, ET, Gs, wue.v = "wue") {
 # to check arguments and program error messages
  # three options for wue.v = wue (water use efficiency), iwue (intrinsic water use efficiency), wuei (inherent water use efficiency), uwue (underlying water use efficiency), all
  
  if (wue.v == "wue") {
    # check that exist the arguments
    wue <- GPP / ET
    return(wue)
  }
  else if (wue.v == "iwue") {
    wue <- GPP / Gs
    return(wue)
  }
  else if (wue.v == "wuei") {
    wue <- GPP * VPD / WUE
    return(wue)
  }
  else if (wue == "uwue") {
    wue <- GPP * sqrt(VPD) / ET
    return(wue)
  }
  else if (wue.v == "all") {
    wue <- GPP / ET
    iwue <- GPP / Gs
    wuei <- GPP * VPD / WUE
    uwue <- GPP * sqrt(VPD) / ET
    return(data.frame(wue = wue, intrinc.wue = iwue, inher.wue = wuei, under.wue = uwue))
  }
}

