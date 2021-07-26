#' @keywords internal
"_PACKAGE"

#' @import xts
#' @importFrom stats lag qt rbinom rnorm
NULL

#' Rolling sum
#' @keywords internal
rsum <- function(x, width, na.rm = FALSE){

  if(length(width) > 1)
    return(xts::period.apply(x, INDEX = width[width>=0], FUN = sum, na.rm = na.rm))

  return(zoo::rollsumr(x, k = width, na.rm = na.rm))

}

#' Rolling mean
#' @keywords internal
rmean <- function(x, width, na.rm = FALSE, trim = 0){

  if(length(width) > 1)
    return(xts::period.apply(x, INDEX = width[width>=0], FUN = mean, na.rm = na.rm, trim = trim))

  if(trim > 0)
    return(zoo::rollapplyr(x, width = width, FUN = mean, na.rm = na.rm, trim = trim)[-(1:(width-1)),])

  return(zoo::rollmeanr(x, k = width, na.rm = na.rm))

}
