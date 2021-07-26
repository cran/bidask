#' Roll Estimator
#'
#' @param x \code{xts} object with the column \code{Close}, representing closing prices.
#' @param width integer width of the rolling window to use, or vector of endpoints defining the intervals to use.
#' @param na.rm a \code{logical} value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each end before the spread is computed. Values of trim outside that range are taken as the nearest endpoint.
#'
#' @return Time series of spread estimates.
#'
#' @references
#' Roll, R. (1984). A simple implicit measure of the effective bid-ask spread in an efficient market. The Journal of Finance, 39 (4), 1127-1139.
#' \doi{10.1111/j.1540-6261.1984.tb03897.x}
#'
#' @keywords internal
#'
Roll <- function(x, width = nrow(x), na.rm = FALSE, trim = 0){

  # compute returns
  R1 <- x$Close/lag(x$Close, 1) - 1
  R2 <- lag(R1, 1)

  # drop leading NA
  R1 <- R1[-1]
  R2 <- R2[-c(1:2)]

  # expectations
  E1 <- rmean(R1, width = width-1, na.rm = na.rm, trim = trim)
  E2 <- rmean(R2, width = width-2, na.rm = na.rm, trim = trim)
  E12 <- rmean(R1*R2, width = width-2, na.rm = na.rm, trim = trim)

  # spread
  s <- E1*E2-E12
  s[] <- 2*sqrt(pmax(0, s))

  # set names
  colnames(s) <- "Roll"

  # return
  return(s)

}