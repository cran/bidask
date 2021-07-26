#' EDGE Estimator
#'
#' @param x \code{xts} object with columns \code{Open}, \code{High}, \code{Low}, \code{Close}, representing OHLC prices.
#' @param width integer width of the rolling window to use, or vector of endpoints defining the intervals to use.
#' @param probs vector of probabilities to compute the critical values.
#' @param na.rm a \code{logical} value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each end before the spread is computed. Values of trim outside that range are taken as the nearest endpoint.
#'
#' @return Time series of spread estimates.
#'
#' @keywords internal
#'
EDGE <- function(x, width = nrow(x), probs = c(0.025, 0.975), na.rm = FALSE, trim = 0){

  if(length(idx <- which(x$High==x$Low & x$Low==lag(x$Close, 1))))
    x[idx,] <- NA

  x <- log(x)

  O <- x$Open
  H <- x$High
  L <- x$Low
  C <- x$Close
  M <- (H+L)/2

  O1 <- lag(O, 1)[-1]
  H1 <- lag(H, 1)[-1]
  L1 <- lag(L, 1)[-1]
  C1 <- lag(C, 1)[-1]
  M1 <- lag(M, 1)[-1]

  X1 <- (M-O)*(O-C1)+(O-C1)*(C1-M1)
  X2 <- (M-O)*(O-M1)+(M-C1)*(C1-M1)

  E.X1 <- rmean(X1, width = width-1, na.rm = na.rm, trim = trim)
  E.X2 <- rmean(X2, width = width-1, na.rm = na.rm, trim = trim)

  E.X1.X1 <- rmean(X1^2,  width = width-1, na.rm = na.rm, trim = trim)
  E.X2.X2 <- rmean(X2^2,  width = width-1, na.rm = na.rm, trim = trim)
  E.X1.X2 <- rmean(X1*X2, width = width-1, na.rm = na.rm, trim = trim)

  N <- rsum(!is.na(X1) & !is.na(X2), width = width-1)
  N <- N - as.integer(N*trim)
  J <- N/(N-1)

  V11 <- J*(E.X1.X1-E.X1^2)
  V22 <- J*(E.X2.X2-E.X2^2)
  V12 <- J*(E.X1.X2-E.X1*E.X2)

  W1 <- V22/(V11+V22)
  W2 <- 1-W1

  K <- rmean(((O==H)+(O==L)+(C==H)+(C==L))/2, width = width, na.rm = na.rm)

  S2 <- (W1*E.X1+W2*E.X2)/(W1*W2*K-0.5)
  S2[is.infinite(S2)] <- NA
  colnames(S2) <- "EDGE"

  if(!is.null(probs)){
    mu <- S2
    sigma <- sqrt(W1^2*V11+W2^2*V22+2*W1*W2*V12)/(0.5-W1*W2*K)
    for(p in probs)
      S2 <- cbind(S2, mu+sigma/sqrt(N)*qt(p = p, df = N-1))
    colnames(S2)[2:ncol(S2)] <- sprintf("EDGE_%s", probs*100)
  }

  S2[S2<0] <- 0

  return(sqrt(S2))

}
