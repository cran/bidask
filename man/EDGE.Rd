% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edge.R
\name{edge}
\alias{edge}
\title{Efficient Estimation of Bid-Ask Spreads from Open, High, Low, and Close Prices}
\usage{
edge(open, high, low, close, sign = FALSE)
}
\arguments{
\item{open}{numeric vector of open prices.}

\item{high}{numeric vector of high prices.}

\item{low}{numeric vector of low prices.}

\item{close}{numeric vector of close prices.}

\item{sign}{whether signed estimates should be returned.}
}
\value{
The spread estimate. A value of 0.01 corresponds to a spread of 1\%.
}
\description{
Implements an efficient estimator of bid-ask spreads 
from open, high, low, and close prices as described in 
\href{https://www.ssrn.com/abstract=3892335}{Ardia, Guidotti, & Kroencke (2021)}.
}
\details{
Prices must be sorted in ascending order of the timestamp.
}
\note{
Please cite \href{https://www.ssrn.com/abstract=3892335}{Ardia, Guidotti, & Kroencke (2021)} 
when using this package in publication.
}
\examples{
# simulate open, high, low, and close prices with spread 1\%
x <- sim(spread = 0.01)

# estimate the spread
edge(x$Open, x$High, x$Low, x$Close)

}
\references{
Ardia, D., Guidotti E., & Kroencke T. A. (2021). Efficient Estimation of Bid-Ask Spreads from Open, High, Low, and Close Prices. 
Available at SSRN: \url{https://www.ssrn.com/abstract=3892335}
}
