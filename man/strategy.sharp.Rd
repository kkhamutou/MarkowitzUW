% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strategy.R
\name{strategy.sharp}
\alias{strategy.sharp}
\title{Sharp ration strategy}
\usage{
strategy.sharp(marketData, riskFreeRate = 0)
}
\arguments{
\item{marketData}{data.frame of list of data.frame, marketData is used to calculate weights}
}
\value{
vector of weights of each stock in a portfolio
}
\description{
The Sharp ration strategy is based on Sharp Ration General-purpose optimization algorithm. All weights are assigned with accordance to Sharp Ration algorithm output.
}
