% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maxSharpRation.R
\name{sharpRation.optimization.GP}
\alias{sharpRation.optimization.GP}
\title{Sharp Ration General-purpose optimization}
\usage{
sharpRation.optimization.GP(returns, riskFreeRate = 0)
}
\arguments{
\item{returns}{A matrix or data.frame. Returns on stocks}

\item{riskFreeRate}{An integer, the risk-free rate of return is the theoretical rate of return of an investment with zero risk.}
}
\description{
The Sharpe ratio (also known as the Sharpe index, the Sharpe measure, and the reward-to-variability ratio) is a way to examine the performance of an investment by adjusting for its risk.
The ratio measures the excess return (or risk premium) per unit of deviation in an investment asset or a trading strategy, typically referred to as risk, named after William F. Sharpe.
Sharp ration function is optimized by general-purpose optimization based on Nelder–Mead, quasi-Newton and conjugate-gradient algorithms. It includes an option for box-constrained optimization and simulated annealing.
}
