library(optimx)

#' Sharp Ration General-purpose optimization
#'
#' The Sharpe ratio (also known as the Sharpe index, the Sharpe measure, and the reward-to-variability ratio) is a way to examine the performance of an investment by adjusting for its risk.
#' The ratio measures the excess return (or risk premium) per unit of deviation in an investment asset or a trading strategy, typically referred to as risk, named after William F. Sharpe.
#' Sharp ration function is optimized by general-purpose optimization based on Nelder–Mead, quasi-Newton and conjugate-gradient algorithms. It includes an option for box-constrained optimization and simulated annealing.
#' @param returns A matrix or data.frame. Returns on stocks
#' @param riskFreeRate An integer, the risk-free rate of return is the theoretical rate of return of an investment with zero risk.
#' @export
sharpRation.optimization.GP <- function(returns, riskFreeRate=0) {

  if (is.null(returns)) {
    stop("The input value cannot be null.")
  }
  numAssets <- length(returns)

  if (numAssets == 1) {
    return(1)
  }

  meanReturns <- lapply(returns, mean)
  covMatrix <- cov(returns, method = "pearson")

  opts <- optim(rep(1/numAssets, numAssets), portfolio.SharpRation, method = "L-BFGS-B",
                control=list(trace=1),
                meanReturn=meanReturns, covMatrix=covMatrix, riskFreeRate=riskFreeRate,
                lower=rep(0, numAssets), upper=rep(1, numAssets))

  res <- opts$par / sum(opts$par)
  names(res) <- names(returns)

  return(res)
}


portfolio.Return <- function(meanReturns, weights) {
  return(Reduce('+', Map('*', meanReturns, weights)))
}

portfolio.StdDev <- function(covMatrix, weights) {
  return(sqrt(t(weights) %*% (covMatrix %*% weights)))
}

portfolio.SharpRation <- function(weights, meanReturn, covMatrix, riskFreeRate) {

  portReturn <- portfolio.Return(meanReturn, weights)
  portStdDev <- portfolio.StdDev(covMatrix, weights)

  return (-(portReturn - riskFreeRate) / portStdDev)
}




