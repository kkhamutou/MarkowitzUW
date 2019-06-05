library(optimx)


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




