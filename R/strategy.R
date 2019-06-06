library(ggplot2)
library(reshape2)
library(scales)

source("./R/marketDataAPI.R")
source("./R/maxSharpRation.R")

#' Comparison of 3 strategies
#'
#' A function is used to compare 3 different stretagies: sharp ration, rational invetor and random investor based on training dataset.Then, it applies derived weights to test dataset to see the performance of those strategies.
#' @param nportfolios An integer, number of portfolios to be generated
#' @param nassets An integer, number of stock in a portfolio
#' @param start_date A Date object, format="YYYY-MM-DD". Retrieve data rows on and after the specified start date.
#' @param end_date A Date object, format="YYYY-MM-DD". Retrieve data rows up to and including the specified end date.
#' @param collapse Change the sampling frequency of the returned data. Default is none; i.e., data is returned in its original granularity.
#' @param api_key A string, authentication toket for a Quandl user.
#' @return list, training=market training dataset, test=market test dataset, sharp=performance of sharp ratio on test dataset, rational=performance of rational investor on test dataset, random=performance of random investor on test dataset.
#' @export
build.test.case <- function(nportfolios, nassets, start_date, end_date, riskFreeRate = 0,
                            collapse=c("daily", "weekly", "monthly", "quarterly", "annual"), api_key) {

  split.by.half <- function(x) {

    n <- as.integer(nrow(x) / 2)
    return(list(training=x[1:n, ], test=x[n:nrow(x), ]))

  }

  marketData <- get.portfolios.random(nportfolios = nportfolios, nassets = nassets, start_date = start_date,
                                      end_date = end_date, collapse = collapse, api_key = api_key)

  marketData.split <- lapply(marketData, split.by.half)

  rm(marketData)

  marketData.training <- lapply(marketData.split, '[[', 'training')
  marketData.test <- lapply(marketData.split, '[[', 'test')

  weights.sharp <- strategy.sharp(marketData.training, riskFreeRate)
  weights.rational <- strategy.rational(marketData.training)
  weights.random <- strategy.random(marketData.training)

  sharp <- list()
  rational <- list()
  rand <- list()

  for (i in 1:length(marketData.training)) {
    d <- as.Date(row.names(marketData.test[[i]]))

    sharp[[i]] <- data.frame("Sharp" = rowSums(mapply("*", marketData.test[[i]], weights.sharp[[i]])),
                             "Date" = d)

    rational[[i]] <- data.frame("Rational" = rowSums(mapply("*", marketData.test[[i]], weights.rational[[i]])),
                                "Date" = d)

    rand[[i]] <- data.frame("Random" = rowSums(mapply("*", marketData.test[[i]], weights.random[[i]])),
                            "Date" = d)
  }

  return(list(training=marketData.training, test=marketData.test, sharp=sharp, rational=rational, random=rand))
}


#' Sharp ration strategy
#'
#' The Sharp ration strategy is based on Sharp Ration General-purpose optimization algorithm. All weights are assigned with accordance to Sharp Ration algorithm output.
#' @param marketData data.frame of list of data.frame, marketData is used to calculate weights
#' @return vector of weights of each stock in a portfolio
#' @export
strategy.sharp <- function(marketData, riskFreeRate=0) {

  sharp.ration <- lapply(marketData, sharpRation.optimization.GP, riskFreeRate)

  return(sharp.ration)
}

#' Rational investor strategy
#'
#' The rational investor strategy is based on the following model: a stock with highest average rate of returns receives 50% wegith, second highest is set to 25% weight, the rest receives equal share of remaning 25%. Note, than onle possitive rate of returns is concerned.
#' @param marketData data.frame of list of data.frame, marketData is used to calculate weights
#' @return vector of weights of each stock in a portfolio
#' @export
strategy.rational <- function(marketData) {

  weights <- list()

  for (portf in 1:length(marketData)) {

    ror.mean <- mapply(mean, marketData[[portf]])
    ror.mean <- ror.mean[order(ror.mean, decreasing = TRUE)]

    n <- length(marketData[[portf]])
    ror.weight <- rep(0, n)
    names(ror.weight) <- names(marketData[[portf]])

    if (ror.mean[1] > 0) {
      ror.weight[names(ror.mean[1])] <- 0.5
    } else {
      weights[[portf]] <- ror.weight
      next
    }

    if (ror.mean[2] > 0 ) {
      ror.weight[names(ror.mean[2])] <- 0.25
    } else {
      ror.weight[names(ror.mean[1])] <- 1
      weights[[portf]] <- ror.weight
      next
    }

    if (ror.mean[3] > 0) {
      temp <- ror.mean[3:length(ror.mean)]
      temp <- temp[which(temp > 0)]
      ror.weight[names(temp)] <- 0.25/length(temp)
    } else {
      ror.weight[names(ror.mean[2])] <- 0.5
      weights[[portf]] <- ror.weight
      next
    }

    weights[[portf]] <- ror.weight

  }

  return(weights)
}

#' Random investor strategy
#'
#' The random investor strategy is based on random investor behaviour. All weights are assigned randomly
#' @param marketData data.frame of list of data.frame, marketData is used to calculate weights
#' @return vector of weights of each stock in a portfolio
#' @export
strategy.random <- function(marketData) {

  weights <- list()

  for (portf in 1:length(marketData)) {
    n <- length(marketData[[portf]])
    ror.weight <- runif(n)
    names(ror.weight) <- names(marketData[[portf]])
    weights[[portf]] <- ror.weight / sum(ror.weight)
  }

  return(weights)
}

#' Plot the output of 3 strategies
#'
#' A function is used to plot 3 different stretagies: sharp ration, rational invetor and random investor based on training dataset.
#' @param df build.test.case output
#' @return list of function to plot.
#' @note In order to plot a portfolio performance, you need to select a particular portfolio from a list and run it.
#' @export
plot.strategies <- function(df) {

  list.to.plot <- list()

  for (i in 1:length(df$sharp)) {

    temp <- data.frame("Sharp" = df$sharp[[i]]$Sharp,
                       "Rational" = df$rational[[i]]$Rational,
                       "Random" = df$random[[i]]$Random,
                       "Date" = df$sharp[[i]]$Date)
    mltdf <- melt(temp, id='Date', value.name = "Weighted rate of returns")
    list.to.plot[[i]] <- ggplot(mltdf, aes(x=Date, y=`Weighted rate of returns`, colour=variable, group=variable)) +
                         geom_line() + scale_x_date(date_breaks = "months" , date_labels = "%b-%y")

  }

  return(list.to.plot)

}






