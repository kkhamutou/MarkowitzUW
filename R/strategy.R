library(ggplot2)
library(reshape2)
library(scales)

source("./R/marketDataAPI.R")
source("./R/maxSharpRation.R")



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

  sharp <- list()
  rational <- list()

  for (i in 1:length(marketData.training)) {
    sharp[[i]] <- data.frame(mapply("*", marketData.test[[i]], weights.sharp[[i]]))
    sharp[[i]]$date <- as.Date(row.names(marketData.test[[i]]))

    rational[[i]] <- data.frame(mapply("*", marketData.test[[i]], weights.rational[[i]]))
    rational[[i]]$date <- as.Date(row.names(marketData.test[[i]]))
  }

  return(list(training=marketData.training, test=marketData.test, sharp=sharp, rational=rational))
}


#' @export
strategy.sharp <- function(marketData, riskFreeRate=0) {

  sharp.ration <- lapply(marketData, sharpRation.optimization.GP, riskFreeRate)

  return(sharp.ration)
}


strategy.rational <- function(marketData) {

  weights <- list()
  res <- list()

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


#
#
#res <- strategy.sharp(3, 5, start_date = start_date, end_date = end_date, collapse = collapse, api_key = token)

#md <- build.test.case(3, 5, start_date = start_date, end_date = end_date, collapse = collapse, api_key = token)

# x <- melt(res$res[[1]], id='date', value.name = "Weighted rate of returns")
#
# x1 <- ggplot(x,aes(x=date,y=`Weighted rate of returns`,colour=variable,group=variable)) +
#   geom_line() + scale_x_date(date_breaks = "months" , date_labels = "%b-%y")




