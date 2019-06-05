#' End of day US Stock Prices
#'
#' Retrieves data from the Quandl Dataset.
#' @param code string or list of strings, code a.k.a ticker symbol on Quandle.
#' @param start_date A Date object, format="YYYY-MM-DD". Retrieve data rows on and after the specified start date.
#' @param end_date A Date object, format="YYYY-MM-DD". Retrieve data rows up to and including the specified end date.
#' @param collapse Change the sampling frequency of the returned data. Default is none; i.e., data is returned in its original granularity.
#' @param api_key Authentication toket for a Quandl user.
#' @param skipNA If FALSE, STOP is a code for a predefined date range is not found, otherwise WARNIGN. Default=FALSE
#' @return list(data=dataframe of returns per stock, missingCode=vector of codes that do not satisfy input params)
#' @examples \dontrun{
#' code <- c("AAPL", "FB", "GOOG", "C", "A", "INTC", "T", "WMT", "TXN")
#' start_date <- "2017-12-31"
#' end_date <- "2018-12-31"
#' collapse <- "daily"
#' api_key <- "User123"
#' market_date <- get.marketDate (code=code, start_date=start_date, end_date=end_date, collapse=collapse, api_key=api_key)
#' }
#' @note Due to Quandle database specification, all ".", " " symbols are represented as "_".
#' @references This R package uses the Quandl API. For more information go to \url{https://www.quandl.com/docs/api}. For more help on the package itself go to \url{https://www.quandl.com/help/r}.
#' @export
get.marketData <- function(code, start_date, end_date,
                     collapse=c("daily", "weekly", "monthly", "quarterly", "annual"),
                     api_key, skipNullStocks = FALSE, skipNA = FALSE) {

  # check if code is correct
  if (!all(gsub("[^A-Z0-9_.]", "", code) == code)) {
    stop("Codes are comprised of capital letters, numbers and underscores only.")
  }


  # convert freq to interger. Used in skipNA=TRUE to validate if data.frame is of complete form.
  frequency2integer <- function(freq) {

    if (is.null(freq) || is.na(freq)) {
      return(252)
    } else {
      switch(freq,
             "daily"    = 252,
             "weekly"   = 52,
             "monthly"  = 12,
             "quarterly" = 4,
             "yearly"   = 1,
             1)
    }
  }


  # setup params for GET request
  params <- list()
  params$collapse <- match.arg(collapse)
  params$start_date <- as.Date(start_date, format = "%Y-%m-%d")
  params$end_date <- as.Date(end_date, format = "%Y-%m-%d")
  params$api_key <- api_key
  params$column_index <- 11
  params$transform <- "rdiff"

  missingCodes <- c()
  tempRes <- NULL

  for (c in code) {

    df <- get.dataset(c, params, skipNullStocks)

    # check if a dataset has been found
    if (is.null(df)) {
      missingCodes <- append(missingCodes, c)
      next
    }

    # // TODO
    # print(length(df[, 2]))
    # print(as.integer(difftime(end_date, start_date,  unit="weeks")))
    if (!(length(df[, 2] & skipNA == TRUE) == as.integer(difftime(end_date, start_date,  unit="weeks")))) {
      missingCodes <- append(missingCodes, c)
      next
    }
    # skipNA=TRUE, check if frequency of dataset returned is correct
    # if (!(length(df[, 2] & skipNA == TRUE) == frequency2integer(params$collapse))) {
    #   missingCodes <- append(missingCodes, c)
    #   next
    # }

    # merge data.frames
    if (is.null(tempRes)) {
      tempRes <- df
    } else {
      tempRes <- merge(tempRes, df, by = "date", all = TRUE)
    }

  }

  return(list("data"=tempRes, "missingCodes"=missingCodes))

}

get.dataset <- function(code, params, skipNullStocks) {

  # writeLines(sprintf("Calling %s data...", code))
  json <- api.call(url_base=sprintf("https://www.quandl.com/api/v3/datasets/WIKI/%s/data.json?", code),
                   params=params)$dataset

  if (length(json$data) == 0) {
    if (skipNullStocks == FALSE) {
      stop(sprintf("Data for %s between %s and %s (%s) does not exists.",
                   code, params$start_date, params$end_date, params$collapse))
    } else {
      writeLines(sprintf("Data for %s between %s and %s (%s) does not exists.",
                 code, params$start_date, params$end_date, params$collapse))
      return(NULL)
    }

  }

  fin_data <- data.frame(json$data, stringsAsFactors = FALSE)
  names(fin_data) <- c("date", code)

  fin_data[, 1] <- as.Date(fin_data[, 1])
  fin_data[, 2] <- as.numeric(fin_data[, 2])

  return(fin_data)

}

api.call <- function(url_base, params) {

  response <- httr::GET(url_base, query=params)

  if (!(httr::status_code(response) >= 200 && httr::status_code(response) < 300)) {
    stop(httr::content(response, as = "text"), call. = FALSE)
  }

  text_response <- httr::content(response, as = "text")

  json_response <- tryCatch(jsonlite::fromJSON(text_response, simplifyVector = TRUE), error = function(e) {
    stop(e, " Failed to parse response: ", text_response)
  })

  return(json_response)
}


#' Random end of day US Stock prices
#'
#' Retrieves N number of random stocks from the Quandl Dataset.
#' @param nassets An integer, number of stock in a portfolio
#' @param start_date A Date object, format="YYYY-MM-DD". Retrieve data rows on and after the specified start date.
#' @param end_date A Date object, format="YYYY-MM-DD". Retrieve data rows up to and including the specified end date.
#' @param collapse Change the sampling frequency of the returned data. Default is none; i.e., data is returned in its original granularity.
#' @param api_key A string, authentication toket for a Quandl user.
#' @param excludedCodes A string or list of strings, stock codes that should be excluded from random sampling.
#' @param skipNA Boolean, if skipNA=TRUE, an assert that has NA in its date range is excluded.
#' @param recursive Boolean, if recursive=TRUE, a function will execute until exact predefined number of stocks is found.
#' @return list(data=dataframe of stocks in portfolio, missingCode=vector of codes that do not satisfy input params)
#' @note If recursive=TRUE & skipNA=TRUE, a function will search for N asserts that satisfy start_date, end_date and collapse arguments. Therefore, eventually, it may go out of requests allowed by Quandle.
#' @references Also see get.marketData function for additional information.
#' @export
get.marketData.random <- function(nassets, start_date, end_date,
                                  collapse=c("daily", "weekly", "monthly", "quarterly", "annual"),
                                  api_key, excludedCodes = NULL, skipNA = TRUE, recursive=FALSE) {

  # Function needed for recursive option
  recursive.helper <- function(df, codes.random, start_date, end_date, collapse, api_key) {


    rec.data.random <- get.marketData(code = codes.random, start_date = start_date, end_date = end_date,
                                      collapse = collapse, api_key = api_key, skipNA = skipNA, skipNullStocks = TRUE)

    if (!(is.null(rec.data.random$data))) {
      df <- merge(df, rec.data.random$data, by="date", all = TRUE)
    }

    return(list("data"=df, "missingCodes"=rec.data.random$missingCodes))
  }

  # Remove codes that should not be included in sample
  remove.codes.subset <- function(codes, codes2remove) {
    return(subset(codes, !(codes %in% codes2remove)))
  }

  if (nassets > 299) {
    stop("Max allowed number of requests is 299 per 10 seconds.")
  }

  codes <- read.table("metadata/tickers", header = TRUE, stringsAsFactors = FALSE)$code
  codes <- remove.codes.subset(codes, excludedCodes)
  codes.random <- sample(codes, nassets)

  data.random <- get.marketData(code = codes.random, start_date = start_date, end_date = end_date,
                                collapse = collapse, api_key = api_key, skipNA = skipNA, skipNullStocks=TRUE)

  # If none of stocks has been found - run the function again.
  if (is.null(data.random$data)) {
    get.marketData.random(nassets, start_date, end_date, collapse, api_key, excludedCodes, recursive)
  }

  if (is.null(data.random$missingCodes)) {
    return(data.random)
  }

  if (recursive == TRUE) {

    codes <- remove.codes.subset(codes, data.random$missingCodes)
    codes.random <- sample(codes, length(data.random$missingCodes))
    data.random$missingCodes <- NULL

    while (TRUE) {

      data.random <- recursive.helper(data.random$data, code = codes.random, start_date = start_date,
                                      end_date = end_date, collapse = collapse, api_key = api_key)

      if (!is.null(data.random$missingCodes)) {

        codes <- remove.codes.subset(codes, data.random$missingCodes)
        codes.random <- sample(codes, length(data.random$missingCodes))

      } else {
        return(data.random)
      }

    }

  }

}

#' Random set of portfolios of end of day US Stock prices
#'
#' Generate N numberb of randomly sampled, unique portfolios
#' @param nportfolios An integer, number of portfolios to be generated
#' @param ... See get.marketData.random
#' @export
get.portfolios.random <- function(nportfolios, nassets, ...) {

  # //TODO remvoe


  start_date <- "2010-12-31"
  end_date <- "2012-12-30"
  collapse <- "weekly"

  portfolios <- list()

  for (portf in 1:nportfolios) {
    df <- get.marketData.random(nassets, start_date = start_date, end_date = end_date, collapse = collapse, token,
                                recursive = TRUE)$data

    # if a portfolio already exists - run again random market data
    if (list(names(df)) %in% lapply(portfolios, names)) {
      portf <- portf - 1
      next
    }
    row.names(df) <- df$date
    df$date <- NULL
    portfolios[[portf]] <- df
  }

  return(portfolios)

}



