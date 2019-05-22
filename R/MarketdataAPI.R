#' End of day US Stock Proces
#'
#' Retrieves Data from the Quandl Dataset endpoint and formats
#' @param code string or list of strings: Code a.k.a ticker symbol on Quandle.
#' @param start_date A Date object, format="YYYY-MM-DD". Retrieve data rows on and after the specified start date.
#' @param end_date A Date object, format="YYYY-MM-DD". Retrieve data rows up to and including the specified end date.
#' @param collapse Change the sampling frequency of the returned data. Default is none; i.e., data is returned in its original granularity.
#' @param api_key Authentication toket for a Quandl user.
#' @return dataframe of returns per stock
#' @examples \dontrun{
#' code <- c("AAPL", "FB", "GOOG", "C", "A", "INTC", "T", "WMT", "TXN")
#' start_date <- "2017-12-31"
#' end_date <- "2018-12-31"
#' collapse <- "daily"
#' api_key <- "User123"s
#' market_date <- get.marketDate (code=code, start_date=start_date, end_date=end_date, collapse=collapse, api_key=api_key)
#' }
#' @note Due to Quandle database specification, all ".", " " symbols are represented as "_".
#' @references This R package uses the Quandl API. For more information go to \url{https://www.quandl.com/docs/api}. For more help on the package itself go to \url{https://www.quandl.com/help/r}.
#' @export
get.marketData <- function(code, start_date, end_date,
                     collapse=c("daily", "weekly", "monthly", "quarterly", "annual"),
                     api_key) {

  # check if code is correct
  if (!all(gsub("[^A-Z0-9_.]", "", code) == code)) {
    stop("Codes are comprised of capital letters, numbers and underscores only.")
  }


  params <- list()
  params$collapse <- match.arg(collapse)
  params$start_date <- as.Date(start_date, format = "%Y-%m-%d")
  params$end_date <- as.Date(end_date, format = "%Y-%m-%d")
  params$end_date <- end_date
  params$api_key <- api_key
  params$column_index <- 11
  params$transform <- "rdiff"

  for (c in code) {

    df <- get.dataset(c, params)
    if (!exists("res")) {
      res <- df
    } else {
      res <- merge(res, df, by = 1, all = TRUE)
    }

  }

  row.names(res) <- res$date
  res$date <- NULL
  return(res)

}

get.dataset <- function(code, params) {

  writeLines(sprintf("Calling %s data...", code))
  json <- api.call(url_base=sprintf("https://www.quandl.com/api/v3/datasets/WIKI/%s/data.json?", code),
                   params=params)$dataset

  if (length(json$data) == 0) {
    stop(sprintf("Data for %s between %s and %s (%s) does not exists.",
                 code, params$start_date, params$end_date, params$collapse))
  }

  data <- data.frame(json$data, stringsAsFactors = FALSE)
  names(data) <- c("date", code)

  data[, 1] <- as.Date(data[, 1])
  data[, 2] <- as.numeric(data[, 2])

  return(data)

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



get.marketData.random <- function(ncode, start_date, end_date,
                                  collapse=c("daily", "weekly", "monthly", "quarterly", "annual"),
                                  api_key) {

  if (ncode > 299) {
    stop("Max allowed number of requests is 299 per 10 seconds.")
  }
  codes <- read.table("metadata/tickers", header = TRUE, stringsAsFactors = FALSE)
  code.random <- sample(codes$code, ncode)

  data.random <- get.marketData(code.random, start_date = start_date, end_date = end_date,
                        collapse = collapse, api_key = api_key)
  return(data.random)

}


