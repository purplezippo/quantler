% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{return_rate}
\alias{return_rate}
\title{Return rate}
\usage{
return_rate(stock.price, type = "raw", days = NA)
}
\arguments{
\item{stock.price}{Vector of a stock price series, time level could be 1day,
1hour or 1min.}

\item{type}{Value of type muste be one of 'raw','day' and 'year'. When type
equals to 'raw' then calculate the total return rate, when type equals to
'day' then calculate daily return rate, when type equals to year then
calculate annual return rate.}

\item{days}{Time span of day of the input stock data. If type is set to
'day' or 'year', this value must given.}
}
\description{
Return rate
}
