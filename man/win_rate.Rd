% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{win_rate}
\alias{win_rate}
\title{Compute win rate}
\usage{
win_rate(strategy.signal, trade.value, type = "buy", weight.s = 0,
  stockp.begin = 0)
}
\arguments{
\item{strategy.signal}{trade signal components of 0,-1,1 which stardends
'hold','sell','buy'.}

\item{trade.value}{Vector of a numeric series which represents the total
value generated by a trade strategy.}

\item{type}{Must be one of 'buy' or 'sell'.}

\item{weight.s}{Stocks holded at begining in the short position.}

\item{stockp.begin}{the first one of the correspond stock price}
}
\description{
Compute win rate
}
