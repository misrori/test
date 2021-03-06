% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_one_stock_ticker.R
\name{stock_get_one_ticker}
\alias{stock_get_one_ticker}
\title{Downloading and adding technical indicators to the historical data of a ticker}
\usage{
stock_get_one_ticker(ticker, start_date = "2000-01-01",
  end_date = Sys.Date(), mas = c(50, 100, 200))
}
\arguments{
\item{ticker}{The ticker of the company to download}

\item{start_date}{The first date of the historical data}

\item{end_date}{The last date of the data}

\item{mas}{List of the simple moving averages to calculate}
}
\description{
Downloading and adding technical indicators to the historical data of a ticker
}
