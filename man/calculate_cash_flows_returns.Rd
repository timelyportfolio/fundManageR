% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waterfall_functions.R
\name{calculate_cash_flows_returns}
\alias{calculate_cash_flows_returns}
\title{Calculate returns for a given set of cash flows}
\usage{
calculate_cash_flows_returns(dates = c("2016-09-01", "2017-08-31",
  "2018-08-31", "2019-08-31", "2020-08-31", "2021-08-31", "2022-08-31",
  "2023-08-31"), cash_flows = c(-4151601, 119499.036215643, 257186.036215643,
  447646.036215643, 200652.036215643, 510409.036215643, 193.036215643166,
  8788626.76409155), working_capital = 125000, remove_cumulative_cols = T,
  distribution_frequency = "annually", date_format = "\%Y-\%m-\%d",
  scale_to_100 = F, return_percentage = F, return_df = T,
  return_message = T)
}
\arguments{
\item{dates}{vector of dates}

\item{cash_flows}{Vector of cash flows}

\item{working_capital}{Amount of working capital, minimum cash}

\item{remove_cumulative_cols}{Remove summary columns}

\item{distribution_frequency}{When is the cash distributed}

\item{date_format}{Format of the date inputs}

\item{scale_to_100}{Scale numbers to 100}

\item{return_percentage}{Return percentages}

\item{return_df}{return data frame}

\item{return_message}{return message}

\item{include_final_day}{Include the final day in calculation}
}
\description{
Calculate returns for a given set of cash flows
}
\examples{
calculate_cash_flows_returns(dates = c( "2016-09-01", "2017-08-31", "2018-08-31", "2019-08-31", "2020-08-31", "2021-08-31", "2022-08-31", "2023-08-31" ),
cash_flows = c( -4151601, 119499.036215643, 257186.036215643, 447646.036215643, 200652.036215643, 510409.036215643, 193.036215643166, 8788626.7640915 ),
working_capital = 125000, remove_cumulative_cols = T, distribution_frequency = 'annually', date_format = '\%Y-\%m-\%d', scale_to_100 = F, return_percentage = F, return_df = T, return_message = T
)
}

