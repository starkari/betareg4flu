% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_forecasts.R
\name{summarize_forecasts}
\alias{summarize_forecasts}
\title{Summarize Forecasts}
\usage{
summarize_forecasts(forecast_list, truth, county, threshold_data)
}
\arguments{
\item{forecast_list}{output from `forecast_season_predictions`$simulation_data}

\item{truth}{data frame containing truth needs columns for county, date, season, and ili_rate}

\item{county}{string of the county the forecast is made}

\item{threshold_data}{data frame of threshold cutoffs by county}
}
\description{
Summarize Forecasts
}
