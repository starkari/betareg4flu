% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_full_model.R
\name{get_full_model}
\alias{get_full_model}
\title{Title}
\usage{
get_full_model(S_mean, S_Precision, lags, model_name = "hhh4")
}
\arguments{
\item{S_mean}{the number of harmonics for the mean term}

\item{S_Precision}{the number of harmonics for the precision term}

\item{lags}{the dumber of lags}

\item{model_name}{the name of the model being formatted, defaults to hhh4}
}
\value{
a string of the full model parameterization to be passed in to 
betareg
}
\description{
Title
}
\examples{

\dontrun{
full_model <- get_full_model(S_mean=4, S_Precision=4, 
lags=1, model_name = "hhh4")
}


}
