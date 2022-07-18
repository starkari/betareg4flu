



#' Title
#'
#' @param S_mean the number of harmonics for the mean term
#' @param S_Precision the number of harmonics for the precision term
#' @param lags the number of lags
#' @param data a data frame outputted from `reformat_data`
#'
#' @return the results of a try (either fitted model or an error) of the model
#' using the above parameters
#' @export
#'
#' @import betareg
#' @import data.table
#' @import lubridate
#' @import here
#'
#' @examples
#' \dontrun{
#' dat <- reformat_data(data = seasons_data_weekly_update,
#' county_name = "MA_Middlesex",
#' training_seasons = c("2015-2016", "2016-2017", "2017-2018"),
#' lags=1)
#' model <- fit_beta_model(S_mean=4, S_Precision=4, lags=1,
#' data=dat[train==T,])
#' }
#'
#'
#'




fit_beta_model <- function(S_mean, S_Precision, lags, data){

  full_model <- get_full_model(S_mean, S_Precision, lags,
                               model_name = "hhh4")

  res <- try(betareg(full_model,
                     data = data,
                     link = "logit", link.phi = "log"))
  return(res)
}
