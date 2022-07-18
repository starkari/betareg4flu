# helper function for getting prediction horizons
#' Title
#'
#' @param prediction_time_ind an indicator of the prediction time 
#' (row index in the data frame being passed in)
#' @param data a data frame containing season_week outputted from `reformat_data`
#'
#' @return a range of horizons to predict for
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' dat <- reformat_data(data = seasons_data_weekly_update,
#' county_name = "MA_Middlesex",
#' training_seasons = c("2015-2016" "2016-2017" "2017-2018"), 
#' lags=1)
#' 
#' prediction_horizons <- get_prediction_horizons(
#' prediction_time_ind = which(dat$train == FALSE)[15],
#' data = dat)
#' }
#' 
#' 


get_prediction_horizons <- function(prediction_time_ind,data){
  sw <- data$season_week[prediction_time_ind] #gets the season week from the data
  
  if (sw %in% 11:42){
    return (1:(43-sw))
  } else {
    return (1:1)
  }
}