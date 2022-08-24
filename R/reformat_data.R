
#' Title
#'
#' @param data data frame of data that has:
#' - a column of county names called 'Adjusted_County' in our data set
#' - a column denoting the season called 'Season' in our data set
#' - a column of influenza emergency department visits called 'ili_rate' in our data set
#' - a date column called 'last_day_week' in our data set
#' - a column denoting the week in the calendar year called 'Week' in our data set
#' @param county_name the name of the county being forecasted
#' @param training_seasons the list of seasons to train on
#' @param lags the number of lags
#'
#' @return A filtered data frame only consisting of "county_name"'s data with additional
#' columns to the one passed in.
#' These consist of:
#' - "train": an indicator of if the data for that row is in the training seasons
#' - "new_ili": a shifted value (increase of 1e3) of the reported "ili_rate"
#' - "week_number": the number of weeks in a season
#' - "season_week": integer corresponding to what week that date is within the given season
#' - columns corresponding to the sine and cosine terms
#' of j*2*pi*omega where j=1,...,5 and omega is the corresponding week in the
#' season divided by total weeks. These columns are denoted sine_InPeriodj and cos_InPeriodj
#' - columns of the logit of the lagged values denoted "pk" for k=1,...,"lags".
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- reformat_data(data = seasons_data_weekly_update,
#' county_name = "MA_Middlesex",
#' training_seasons = c("2015-2016" "2016-2017" "2017-2018"),
#' lags=1)
#' }
#'

reformat_data <- function (data, county_name,
                               training_seasons,
                               lags){

  data <- data.table::data.table(data %>%
                       filter(Adjusted_County==county_name))


  # this is now training on first 3 and testing on 2018-2019
  data$train <- ifelse(data$Season %in% training_seasons,TRUE,FALSE)


  data$new_ili <- data$ili_rate+1e-3


  data[, week_number <- max(Week), by = Season]

  ## Season week column: week number within season
  data$season_week <- ifelse(data$Week>=30,data$Week-29,data$Week+(data$week_number-29))

  data[, InPeriod <- season_week/week_number, by = Year]

  data[, sin_InPeriod1 <- sin(2 * pi * InPeriod)]
  data[, cos_InPeriod1 <- cos(2 * pi * InPeriod)]
  data[, sin_InPeriod2 <- sin(4 * pi * InPeriod)]
  data[, cos_InPeriod2 <- cos(4 * pi * InPeriod)]
  data[, sin_InPeriod3 <- sin(6 * pi * InPeriod)]
  data[, cos_InPeriod3 <- cos(6 * pi * InPeriod)]
  data[, cos_InPeriod4 <- cos(8 * pi * InPeriod)]
  data[, sin_InPeriod4 <- sin(8 * pi * InPeriod)]
  data[, cos_InPeriod5 <- cos(10 * pi * InPeriod)]
  data[, sin_InPeriod5 <- cos(10 * pi * InPeriod)]



  for (i in 1 : lags){
    data[, paste0("p", i) <- logit_FUN(shift(new_ili,n = i, type = "lag"))]
  }

  return (data)


}
