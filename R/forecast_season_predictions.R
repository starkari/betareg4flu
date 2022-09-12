#' Title
#'
#' @param data: data frame of data that has:
#' - a column of county names called 'Adjusted_County' in our data set
#' - a column denoting the season called 'Season' in our data set
#' - a column of influenza emergency department visits called 'ili_rate' in our data set
#' - a date column called 'last_day_week' in our data set
#' - a column denoting the week in the calendar year called 'Week' in our data set
#' @param county: string of the county you want to forecast for
#' @param training_seasons: character string of the seasons to be included
#' in training set
#' @param parameter_data: data frame of the parameters $S_v$, $S_{\phi}$ and
#' $p$ for the counties
#' @param threshold_data: data frame that gives the 40th, 67.5th and 95th threshold
#'
#' @return a list of 3 containing the forecasts(called results), runtime, and
#' quantile_matrix
#' results: filled in data_set_results data frame from create_results_data_frames
#' runtime: specification of how long this function took to run
#' quantile_matrix: filled in quantile_matrix data frame from create_results_data_frames
#' @export
#'
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' forecast <- forecast_season_predictions_loops(
#' data = seasons_data_weekly_update,
#' county = "MA_Middlesex",
#' training_seasons = c("2015-2016" "2016-2017" "2017-2018"),
#' parameter_data =  param_by_AIC_by_county)
#' }
#'
#'
#'


forecast_season_predictions <- function(data, county,
                                              training_seasons,
                                              parameter_data,
                                              threshold_data, num_sims=1000,
                                              dates_to_forecast="all"){

  ptm <- proc.time()

  # gets index of county of interest in parameter_data
  i=match(county,parameter_data$county)

  S_mean <- as.numeric(parameter_data[i,"S_v"])
  S_Precision <- as.numeric(parameter_data[i,"S_phi"])
  lags <- as.numeric(parameter_data[i,"lag"])

  # keep copy of original data to modify later
  data_copy <- data %>%
    dplyr::filter(Adjusted_County == parameter_data$county[i])

  data <- reformat_data(data = data,
                        county_name = parameter_data$county[i],
                        training_seasons, lags)

  prediction_time_inds <- which(data$train == FALSE)


  results_row_ind <- 1L


  max_S <- max(S_mean, S_Precision)
  covar <- c(paste0(c(rep("sin_InPeriod",max_S), rep("cos_InPeriod",max_S)), 1 : max_S))


  test_data <- data %>%
    dplyr::filter(train == FALSE)

  if (typeof(dates_to_forecast)=="character") {
    if (dates_to_forecast=="all") {
      dates_to_forecast <- test_data %>%
        pull(last_day_week)
    }
    else {
      dates_to_forecast <- as.Date(dates_to_forecast)
    }
  }


  simulation_data <- vector(mode="list", length = (nrow(test_data)))

  for (time_index in 1:(nrow(test_data))) {


    prediction_time_ind <- prediction_time_inds[time_index]

    n_horizons <- max(get_prediction_horizons(prediction_time_ind,data))

    # only make predictions for dates want to forecast
    if(test_data[time_index,last_day_week] %in% dates_to_forecast) {
      # only make predictions for dates in season_week 11 to 43 (calendar week 40 to 20)
      if (test_data[time_index,season_week] %in% c(11:43)) {
        forecast_data <- forecast_k_steps(k=n_horizons, num_sims, test_data, raw_data=data_copy,
                                          time_index, prediction_time_ind,
                                          S_mean, S_Precision, lags, county, training_seasons,
                                          covar)

        simulation_data_time_index <- list("Date"=as.character(data$last_day_week[prediction_time_ind]),
                                           "Simulation"=as.data.frame(forecast_data),
                                           "Beta_regression_results"=forecast_data$Beta_regression_results)
        simulation_data[[time_index]]<- simulation_data_time_index



      }
    }

    results_row_ind <- results_row_ind+n_horizons

  }


  simulation_data <- purrr::compact(simulation_data)

  run_time <- proc.time() - ptm
  BetaRegression <- list(simulation_data = simulation_data)

  return(BetaRegression)


}

