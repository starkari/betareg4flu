

#' Title
#'
#' @param k: integer of the number of steps to forecast
#' @param num_sims: the number of forecasted simulations
#' @param test_data: only the data that is being tested and not trained
#' @param raw_data: the raw data that has the necessary columns
#' to run through `reformat_data`
#' @param time_index: the time index (row number in test_data)
#' @param prediction_time_ind: row index in entire data frame corresponding to the time index
#' @param S_mean: the number of harmonics for the mean term
#' @param S_Precision: the number of harmonics for the precision term
#' @param lags: the number of lags
#' @param county: the county name
#' @param covar: the sine and cosine covariates
#'
#' @return
#' @export
#'
#' @examples
forecast_k_steps <- function(k, num_sims, test_data, raw_data, time_index,
                             prediction_time_ind,S_mean, S_Precision, lags,
                             county, training_seasons, covar) {


  # data we currently have
  current_data <- matrix(rep(test_data[1:time_index,c(new_ili)],num_sims),
                         nrow = num_sims, byrow = TRUE)

  colnames(current_data) <- paste0("y_",1:time_index)

  forecast_data <- matrix(NA, ncol = k, nrow=num_sims)
  colnames(forecast_data) <- paste0("y_",time_index,"+",1:k)

  # gets all the data that is assigned as TRAIN
  data_fit_beta_1 <- reformat_data(data = raw_data,
                                   county_name = county,
                                   training_seasons, lags) %>%
    dplyr::filter(train==TRUE)

  # gets all the prior data and filters to be data to TEST on ie
  # getting the new data in the test season
  data_fit_beta_2 <- reformat_data(data = raw_data,
                                   county_name = county,
                                   training_seasons, lags)[
                                     seq_len(max(0, prediction_time_ind-1)), ] %>%
    dplyr::filter(train==FALSE)

  # combines two sets to form data to fit model on
  data_fit_beta <- rbind(data_fit_beta_1,data_fit_beta_2)

  Update_BetaReg <- fit_beta_model(S_mean, S_Precision, lags,
                                   data=data_fit_beta)


  for (sim in 1:num_sims) {

    # copy of data that gets updated and reformmated with each new horizon
    data_this_sim <- raw_data


    for (prediction_horizon in 1:k) {

      # does all the lag formatting with the potentially untrue data
      data_this_sim_this_horizon <- reformat_data(data = data_this_sim,
                                                  county_name = county,
                                                  training_seasons, lags)

      new_data <- data_this_sim_this_horizon[
        seq_len(max(0, prediction_time_ind+prediction_horizon-1)), ]


      # gets the p lagged values before the time being predicted
      p_lagged_names <- paste0("p",rep(1:lags))
      p_lagged_prior <- data_this_sim_this_horizon[prediction_time_ind+prediction_horizon,
                                                   ..p_lagged_names] %>%
        as.numeric() %>%
        plogis()

      pr <- forecast_1_step(object = Update_BetaReg,
                            p = lags,
                            start_value = p_lagged_prior,
                            # gets the sine and cosine terms
                            coefs_subset = data_this_sim_this_horizon[(prediction_time_ind+prediction_horizon),
                                                                      ),
                                                                      ..covar])

      #updates the data we are using to forecast with the new result
      data_this_sim[prediction_time_ind+prediction_horizon,"ili_rate"] <-
        pr$pt_sample

      #storing the data
      forecast_data[sim,prediction_horizon] <- pr$pt_sample

    }

  }

  return(list("forecast_dat"=forecast_data,
              "Beta_regression_results"=Update_BetaReg))




}
