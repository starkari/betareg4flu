
#' Summarize Forecasts
#'
#' @param forecast_list output from `forecast_season_predictions`$simulation_data
#' @param truth data frame containing truth needs columns for county, date, season, and ili_rate
#' @param county string of the county the forecast is made
#' @param threshold_data data frame of threshold cutoffs by county
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
summarize_forecasts_distribution <- function(forecast_list, truth, county, threshold_data){

  total_rows <- 0

  for (i in 1:length(forecast_list)){
    data_i <- forecast_list[[i]]$Simulation
    total_rows <- total_rows+ncol(data_i)+1
  }

  true_data <- truth %>%
    dplyr::filter(Adjusted_County==county)


  data_set_results <- as.data.frame(matrix(NA, ncol=12,
                                           nrow = total_rows))

  colnames(data_set_results) <- c("forecast_date", "location", "target","point","truth","abs_error",
                                  "low","moderate","high","vhigh","log_score","truth_threshold")

  results_row_ind <- 1

  for (i in 1:length(forecast_list)){
    data_i <- forecast_list[[i]]$Simulation
    forecast_date_i <- forecast_list[[i]]$Date
    fit_i <- forecast_list[[i]]$Beta_regression_results
    num_sims <- nrow(data_i)
    date_i_index <- which(true_data$last_day_week==(as.Date(forecast_date_i)))
    test_season_i <- true_data[date_i_index,]$Season

    seasons <- c("2015-2016","2016-2017","2017-2018","2018-2019")
    training_seasons_i <- seasons[ ! seasons %in% test_season_i]

    lags_i <- fit_i$coefficients$mean %>%
      t() %>%
      as.data.frame() %>%
      dplyr::select(dplyr::starts_with("p")) %>%
      ncol()

    reformatted_data_i <- betareg4flu::reformat_data(data = truth,
                                                    county_name = county,
                                                    training_seasons = training_seasons_i,
                                                    lags = lags_i) %>%
      dplyr::select(c("last_day_week",
                      dplyr::starts_with("sin_"),
                      dplyr::starts_with("cos_"),
                      dplyr::starts_with("p"))) %>%
      filter(last_day_week > forecast_date_i) %>%
      mutate("(Intercept)"=1)


    for (j in 1:ncol(data_i)){
      target_j <- paste0(j," wk ahead forecast")
      data_wk_j <- data_i[,j]
      pt_fcst <- mean(data_wk_j)

      # adds the forecast to the lags so when future j are passed in to function to get
      # distribution they use predicted values when lag date is after forecast_date and
      # truth before
      for(i in 1:lags_i){
        reformatted_data_i[(j+i),paste0("p",i)] <- qlogis(pt_fcst)
      }

      truth_index <- which(true_data$last_day_week==(as.Date(forecast_date_i)+7*j))
      truth_value <- true_data[truth_index,"ili_rate"]


      severity_category_horizon_j <- get_distribution_severity_categories(values = reformatted_data_i[j,],
                                                             county=county,
                                                             threshold_data = threshold_data,
                                                             beta_fit = fit_i)
      sev_low <- ((severity_category_horizon_j$low_prop))
      sev_mod <- ((severity_category_horizon_j$moderate_prop))
      sev_high <- ((severity_category_horizon_j$high_prop))
      sev_vhigh <- ((severity_category_horizon_j$very_high_prop))

      pred_thresholds <- c(sev_low, sev_mod, sev_high, sev_vhigh)

      log_score <- get_log_score(value = truth_value,
                                 county = county, threshold_data = threshold_data,
                                 pred_thresholds = pred_thresholds)

      data_set_results[results_row_ind,"forecast_date"] <- forecast_date_i
      data_set_results[results_row_ind,"location"] <- county
      data_set_results[results_row_ind,"target"] <- target_j
      data_set_results[results_row_ind,"point"] <- pt_fcst
      data_set_results[results_row_ind,"truth"] <- truth_value
      data_set_results[results_row_ind,"abs_error"] <- abs(truth_value-pt_fcst)
      data_set_results[results_row_ind,"low"] <- sev_low
      data_set_results[results_row_ind,"moderate"] <- sev_mod
      data_set_results[results_row_ind,"high"] <- sev_high
      data_set_results[results_row_ind,"vhigh"] <- sev_vhigh
      data_set_results[results_row_ind,"log_score"] <- log_score$score
      data_set_results[results_row_ind,"truth_threshold"] <- log_score$threshold


      results_row_ind <- results_row_ind+1

    }

    current_season <- as.character(true_data[which(true_data$last_day_week==(as.Date(forecast_date_i))),
                                             "Season"])

    true_peak_remaining <- true_data %>%
      dplyr::filter(Season==current_season) %>%
      dplyr::filter(last_day_week > as.Date(forecast_date_i)) %>%
      dplyr::pull(ili_rate) %>%
      max()

    max_per_simutation <- apply(data_i,1,max)
    severity_category_peak_remaining <- get_severity_categories(values = max_per_simutation,
                                                                county=county,
                                                                threshold_data = threshold_data)

    sev_peak_low <- ((severity_category_peak_remaining$low_count/num_sims))
    sev_peak_mod <- ((severity_category_peak_remaining$moderate_count/num_sims))
    sev_peak_high <- ((severity_category_peak_remaining$high_count/num_sims))
    sev_peak_vhigh <- ((severity_category_peak_remaining$very_high_count/num_sims))

    log_score_peak <- get_log_score(value = true_peak_remaining,
                                    county = county, threshold_data = threshold_data,
                                    pred_thresholds = c(sev_peak_low, sev_peak_mod,
                                                        sev_peak_high, sev_peak_vhigh))


    data_set_results[results_row_ind,"forecast_date"] <- forecast_date_i
    data_set_results[results_row_ind,"location"] <- county
    data_set_results[results_row_ind,"target"] <- "season peak remaining"
    data_set_results[results_row_ind,"truth"] <- true_peak_remaining

    data_set_results[results_row_ind,"low"] <- sev_peak_low
    data_set_results[results_row_ind,"moderate"] <- sev_peak_mod
    data_set_results[results_row_ind,"high"] <- sev_peak_high
    data_set_results[results_row_ind,"vhigh"] <- sev_peak_vhigh
    data_set_results[results_row_ind,"log_score"] <- log_score_peak$score
    data_set_results[results_row_ind,"truth_threshold"] <- log_score_peak$threshold


    results_row_ind <- results_row_ind+1

  }




  return(data_set_results)


}
