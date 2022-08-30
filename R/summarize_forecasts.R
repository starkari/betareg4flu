
#' Summarize Forecasts
#'
#' @param forecast_list output from `forecast_season_predictions`
#' @param truth data frame containing truth needs columns for county, date, season, and ili_rate
#' @param county string of the county the forecast is made
#' @param threshold_data data frame of threshold cutoffs by county
#'
#' @return
#' @export
#'
#' @examples
summarize_forecasts <- function(forecast_list, truth, county, threshold_data){

  total_rows <- 0

  for (i in 1:length(forecast_list$simulation_data)){
    data_i <- forecast_list$simulation_data[[i]]$Simulation
    total_rows <- total_rows+ncol(data_i)+1
  }

  true_data <- truth %>%
    filter(Adjusted_County==county)

  data_set_results <- as.data.frame(matrix(NA, ncol=12,
                                           nrow = total_rows))

  colnames(data_set_results) <- c("forecast_date", "location", "target","point","truth","abs_error",
                                  "low","moderate","high","vhigh","log_score","truth_threshold")

  results_row_ind <- 1

  for (i in 1:length(forecast_list$simulation_data)){
    data_i <- forecast_list$simulation_data[[i]]$Simulation
    forecast_date_i <- forecast_list$simulation_data[[i]]$Date
    num_sims <- nrow(data_i)


    for (j in 1:ncol(data_i)){
      target_j <- paste0(j," wk ahead forecast")
      data_wk_j <- data_i[,j]
      pt_fcst <- mean(data_wk_j)

      truth_index <- which(true_data$last_day_week==(as.Date(forecast_date_i)+7*j))
      truth_value <- true_data[truth_index,"ili_rate"]

      severity_category_horizon_j <- get_severity_categories(values = data_wk_j,
                                                             county=county,
                                                             threshold_data = threshold_data)
      sev_low <- ((severity_category_horizon_j$low_count/num_sims))
      sev_mod <- ((severity_category_horizon_j$moderate_count/num_sims))
      sev_high <- ((severity_category_horizon_j$high_count/num_sims))
      sev_vhigh <- ((severity_category_horizon_j$very_high_count/num_sims))

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
