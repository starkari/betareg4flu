
#' Title
#'
#' @param data data frame outputted from from `reformat_data`
#' @param prediction_time_indsindicators of the values one wants to predict from
#' @param S_mean the number of harmonics for the mean term
#' @param S_Precision the number of harmonics for the precision term
#'
#' @return empty data_set_results data frame
#' data_set_results: a data frame which has the summarized scores with columns
#' "model" (model statement), "prediction_horizon",
#' "prediction_target" (date of predicted value), "log_score",
#' "pt_pred" (point prediction), "AE" (absolute error),
#' "interval_pred_lb_95" (95% Prediction Interval lower bound),
#' "interval_pred_ub_95" (95% Prediction Interval upper bound),
#' "interval_pred_lb_50" (50% Prediction Interval lower bound),
#' "interval_pred_ub_50" (50% Prediction Interval upper bound),
#' "precision" (predicted precision),
#' "shape1" (shape parameter 1 in beta distribution as a function of mean and precision),
#' "shape2" (shape parameter 2 in beta distribution as a function of mean and precision),
#' "DS_score" (log(variance)+(AE^2)/variance), "vari" (variance of prediction)
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- reformat_data(data = seasons_data_weekly_update,
#' county_name = "MA_Middlesex",
#' training_seasons = c("2015-2016" "2016-2017" "2017-2018"),
#' lags=1)
#' data_frames <- create_results_data_frames(data=dat,
#' prediction_time_inds = which(dat$train == FALSE),
#' S_mean=4,
#' S_Precision=4)
#' }

create_results_data_frames <- function (data, prediction_time_inds,
                                        S_mean, S_Precision) {
  data_set_results <- data.frame()

  row_indicator <- 1

  for (pti in prediction_time_inds) {
    data_set = data[pti,]
    horizons <- get_prediction_horizons(pti,data)
    data_set_results[row_indicator-1+horizons,1:(ncol(data_set)+23)] <-
      c(data_set,
        model = paste0("BetaReg", "(", S_mean, ",", S_Precision, ")"),
        prediction_horizon = rep(NA_integer_, length(horizons)),
        prediction_target = rep(NA, length(horizons)),
        log_score = rep(NA_real_, length(horizons)),
        pt_pred = rep(NA_real_, length(horizons)),
        AE = rep(NA_real_, length(horizons)),
        interval_pred_lb_95 = rep(NA_real_,  length(horizons)),
        interval_pred_ub_95 = rep(NA_real_, length(horizons)),
        interval_pred_lb_50 = rep(NA_real_, length(horizons)),
        interval_pred_ub_50 = rep(NA_real_, length(horizons)),
        precision = rep(NA_real_, length(horizons)),
        shape1 = rep(NA_real_, length(horizons)),
        shape2 = rep(NA_real_, length(horizons)),
        DS_score = rep(NA_real_, length(horizons)),
        stringsAsFactors = FALSE,
        vari = rep(NA_real_, length(horizons)),
        point_prediction_low_threshold_proportion = rep(NA_real_, length(horizons)),
        point_prediction_mod_threshold_proportion = rep(NA_real_, length(horizons)),
        point_prediction_high_threshold_proportion = rep(NA_real_, length(horizons)),
        point_prediction_vhigh_threshold_proportion = rep(NA_real_, length(horizons)),
        peak_prediction_low_threshold_proportion = rep(NA_real_, length(horizons)),
        peak_prediction_mod_threshold_proportion = rep(NA_real_, length(horizons)),
        peak_prediction_high_threshold_proportion = rep(NA_real_, length(horizons)),
        peak_prediction_vhigh_threshold_proportion = rep(NA_real_, length(horizons)),
        row.names = NULL)
    row_indicator <- row_indicator+max(horizons)
  }

  colnames(data_set_results) <- c(paste0("data_set.",colnames(data)),
                                  "model","prediction_horizon","prediction_target",
                                  "log_score","pt_pred","AE","interval_pred_lb_95",
                                  "interval_pred_ub_95","interval_pred_lb_50",
                                  "interval_pred_ub_50","precision","shape1",
                                  "shape2","DS_score","vari",
                                  "point_prediction_low_threshold_proportion",
                                  "point_prediction_mod_threshold_proportion",
                                  "point_prediction_high_threshold_proportion",
                                  "point_prediction_vhigh_threshold_proportion",
                                  "peak_prediction_low_threshold_proportion",
                                  "peak_prediction_mod_threshold_proportion",
                                  "peak_prediction_high_threshold_proportion",
                                  "peak_prediction_vhigh_threshold_proportion")




  result_data_frames <- list(data_set_results = data_set_results)

  return (result_data_frames)

}
