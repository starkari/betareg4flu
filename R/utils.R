
#' Title
#'
#' @param x a real number on the interval [1e-3,1)
#'
#' @return the logit of x
#' @export
#'
#' @examples
logit_FUN <- function(x){
  qlogis(x)
}


### ---------------------

#' Get Log Score
#'
#' @param value the value you want the log score of (true value)
#' @param county county the forecast was made
#' @param threshold_data data frame of threshold cutoffs by county
#' @param pred_thresholds the predicted threshold proportions array in low, mod, high, vhigh order
#'
#' @return
#' @export
#'
#' @examples
get_log_score <- function(value,county,threshold_data,pred_thresholds) {

  score <- NA
  threshold <- NA

  truth_severity <- get_severity_categories(values = truth_value,
                                            county=county,
                                            threshold_data = threshold_data)

  if (truth_severity$low_count != 0) {
    score <- log(pred_thresholds[1])
    threshold <- "low"
  }

  if (truth_severity$moderate_count != 0) {
    score <- log(pred_thresholds[2])
    threshold <- "mod"
  }

  if (truth_severity$high_count != 0) {
    score <- log(pred_thresholds[3])
    threshold <- "high"
  }

  if (truth_severity$very_high_count != 0) {
    score <- log(pred_thresholds[4])
    threshold <- "vhigh"
  }


  return(list(score=score, threshold=threshold))


}



#### ------------------------------------------------------ ###

### NEEDS MODIFYING ONCE `forecast_1_step` is finalized

# obs is percentage
#' Title
#'
#' @param sim_object: a sample of a call to Beta_forecast_p
#' @param obs: observed prediction target
#' @param ph: prediction horizon
#'
#' @return the log score
#' @export
#'
#' @examples
#'
cal_log_score <- function(sim_object,
                          obs,
                          ph){
  shape1 <- sim_object$shape1[, ph]
  shape2 <- sim_object$shape2[, ph]
  dens <- mapply(dbeta,
                 x = obs/100,
                 shape1 = shape1,
                 shape2 = shape2)
  log(mean(dens)) - log(100)
}
