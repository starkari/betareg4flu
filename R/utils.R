
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