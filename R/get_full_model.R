#' Title
#'
#' @param S_mean the number of harmonics for the mean term
#' @param S_Precision the number of harmonics for the precision term
#' @param lags the dumber of lags
#' @param model_name the name of the model being formatted, defaults to hhh4
#'
#' @return a string of the full model parameterization to be passed in to
#' betareg
#' @export
#'
#' @examples
#'
#' \dontrun{
#' full_model <- get_full_model(S_mean=4, S_Precision=4,
#' lags=1, model_name = "hhh4")
#' }
#'
#'

get_full_model <- function(S_mean, S_Precision, lags,
                           model_name = "hhh4"){
  if (model_name == "hhh4") {
    mean_covar <- paste0(c(rep("sin_InPeriod",S_mean),rep("cos_InPeriod",S_mean)),1 : S_mean)
    precision_covar <- paste0(c(rep("sin_InPeriod",S_Precision), rep("cos_InPeriod",S_Precision)), 1 : S_Precision)
    if (lags == 0) {
      mean_AR <- ""
    } else {
      mean_AR <- paste("new_ili ~", paste(paste0("p", 1 : lags), collapse = " + "))
    }
    mean_model <- paste(mean_AR, paste(mean_covar,collapse = " + "), sep = " + ")
    precision_model <- paste(precision_covar, collapse = " + ")
    full_model <- paste(mean_model, "|", precision_model)
  }

  return (full_model)
}
