


#' Title
#'
#' @param values the sine, cosine, and lag values for the given target week
#' @param county the county being forecasted for
#' @param threshold_data data frame of county thresholds
#' @param beta_fit the model fit for the given forecast date
#'
#' @return
#' @export
#'
#' @examples

get_distribution_severity_categories <- function(values, county, threshold_data, beta_fit){

  low_count <- 0
  moderate_count <- 0
  high_count <- 0
  very_high_count <- 0

  # extract mean and precision coefficients from fit
  mean_coef <- beta_fit$coefficients$mean %>%
    t() %>%
    as.data.frame()
  prec_coef <- beta_fit$coefficients$precision %>%
    t() %>%
    as.data.frame()

  # get threshold values
  thresholds <- threshold_data[which(threshold_data$Adjusted_County==county),
                               c("40","67.5","95")]
  # extract values from data that contribute to mean and precision values
  mean_values <- values %>%
    dplyr::select(colnames(mean_coef))

  prec_values <- values %>%
    dplyr::select(colnames(prec_coef))

  # get each terms contribution to mean and precision
  mean_comb <- suppressMessages(dplyr::full_join(mean_coef,mean_values)) %>%
    t()
  mean_contribution <- mean_comb[,1]*mean_comb[,2]

  prec_comb <- suppressMessages(dplyr::full_join(prec_coef,prec_values)) %>%
    t()
  prec_contribution <- prec_comb[,1]*prec_comb[,2]

  # get values for mean and precision for this forecast_date and target date
  mean_term <- plogis(sum(mean_contribution))
  prec_term <- exp(sum(prec_contribution))

  shape_1 <- mean_term*prec_term
  shape_2 <- (1-mean_term)*prec_term



  low_prop <- pbeta(q=as.numeric(thresholds[1]), shape1=shape_1, shape2=shape_2,
                    ncp = 0, lower.tail = TRUE, log.p = FALSE)
  moderate_prop <- pbeta(q=as.numeric(thresholds[2]), shape1=shape_1, shape2=shape_2,
                    ncp = 0, lower.tail = TRUE, log.p = FALSE) - low_prop
  high_prop <- pbeta(q=as.numeric(thresholds[3]), shape1=shape_1, shape2=shape_2,
                    ncp = 0, lower.tail = TRUE, log.p = FALSE) - moderate_prop -low_prop
  very_high_prop <- 1- high_prop - moderate_prop - low_prop


  return(list(low_prop=low_prop,
              moderate_prop=moderate_prop,
              high_prop=high_prop,
              very_high_prop=very_high_prop))

}
