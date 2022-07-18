


#' Title
#'
#' @param object: output of a call to `fit_beta_model`
#' @param p: lag vale
#' @param start_value: the p lagged values before the time being predicted
#' @param coefs_subset: the sine and cosine terms for the value being predicted
#'
#' @return one possible one week ahead forecast
#' @export
#' @import betareg
#' @examples
#'
#' \dontrun{
#' "example coming once I check the start_value and coefs_subset work
#' with updated code"
#' }
#'
forecast_1_step <- function(object,
                            p = 1,
                            start_value,
                            coefs_subset){

  pt_sample <- shape1 <- shape2 <- mean <- precision <-
    as.data.frame(matrix(NA_real_, nrow = 1, ncol = 1,
                         dimnames = list(NULL, "ph 1")))

  starti <- as.data.frame(matrix(sapply(start_value, rep, each = 1), nrow = 1))
  vari <- sapply(starti, logit_FUN)
  # coefs_subset is the sine and cosine terms that form the design matrix for the future value
  # coefi is the design matrix with sines, cosines, and laggs
  coefi <- as.data.frame(c(vari,coefs_subset))
  names(coefi)[1 : p] <- paste0("p", p:1)
  meani <- predict(object,
                                coefi,
                                type = "response")
  precisioni <- predict(object,
                                          coefi,
                                          type = "precision")
  shape1i <- meani * precisioni
  shape2i <- precisioni - shape1i

  starti <- rbeta(n = 1,
                  shape1 = shape1i,
                  shape2 = shape2i)
  pt_sample <- starti




  res <- list(pt_sample = pt_sample,
              mean = mean,
              precision = precision,
              shape1 = shape1,
              shape2 = shape2)
  return(res)
}












