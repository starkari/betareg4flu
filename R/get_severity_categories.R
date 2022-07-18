


#' Title
#'
#' @param values 
#' @param county 
#' @param threshold_data 
#'
#' @return
#' @export
#'
#' @examples

get_severity_categories <- function(values, county, threshold_data){
  
  low_count <- 0
  moderate_count <- 0
  high_count <- 0
  very_high_count <- 0
  
  thresholds <- threshold_data[which(threshold_data$Adjusted_County==county),
                               c("40","67.5","95")]
  
  for(value in values) {
    
    if(value <= thresholds[1])  {
      low_count <- low_count+1
    }
    if(value > thresholds[1] & value <= thresholds[2])  {
      moderate_count <- moderate_count+1
    }
    
    if(value > thresholds[2] & value <= thresholds[3])  {
      high_count <- high_count+1
    }
    
    if(value > thresholds[3])  {
      very_high_count <- very_high_count+1
    }
    
  }
  
  
  return(list(low_count=low_count,
              moderate_count=moderate_count,
              high_count=high_count,
              very_high_count=very_high_count))
  
}
