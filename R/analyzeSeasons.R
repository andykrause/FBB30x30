#*****************************************************************************************
#
#  Season Analysis Functions
#
#*****************************************************************************************

#' 
#' Analyze Season to get point differntials
#' 
#' Get point differentials across categories
#' 
#' @param season A `season` object 
#' @return Median difference between categories
#' @importFrom dplyr select
#' @importFrom stats median
#' @importFrom tidyselect contains
#' @importFrom magrittr "%>%"
#' @export

seasonAnalysis <- function(season,
                           ...){
  
  x_mat <- season %>% 
    dplyr::select(-tidyselect::contains('starts'),
                  -tidyselect::contains('moves'), 
                  -tidyselect::contains('rank'),
                  -tidyselect::contains('team'))
  
  data.frame(category = names(x_mat),
             diff = unlist(lapply(x_mat, categoryDiffEngine, ...)),
             players = nrow(x_mat))
  
}

#' 
#' Analyze Season to get point differntials
#' 
#' Get point differentials across categories
#' 
#' @param season A `season` object 
#' @return Median Difference between rankings for category
#' @importFrom stats median
#' @export

categoryDiffEngine <- function(x, metric = 'median', trim = FALSE){
  
  calcMetric <- get(metric)
  if (trim){
    xx <- sort(x)[2:(length(x) -1)]
  } else {
    xx <- sort(x)
  }
  round(abs(calcMetric(xx[-1] - xx[-length(xx)])),3)
}
