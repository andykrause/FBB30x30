#*****************************************************************************************
#
#  Functions to calculate statistical measures
#
#*****************************************************************************************

#' 
#' Calculate SLG
#' 
#' Calculate slugging percentage
#' 
#' @param x_df Data.frame 
#' @return Vector of slugging percentages
#' @export

calcSLG <- function(x_df) round((x_df$h + x_df$x2b + 2*x_df$x3b + 3*x_df$hr)/x_df$ab, 3)

#' 
#' Calculate Plate Appearances
#' 
#' Calculate Plate Appearances
#' 
#' @param x_df Data.frame 
#' @return Vector of plate appearances
#' @export

calcPA <- function(x_df) x_df$ab + x_df$ibb + x_df$hbp + x_df$bb + x_df$sh + x_df$sf
  
#' 
#' Calculate OBP
#' 
#' Calculate On Base Percentages
#' 
#' @param x_df Data.frame 
#' @return Vector of plate appearances
#' @export

calcOBP <- function(x_df){
  
  if (!'pa' %in% names(x_df)){
    x_df$pa <- calcPA(x_df)
  }
  
  round((x_df$h + x_df$ibb + x_df$hbp + x_df$bb)/ (x_df$pa - x_df$sh), 3)
  
}     

#' 
#' Calculate WHIP
#' 
#' Calculate WHIP
#' 
#' @param x_df Data.frame 
#' @return Vector of WHIP
#' @export

calcWHIP <- function(x_df) round((x_df$ibb + x_df$bb + x_df$h)/ x_df$ip, 2)
  