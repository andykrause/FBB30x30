#*****************************************************************************************
#
#  Functions to help with the general data munging processes
#
#*****************************************************************************************

#' 
#' Fix Fantasy Pros Name Field
#' 
#' Extract out the player, pos and team from Fantasy Pros team names
#' 
#' @param x Vector of existing names in FP format
#' @param field ['name'] Name of existing field to convert
#' @param type [Position] Includes the position as well as name + team 
#' @return Data.frame of converted names, team and position
#' @importFrom stringr str_locate
#' @export

fixFPName <- function(df, 
                      field = 'name',
                      type = 'Position'){
  
  x <- as.character(df[[field]])
  
  # Find the start of Team
  beg <- stringr::str_locate(x, '[(]')[ ,1]
  end <- str_locate(x, '[)]')[ ,1]
  
  # If there is a position, find its start, if not find where multiple teams split
  if(type == 'Position'){
    mid <- str_locate(x, '[-]')[ ,1]
    space <- 2
  } else {
    mid <- str_locate(x, '[,]')[ ,1]
    space <- 1
  }
  
  xx <- data.frame(player = substr(x, 1, beg - 2),
                   team = substr(x, beg + 1, mid - space),
                   pos = substr(x, mid + 2, end - 1),
                   stringsAsFactors = FALSE)
  
  # Convert all outlfield spots to OF
  xx$pos <- gsub("CF", "OF", xx$pos)
  xx$pos <- gsub("RF", "OF", xx$pos)
  xx$pos <- gsub("LF", "OF", xx$pos)
  xx$pos <- gsub("OF,OF,OF", "OF", xx$pos)
  xx$pos <- gsub("OF,OF", "OF", xx$pos)
  
  # Return
  cbind(xx, df[, !names(df) %in% field])
  
}