#*****************************************************************************************
#
#  retroSheet scraping functions
#
#*****************************************************************************************

#' 
#' Scrape RetroSheet Annual Stats
#' 
#'  Get annual player statistic by player id
#' 
#' @param code Type of table to import (see data(baseballref_df))
#' @param year Year of data to grab 
#' @return Table of data for this type from this year
#' @importFrom readr read_lines
#' @importFrom purrr map
#' @importFrom dplyr bind_rows group_by summarize mutate filter
#' @importFrom magrittr "%>%"
#' @importFrom tidyr spread
#' @export

scrapeBRStats <- function(code, 
                          year){
  
  types_ok <- get(data(baseballref_df))$code
  
  if (!code %in% types_ok) stop('Type must be one of: ', paste(types_ok, collapse = ', '))
  
  br_url <- paste0('https://www.baseball-reference.com/leagues/MLB/', year, 
                   '-', code, '.shtml')
  
  raw_html <- readr::read_lines(br_url)
  
  stat_obj <- raw_html[grep('data-append-csv', raw_html)]
  
  structure(purrr::map(.x = stat_obj,
                       .f = brToTable) %>%
               dplyr::bind_rows() %>%
               dplyr::distinct(stat, player_id, .keep_all = T) %>%
               tidyr::spread(key = 'stat', value = 'value'),
            class = c('brstat', 'tbl_df', 'tbl', 'data.frame'))
  
}

#' 
#' Convert Raw BR data to a table
#' 
#' Convert scraped BR statistical information to a table
#' 
#' @param br_obj Baseball reference scraped object (build internally in scrapeBRStats()) 
#' @return Stats for each player in a key-value pair table
#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate
#' @importFrom magrittr "%>%"
#' @export

brToTable <- function(br_obj){
  
  br_ <- unlist(strsplit(br_obj, 'td><td'))
  
  ## Extract IDs
  player_id <- brExtractID(br_[1])
  
  ## Extract Teams
  team_id <- substr(br_[3], nchar(br_[3]) - 8, nchar(br_[3]) - 6)
  
  ## Extract Stats
  stats_ <- br_[4:length(br_)]
  purrr::map(.x = stats_,
             .f = brExtractStats) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(player_id = player_id,
                  team_id = team_id)
}

#' 
#' Extract each individual stat
#' 
#' Get a single stat name and value from HTML code
#' 
#' @param stat_v Single line of stat code
#' @return One row data.frame with stat and value
#' @importFrom stringr str_locate
#' @export

brExtractStats <- function(stat_v){
  
  prefix_end <- stringr::str_locate(stat_v, 'stat=')[, 2]
  
  stat_v <- substr(stat_v, prefix_end + 1, nchar(stat_v))
  
  stat_ <- unlist(strsplit(stat_v, ' '))
  
  suffix_end <- stringr::str_locate(stat_[2], '<')[, 1]
  
  value <- substr(stat_[2], 2, suffix_end - 1)
  
  if (grepl('%', value)){
    value <- as.numeric(gsub('%', '', value)) / 100
  } else {
    value <- as.numeric(value)
  }
  
  data.frame(stat = gsub("[^A-Za-z0-9]", '', stat_[1]),
             value = value,
             stringsAsFactors = FALSE)
  
}

#' 
#' Extract player ID
#' 
#' Get player ID from html code
#' 
#' @param name_obj Single line of stat code
#' @return Player's BR ID
#' @importFrom stringr str_locate
#' @export

brExtractID <- function(name_obj){
  
  prefix_end <- stringr::str_locate(name_obj[1], 'data-append-csv=')[, 2]
  
  name_v <- substr(name_obj, prefix_end + 1, nchar(name_obj))
  
  suffix_start <- stringr::str_locate(name_v, ' data-stat')[1, ]
  
  name_v <- substr(name_v, 1, suffix_start)
  
  gsub("[^A-Za-z0-9]", '', name_v)
}

#' 
#' Wrapper for getting BR stats (Generic Method)
#' 
#' Get all necessary stats of one of three types (fielding, pitching, batting)
#' 
#' @param type ('fielding', 'pitching' or 'batting')
#' @param year year of stats
#' @return Stats for that year
#' @importFrom dplyr select
#' @export

getBRStats <- function(type, year){
  
  type <- structure(type, class = type)
  
  UseMethod('getBRStats', type)
  
}

#' 
#' Wrapper for getting basic BR fielding stats
#' 
#' Get all necessary fielding stats 
#' 
#' @param type ('fielding', 'pitching' or 'batting')
#' @param year year of stats
#' @return Stats for that year
#' @importFrom dplyr select mutate
#' @method getBRStats fielding
#' @export

getBRStats.fielding <- function(type, year){
  
  structure(scrapeBRStats('standard-fielding', year) %>%
              dplyr::select(player_id, g = G, gs = GS, cg = CG, innings = Inndef, 
                            e = Edef) %>%
              dplyr::mutate(year = year),
            class = c('fielding', 'stats', 'tbl_df', 'tbl', 'data.frame'))
}  
  
#' 
#' Wrapper for getting basic BR batting stats
#' 
#' Get all necessary batting stats 
#' 
#' @param type ('fielding', 'pitching' or 'batting')
#' @param year year of stats
#' @return Stats for that year
#' @importFrom dplyr select mutate
#' @method getBRStats fielding
#' @export

getBRStats.batting <- function(type, year){
  
  structure(scrapeBRStats('standard-batting', year) %>%
               dplyr::select(player_id, pa = PA, ab = AB, h = H, r = R, hr = HR, rbi = RBI, 
                             bb = BB, hbp = HBP, ibb = IBB, sb = SB, so = SO, x2b = `2B`,
                             x3b = `3B`, avg = battingavg, obp = onbaseperc, 
                             slg = sluggingperc, gidp = GIDP, sf = SF, sh = SH)%>%
              dplyr::mutate(year = year),
            class = c('batting', 'stats', 'tbl_df', 'tbl', 'data.frame'))

}


#' 
#' Wrapper for getting basic BR pitching stats
#' 
#' Get all necessary pitching stats 
#' 
#' @param type ('fielding', 'pitching' or 'batting')
#' @param year year of stats
#' @return Stats for that year
#' @importFrom dplyr select mutate
#' @method getBRStats fielding
#' @export

getBRStats.pitching <- function(type, year){
  
  stand <- scrapeBRStats('standard-pitching', year) %>%
              dplyr::select(player_id, ip = IP, g = G, gs = GS, k = SO, sv = SV, w = W,
                            whip)

  start <- scrapeBRStats('starter-pitching', year) %>%
    dplyr::select(player_id, qs = QS)

  relieve <- scrapeBRStats('reliever-pitching', year) %>%
    dplyr::select(player_id, holds = Hold)

  pitch <- stand %>%
    dplyr::left_join(start, by = 'player_id') %>%
    dplyr::left_join(relieve, by = 'player_id') %>%
    dplyr::mutate(qs = ifelse(is.na(qs), 0, qs),
                  holds = ifelse(is.na(holds), 0, holds))%>%
    dplyr::mutate(year = year)
  
  structure(pitch, class = c('pitching', 'stats', 'tbl_df', 'tbl', 'data.frame'))  
  
}

#' Get Marcel projections
#' 
#' Scrape the Marcel 2019 projections from baseball reference 
#' 
#' @param year year of stats
#' @return Stats for that year
#' @importFrom dplyr bind_rows filter distinct select
#' @importFrom readr read_lines
#' @importFrom tidyr spread
#' @importFrom purrr map
#' @export


scrapeMarcelProjections <- function(year = 2018){
  
  # Make URL and scrape raw html
  br_url <- paste0('https://www.baseball-reference.com/leagues/MLB/', year, 
                   '-projections.shtml')
  raw_html <- readr::read_lines(br_url)
  
  # Limit to table objects
  proj_obj <- raw_html[grep('data-append-csv', raw_html)]
  
  # Convert to a data.frame
  all_proj <- purrr::map(.x = proj_obj,
                         .f = brToTable) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(stat, player_id, .keep_all = T) %>%
    tidyr::spread(key = 'stat', value = 'value')
  
  ## Split into batting and pitching
  
  # Batting
  bat_proj <- structure(all_proj %>%
                          dplyr::filter(!is.na(AB)) %>%
                          dplyr::select(ab = AB, h = H, r = R, hr = HR, rbi = RBI, bb = BB,
                                        avg = battingavg, obp = onbaseperc, 
                                        slg = sluggingperc, so = SO, hbp = HBP, ibb = IBB,
                                        sb = SB, sf = SF, sh = SH, gidp = GIDP, reliability),
                        class = c('battingProj', 'tbl_df', 'tbl', 'data.frame'))
  
  # Pitching
  pitch_proj <- structure(all_proj %>%
                            dplyr::filter(!is.na(IP)) %>%
                            dplyr::select(ip = IP, k = SO, bb = BB, era = earnedrunavg,
                                          h = H, r = R, sv = SV, whip, reliability),
                          class = c('pitchingProj', 'tbl_df', 'tbl', 'data.frame'))
  
  # Return
  structure(list(batting = bat_proj,
                 pitching = pitch_proj),
            class = 'marcelProjections')                      
}




  
