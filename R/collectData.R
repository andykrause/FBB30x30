#*****************************************************************************************
#
#  Data collection functions
#
#*****************************************************************************************

### Baseball-Reference.com ---------------------------------------------------------------

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

### Retrosheet ---------------------------------------------------------------------------


#' 
#' Scrape RetroSheet Annual Stats
#' 
#'  Get annual player statistic by player id
#' 
#' @param playerid 'Retrosheet' player id
#' @param years Years of data to grab 
#' @return List of pitching, batting and fielding annual stats
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate filter
#' @importFrom magrittr "%>%"
#' @export

scrapeRetroAnnualData <- function(playerid, 
                                  years = 2018){
  
  # Create URL
  url <- paste0('https://www.retrosheet.org/boxesetc/', substr(playerid, 1,1), 
                '/P', playerid, '.htm')
  
  # Read in raw htm data
  url_raw <- tryCatch({url %>% xml2::read_html() %>%
      rvest::html_nodes(., css = 'body') %>%
      rvest::html_text() %>%
      strsplit('\n') %>%
      unlist()}, error = function(e){NULL})
  
  if (is.null(url_raw)) return(NULL)
  
  ## Extract batting data
  
  if(any(grepl('Batting Record', url_raw))){ 
    url_raw <- url_raw[(grep('    Batting Record', url_raw)[1]):length(url_raw)]  
    batt_end <- min(which(nchar(url_raw) == 0)[1], grep('[(]', url_raw)[1])
    batting <- url_raw[2:(batt_end - 1)] 
    batting <- purrr::map(batting[-1],
                          .f = organizeRSAnnBatting) %>%
      dplyr::bind_rows()%>%
      dplyr::mutate(playerid = playerid)%>%
      dplyr::filter(year %in% years)%>%
      dplyr::arrange(year, desc(g))
  } else {
    batting <- NULL
    batt_end <- 1
  }
  
  # Remove batting from url object
  url_raw <- url_raw[batt_end:length(url_raw)]
  
  ## Extract pitching data
  
  if(any(grepl('Pitching Record', url_raw))){ 
    url_raw <- url_raw[(grep('    Pitching Record', url_raw)[1]):length(url_raw)]  
    pitch_end <- min(which(nchar(url_raw) == 0)[1], grep('[(]', url_raw)[1])
    pitching <- url_raw[2:(pitch_end - 1)]
    pitching <- purrr::map(pitching[-1],
                           .f = organizeRSAnnPitching) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(playerid = playerid)%>%
      dplyr::filter(year %in% years)%>%
      dplyr::arrange(year, desc(ip))
  } else {
    pitching <- NULL
    pitch_end <- 1
    
  }
  
  # Remove pitching from url object
  url_raw <- url_raw[pitch_end:length(url_raw)]
  
  ## Extract fielding
  
  url_raw <- url_raw[(grep('    Fielding Record', url_raw)[1]):length(url_raw)]  
  field_end <- min(which(nchar(url_raw) == 0)[1], grep('[(]', url_raw)[1])
  fielding <- url_raw[2:(field_end-1)] 
  fielding <- purrr::map(as.list(fielding[-1]),
                         .f = organizeRSAnnFielding) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(playerid = playerid) %>%
    dplyr::filter(year %in% years) %>%
    dplyr::arrange(year, desc(inn))
  
  ## Return  
  structure(list(pitching = pitching,
                 fielding = fielding,
                 batting = batting), class = c('retrosheet', 'raw', 'annual'))
}

#' 
#' Organize Batting Data
#' 
#' Organize retrosheet raw batting data annual
#' 
#' @param x Raw retrosheet htm table row 
#' @return Data.frame of annual batting stats
#' @export

organizeRSAnnBatting <- function(x){
  
  data.frame(year = as.integer(substr(x, 1, 4)),
             team = as.character(substr(x, 6, 10)),
             g = as.integer(substr(x, 29, 31)),
             ab = as.integer(substr(x, 35, 37)),
             r = as.integer(substr(x, 39, 42)),
             h = as.integer(substr(x, 44, 47)),
             x2b = as.integer(substr(x, 49,51)),
             x3b = as.integer(substr(x, 53, 55)),
             hr = as.integer(substr(x, 57, 60)),
             rbi = as.integer(substr(x, 61, 64)),
             bb = as.integer(substr(x, 66, 69)),
             ibb = as.integer(substr(x, 71, 73)),
             so = as.integer(substr(x, 75, 78)),
             hbp = as.integer(substr(x, 80, 82)),
             sh = as.integer(substr(x, 84, 86)),
             sf = as.integer(substr(x, 88, 90)),
             xi = as.integer(substr(x, 92, 94)),
             roe = as.integer(substr(x, 96, 98)),
             gdp = as.integer(substr(x, 100, 102)),
             sb = as.integer(substr(x, 105, 107)),
             cs = as.integer(substr(x, 109, 111)),
             avg = as.numeric(substr(x, 113, 117)),
             obp = as.numeric(substr(x, 119, 123)),
             slg = as.numeric(substr(x, 125, 129)),
             bfw = as.numeric(substr(x, 131, 136)),
             stringsAsFactors = FALSE)
  
}

#' 
#' Organize Pitching Data
#' 
#' Organize retrosheet raw pitching data annual
#' 
#' @param x Raw retrosheet htm table row 
#' @return Data.frame of annual piching stats
#' @export

organizeRSAnnPitching <- function(x){
  
  data.frame(year = as.integer(substr(x, 1, 4)),
             team = as.character(substr(x, 6, 10)),
             g = as.integer(substr(x, 29, 31)),
             gs = as.integer(substr(x, 33, 35)),
             cg = as.integer(substr(x, 37, 39)),
             sho = as.integer(substr(x, 41, 43)),
             gf = as.integer(substr(x, 45, 47)),
             sv = as.integer(substr(x, 49, 51)),
             ip = as.numeric(substr(x, 53, 58)),
             h = as.integer(substr(x, 60, 63)),
             bfp = as.integer(substr(x, 66, 69)),
             hr = as.integer(substr(x, 71, 73)),
             r = as.integer(substr(x, 75, 78)),
             er = as.integer(substr(x, 80, 83)),
             bb = as.integer(substr(x, 85, 88)),
             ib = as.integer(substr(x, 90, 92)),
             so = as.integer(substr(x, 94, 97)),
             sh = as.integer(substr(x, 99, 101)),
             sf = as.integer(substr(x, 103, 105)),
             wp = as.integer(substr(x, 107, 109)),
             hbp = as.integer(substr(x, 111, 113)),
             bk = as.integer(substr(x, 115, 117)),
             x2b = as.integer(substr(x, 119, 121)),
             x3b = as.integer(substr(x, 123, 125)),
             gdp = as.integer(substr(x, 127, 129)),
             roe = as.integer(substr(x, 131, 133)),
             w = as.integer(substr(x, 135, 137)),
             l = as.integer(substr(x, 139, 141)),
             era = as.numeric(substr(x, 143, 148)),
             rs = as.numeric(substr(x, 150, 155)),
             pw = as.numeric(substr(x, 157, 160)),
             stringsAsFactors = FALSE)
  
}

#' 
#' Organize Fielding Data
#' 
#' Organize retrosheet raw fielding data annual
#' 
#' @param x Raw retrosheet htm table row 
#' @return Data.frame of annual fielding stats
#' @export

organizeRSAnnFielding <- function(x){
  
  df = data.frame(year = as.integer(substr(x, 1, 4)),
                  team = substr(x, 6, 10),
                  pos = substr(x, 26, 27),
                  g = as.integer(substr(x, 30, 32)),
                  gs = as.integer(substr(x, 35, 37)),
                  cg = as.integer(substr(x, 40, 42)),
                  inn = as.numeric(substr(x, 45, 50)),
                  po = as.integer(substr(x, 53, 56)),
                  a = as.integer(substr(x, 59, 61)),
                  err = as.integer(substr(x, 64, 66)),
                  dp = as.integer(substr(x, 70, 72)),
                  tp = as.integer(substr(x, 73, 75)),
                  stringsAsFactors = FALSE)
  
  if (grepl('P', df$pos)){
    
    df$pb = as.integer(substr(x, 78, 80))
    df$sb = as.integer(substr(x, 83, 85))
    df$cs = as.integer(substr(x, 87, 90))
    df$pko = as.integer(substr(x, 92, 95))
    df$avg = as.numeric(substr(x, 97, 101))
    
  } else {
    df$avg = as.numeric(substr(x, 77,81))
  }
  df
}
