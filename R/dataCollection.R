#*****************************************************************************************
#
#  Data gathering functions
#
#*****************************************************************************************

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
