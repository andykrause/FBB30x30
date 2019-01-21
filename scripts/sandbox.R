

### Data Munge Functions

#' @title prepAddPlayerID
#' @description Add Lahman unique player IDs to MLB/FantasyPros data
#' @param pos_proj data.frame of projection data
#' @param player_data data.frame of player information
#' @param teampos_data data.frame of team and position data
#' @return data.frame containing player name, team, position
#' @export

prepAddPlayerID <- function(x_data,
                            player_df){
  
  ## Make matches
  
  match_df <- x_df %>%
    dplyr::mutate(temp_id = 1:nrow(x_df)) %>%
    dplyr::left_join(players_df[, c('full', 'player_id')],
                     by=c('player' = 'full')) %>%
    dplyr::group_by(temp_id) %>%
    dplyr::mutate(count = n()) %>%
    dplyr::ungroup()
  
  ## Isolate the single matches from double and those that missed
  
  match_1 <- match_df %>%
    dplyr::filter(count == 1 & !is.na(player_id)) %>%
    dplyr::mutate(temp_id = NULL,
                  count = NULL)
  message(nrow(match_1),' of ', nrow(x_df), ' direct matches\n')
  
  match_more <- match_df %>% dplyr::filter(count > 1)
  message(nrow(match_more)/2,' of ', nrow(x_df), ' duplicate matches\n')
  
  match_no <- match_df %>% dplyr::filter(is.na(player_id))
  message(nrow(match_no),' of ', nrow(x_df), ' failed matches\n')
  
  ## Fix duplicates
  
  # Prep fielding data (data that has teams and years on it)
  dup_df <- players_df %>%
    dplyr::filter(player_id %in% match_more$player_id)
  
  # Match up the duplicates
  match_more <- bind_rows(purrr::map(.x=split(match_more, match_more$temp_id),
                                     dup_df=dup_df,
                                     .f=prepMatchDup)) %>%
    dplyr::mutate(temp_id = NULL,
                  count = NULL)
  
  ## Work on the non matches
  match_no <- match_no %>%
    dplyr::mutate(temp_id = NULL,
                  count = NULL,
                  player = gsub(' Jr.', '', player))
  
  space_loc <- unlist(lapply(str_locate_all(match_no$player, " "),
                             function(x) max(unlist(x))))
  match_no$last_name <- tolower(substr(match_no$player, space_loc + 1, 100))
  match_no$temp_id <- 1:nrow(match_no)
  
  player_last <- player_df %>%
    dplyr::mutate(name_last = tolower(name_last)) %>%
    dplyr::filter(name_last %in% match_no$last_name) %>%
    dplyr::filter(!player_id %in% c(match_1$player_id, match_more$player_id)) %>%
    dplyr::filter(!unlist(lapply(ytpg, is.null)))
  
  match_none <- dplyr::bind_rows(purrr::map(.x=split(match_no, match_no$temp_id),
                                            .f=prepMatchNone,
                                            player_df=player_last)) %>%
    dplyr::mutate(last_name = NULL,
                  temp_id = NULL)
  
  rbind(match_1, match_more, match_none)
  
}

#' @title prepMatchDup
#' @description Fix duplicate matches
#' @param match_df data.frame of duplicate matches
#' @param dup_df data.frame of team and positions of players
#' @return data.frame containing player name, team, position
#' @export

prepMatchDup <- function(match_df,
                         dup_df){
  
  imd <- match_df %>%
    dplyr::distinct(player, team, year, pos, .keep_all = T)
  
  imd$pos[imd$pos %in% c('RP', 'SP', 'SP | RP', 'SP,RP')] <- "P"
  
  for (i in 1:nrow(imd)){
    
    # Get retro ids
    retros <- dup_df$retro_id[dup_df$full == imd$player[i]]
    res_df <- purrr::map(.x = retros,
                                .f = scrapeRetroAnnualData,
                                year = imd$year[i]) %>%
      purrr::map(., .f = function(x){
        data.frame(playerid = x$fielding$playerid,
                   team = x$fielding$team) %>%
          dplyr::distinct(team, .keep_all = T)}) %>%
      dplyr::bind_rows()
    
    
    res_df <- res_df[res_df$team = ]  
     
  }
  
  
}

#' @title prepMatchNone
#' @description Matches on last name only
#' @param no_row None matched players
#' @param player_df Data.frame of player names and info
#' @return data.frame containing player name, team, position
#' @export

prepMatchNone <- function(no_row,
                          player_df){
  
  ipl <- player_df[tolower(player_df$name_last) == no_row$last_name, ]
  
  no_row$position[no_row$position %in% c('SP', "RP", 'P', 'SP | RP')] <- "P"
  
  if (nrow(ipl) == 0) return(NULL)
  
  ipl <- ipl %>% tidyr::unnest()
  
  if (nrow(ipl) >= 1){
    ipl <- ipl[ipl$position %in% trimws(substr(no_row$position, 1, 2)), ]
  }
  
  if (length(unique(ipl$player_id)) > 1){
    ipl <- ipl[ipl$team %in% no_row$team &
                 ipl$position %in% trimws(substr(no_row$position, 1, 2)), ]
  }
  
  if (length(unique(ipl$player_id)) == 1){
    no_row$player_id <- ipl$player_id[1]
    return(no_row)
  }
}


scrapeBRStats <- function(type, year){
  
  types_ok <- c('starter-pitching', 'reliever-pitching')
  
  if (!type %in% types_ok) stop('Type must be one of: ', paste(types_ok, collapse = ', '))
  
  br_url <- paste0('https://www.baseball-reference.com/leagues/MLB/', year, 
                   '-', type, '.shtml')

  raw_html <- readr::read_lines(br_url)
  
  stat_obj <- raw_html[grep('data-append-csv', raw_html)]
  
  purrr::map(.x = stat_obj,
             .f = brToTable) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(team_id != 'D"') %>%
    dplyr::group_by(player_id, stat) %>%
    dplyr::summarize(value = sum(value))%>%
    tidyr::spread(key = 'stat', value = 'value')
    
}

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

brExtractStats <- function(stat_v){
  
  prefix_end <- stringr::str_locate(stat_v, 'stat=')[, 2]
  
  stat_v <- substr(stat_v, prefix_end + 1, nchar(stat_v))
  
  stat_ <- unlist(strsplit(stat_v, ' '))
  
  suffix_end <- stringr::str_locate(stat_[2], '<')[, 1]
  
  data.frame(stat = gsub("[^A-Za-z0-9]", '', stat_[1]),
             value = as.numeric(substr(stat_[2], 2, suffix_end - 1)))
  
}

brExtractID <- function(name_obj){
  prefix_end <- stringr::str_locate(name_obj[1], 'data-append-csv=')[, 2]
  name_v <- substr(name_obj, prefix_end + 1, nchar(name_obj))
  suffix_start <- stringr::str_locate(name_v, ' data-stat')[1, ]
  name_v <- substr(name_v, 1, suffix_start)
  gsub("[^A-Za-z0-9]", '', name_v)
}




b <- strsplit(a[1456], 'td><td')

c <- b[[1]][4:length(b[[1]])]

j <- function(x) stringr::str_locate(x, 'stat')[,2] + 3

jj <- function(x) substr(x, j(x), nchar(x)- 2)

jjj <- function(x) strsplit(jj(x), ' ') %>% unlist()

ff <- function(x) gsub(pattern = "[^A-Za-z0-9]", replacement = '', x)

jjjj <- function(x){
  xx<-jjj(x)
  data.frame(name = ff(xx[1]),
             value = as.numeric(gsub('>', '', xx[2])))
} 

jjjj(x) -> h

lapply(c, jjjj)



# Read in raw htm data
url_raw <- tryCatch({url %>% xml2::read_html() %>%
    rvest::html_nodes(., css = 'body') %>%
    rvest::html_text() %>%
    strsplit('\n') %>%
    unlist()}, error = function(e){NULL})

  df <- read_html(paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=p&lastndays=7&dates=fromandto&fromandto=", t1, ".", t2, "&level=mlb&franch=&stat=&stat_value=0"))
  df <- df %>% html_nodes(xpath = '//*[@id="daily"]') %>% html_table(fill = TRUE)
