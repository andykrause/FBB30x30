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

### Data Munge Functions

#' @title prepAddPlayerID
#' @description Add Lahman unique player IDs to MLB/FantasyPros data
#' @param pos_proj data.frame of projection data
#' @param player_data data.frame of player information
#' @param teampos_data data.frame of team and position data
#' @return data.frame containing player name, team, position
#' @export

addPlayerID <- function(x_df,
                        players_df){
  
  players_df$first <- gsub(' ', '', players_df$first)
  players_df$full <- paste0(players_df$first, ' ', players_df$last)
  
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
    dplyr::filter(count == 1 & !is.na(player_id))
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
  match_more <- bind_rows(purrr::map(.x = split(match_more, match_more$temp_id),
                                     dup_df = dup_df,
                                     .f = prepMatchDup)) %>%
    dplyr::filter(!is.na(player_id)) %>%
    dplyr::distinct(player_id, year, .keep_all = TRUE)
  message('...', nrow(match_more),' duplicates matched on second try\n')
  
  
  ## Work on the non matches
  match_no <- match_no %>%
    dplyr::mutate(player = gsub(' Jr.', '', player))
  
  space_loc <- unlist(lapply(str_locate_all(match_no$player, " "),
                             function(x) max(unlist(x))))
  match_no$last_name <- tolower(substr(match_no$player, space_loc + 1, 100))

  player_last <- players_df %>%
    dplyr::mutate(last = tolower(last)) %>%
    dplyr::filter(last %in% match_no$last_name)
  
  match_none <- dplyr::bind_rows(purrr::map(.x = split(match_no, match_no$temp_id),
                                            .f = prepMatchNone,
                                            player_df = player_last))
  message('...', nrow(match_none),' missing matched on second try\n')
  
  matched_ids <- c(match_1$temp_id, match_more$temp_id, match_none$temp_id)
  not_matched <- match_df[!match_df$temp_id %in% matched_ids, ] %>%
    dplyr::mutate(player_id = paste0('_', gsub(' ', '', player))) %>%
    dplyr::distinct(player_id, .keep_all = T)
  
  dplyr::bind_rows(list(match_1, match_more, match_none, not_matched)) %>%
    dplyr::select(-temp_id, -count, -last_name) 
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
  
  # Get retro ids
  retros <- dup_df$retro_id[dup_df$full == imd$player[i]]
  res_df <- purrr::map(.x = retros,
                       .f = scrapeRetroAnnualData,
                       year = pmin(imd$year[i], 2018)) %>%
    purrr::map(., .f = function(x){
      data.frame(player_id = x$fielding$playerid,
                 team = x$fielding$team,
                 stringsAsFactors = FALSE) %>%
        dplyr::distinct(team, .keep_all = T)}) %>%
    dplyr::bind_rows()
  res_df <- res_df %>%
    dplyr::left_join(get(data(teams_df)) %>%
                       rename(team = retrosheet_id), by = 'team')
  
  res_df <- res_df[res_df$team_id == imd$team, ] 
  match_df$player_id <- as.character(res_df$player_id[1])
  match_df
}

#' @title prepMatchNone
#' @description Matches on last name only
#' @param no_row None matched players
#' @param player_df Data.frame of player names and info
#' @return data.frame containing player name, team, position
#' @export

prepMatchNone <- function(no_row,
                          player_df){
  
  ipl <- player_df[tolower(player_df$last) == no_row$last_name, ]
  
  no_row$pos[no_row$pos %in% c('SP', "RP", 'P', 'SP | RP')] <- "P"
  
  if (nrow(ipl) == 0) return(NULL)
  
  if (nrow(ipl) >= 1){
    
    # Check if under given
    ipl$givenlast <- paste0(tolower(ipl$given), ' ', ipl$last)
    if (tolower(no_row$player) %in% ipl$givenlast){
      no_row$player_id <- ipl$player_id[which(tolower(no_row$player) == ipl$givenlast)[1]]
      return(no_row)
    }
  }
  
  fl <- which(tolower(substr(ipl$first, 1, 1)) %in% tolower(substr(no_row$player, 1, 1)) |
                tolower(substr(ipl$given, 1, 1)) %in% tolower(substr(no_row$player, 1, 1)))
  
  if (length(fl) == 1){
    no_row$player_id <- ipl$player_id[fl]
    return(no_row)
  }
  
  return(NULL)
}
