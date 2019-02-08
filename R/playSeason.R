


#' @export

playSeason <- function(draft_obj,
                       configs){
  
  seasonstats_ <- buildSeasonStats(configs)
  
  teams_ <- draft_obj$teams
  
  points_df <- purrr::map(.x = teams_,
                          .f = playTeamSeason,
                          bat_stats = seasonstats_$bat,
                          pitch_stats = seasonstats_$pitch,
                          configs = configs) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(team = 1:length(teams_)) %>%
    dplyr::select(team, tidyselect::everything())
  
  standings_df <- points_df %>%
    dplyr::mutate_at(., vars(-team), rank)
  
  reverse <- as.character(configs$scoring$stat[configs$scoring$category == '-'])
  for (rr in reverse) standings_df[[rr]] <- (configs$nbr_owner + 1) - standings_df[[rr]]
  
  total_df <- data.frame(team = standings_df$team,
                         points = rowSums(standings_df) - standings_df$team)
  
  structure(list(total = total_df,
                 standings = standings_df,
                 points = points_df,
                 teams = teams_),
            class = 'seasonResults')
}


#' @export

playTeamSeason <- function(team_df,
                           bat_stats,
                           pitch_stats,
                           configs){
  
  bat_cats <- configs$scoring[configs$scoring$class == 'hit', ]
  pitch_cats <- configs$scoring[configs$scoring$class == 'pitch', ]
  
  # Calc batting  
  team_bat <- team_df$roster %>% 
    dplyr::filter(!roster %in% c('SP', 'RP', 'P')) %>%
    dplyr::left_join(bat_stats,
                     by = 'player_id') %>%
    dplyr::mutate(obp = obp * (pa/(sum(pa, na.rm=T))),
                  slg = slg * (ab/(sum(ab, na.rm=T)))) %>%
    dplyr::summarize_at(., as.character(bat_cats$stat), sum, na.rm=TRUE)
  
  # calc pitching    
  team_pitch <- team_df$roster %>% 
    dplyr::filter(roster %in% c('SP', 'RP', 'P')) %>%
    dplyr::left_join(pitch_stats,
                     by = 'player_id') %>%
    dplyr::filter(!is.na(g)) %>%
    dplyr::mutate(whip = whip * (ip/(sum(ip, na.rm=TRUE)))) %>%
    dplyr::summarize_at(., as.character(pitch_cats$stat), sum, na.rm=TRUE)
  
  cbind(team_bat, team_pitch)
}

#' @export

buildSeasonStats <- function(configs){
  if (configs$season == 2019){
    
    bat_stats <- get(data(batprojs_df)) %>% 
      dplyr::filter(year == 2019) %>%
      dplyr::mutate(pa = ab * 1.1)
    
    pitch_stats <- get(data(pitchprojs_df)) %>%
      dplyr::filter(year == 2019)
  } else {
    
    bat_stats <- get(data(batting_df)) %>% 
      dplyr::filter(year == configs$season) %>%
      dplyr::left_join(., get(data(fielding_df)) %>%
        dplyr::select(player_id, e), 
        by = 'player_id') %>%
      dplyr::mutate(e = ifelse(is.na(e), 0 , e))
    
    pitch_stats <- get(data(pitching_df)) %>%
      dplyr::filter(year == configs$season)
    
  }
  structure(list(bat = bat_stats,
                 pitch = pitch_stats),
            class = 'seasonStats')
}


