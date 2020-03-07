
  library(FBB30x30)
  library(tidyverse)

  configs <- setConfigs(nbr_owners = 8,
                        season_year = 2019,
                        rankings = c('rrv', rep('fp', 7)))

  
  customRankings(type = 'rrv',
                 configs = configs,
                 vmc = 11) -> x
  
  
  
  
 a <- setConfigs(nbr_owners = 8,
                 season_year = 2019)  
 cr <- customRankings(type = 'rrv',
                      configs = configs)  
  
  
  
  
  
  
  
  
  
  
  
  ms_ <- multiFBBSimulations(sims = 25,
                             season_year = 2017,
                             rankings = c('rrv', 'rrvt', 'rrvp', 'rrvpt', 'fp'),
                             strategies = c('ba', 'ba5'))
  
  lm(points ~ ranking + strategy + as.factor(team), data = ms_$summary$summ)%>% summary()
  
  lm(points ~ ranking + as.factor(team), data = ms_$summary$summ)%>% summary()
  
  
  
  
  if(F){}
  crBuildProjs <- function(configs){
    
  }
  
  
  projs_ <- list(bat = get(data(batprojs_df)),
                 pitch = get(data(pitchprojs_df)))
  
  # Stats
  stats_ <- list(bat = get(data(batting_df)),
                 pitch = get(data(pitching_df)),
                 field = get(data(fielding_df)))
  
  crBuildStats <- function(configs,
                           ...){
    
    vmc('Building Stats data for previous MLB season', ...)
    
    bat_df <- get(data(batting_df)) %>%
      dplyr::filter(year == configs$season - 1) %>%
      dplyr::left_join(., get(data(fielding_df)) %>%
                          dplyr::filter(year == configs$season - 1) %>%
                          dplyr::select(player_id, e),
                       by = 'player_id')

    # Limit by plat appearences
    limit_pa <- sort(bat_df$pa, 
                     decreasing = TRUE)[configs$summary$total_hitters * 1.5]
    bat_df <- bat_df %>%
      dplyr::filter(pa >= limit_pa)
    
    
    
    pitch_df <- get(data(pitching_df)) %>%
      dplyr::filter(year == configs$season - 1)
    
    
  }

  ## Prep Statistics
  
  stats_df <- pitching_df %>%
    dplyr::filter(year == configs$season - 1) %>%
    dplyr::mutate(SP = ifelse(gs / g >= .5, T, F))
  
  
  # Limit stats by plate appearances
  gs_limit <- sort(stats_df$gs[stats_df$SP], 
                   decreasing = TRUE)[round(.60 * (roster_count * 2), 0)]
  sv_limit <- sort(stats_df$sv[!stats_df$SP], 
                   decreasing = TRUE)[round(.20 * (roster_count * 2), 0)]
  hd_limit <- sort(stats_df$holds[!stats_df$SP], 
                   decreasing = TRUE)[round(.20 * (roster_count * 2), 0)]
  
  limits_ <- list(gs = gs_limit, 
                  sv = sv_limit,
                  hd = hd_limit)
  
  # Filter to roster-likely pitchers
  stats_df <- stats_df %>%
    dplyr::filter((gs >= gs_limit & SP) | (sv >= sv_limit & !SP) | 
                    (holds >= hd_limit & !SP))
  
  ## Prep Projections
  
  proj_df <- pitchprojs_df %>%
    dplyr::filter(year == configs$season)%>%
    dplyr::mutate(SP = ifelse(gs/g >= .5, T, F))
  