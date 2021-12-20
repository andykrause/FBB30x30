
  library(FBB30x30)
  library(tidyverse)

  configs <- setConfigs(nbr_owners = 8,
                        season_year = 2020,
                        rankings = c('rrv', rep('fp', 7)))

  
  x <- customRankings(type = 'rrv',
                 configs = configs,
                 vmc = 11) %>%
    dplyr::mutate(rrv = round(rrv, 2))
  
  
  data(rankings_df)
  
  full_df <- x %>% 
    dplyr::left_join(., rankings_df %>%
                       dplyr::filter(year == 2020) %>%
                       dplyr::select(player_id, adp = ranking),
                     by = 'player_id') %>%
    dplyr::select(-c(pos_list, player_id)) %>%
    dplyr::mutate(diff = adp - ranking) %>%
    dplyr::arrange(ranking)
  
  team_ <- purrr::map(.x = full_df %>% plyr::dlply('team'),
                      .f = function(x) x %>% dplyr::slice(1:20))
  
  pos_ <- purrr::map(.x = x %>% 
                       dplyr::left_join(., rankings_df %>%
                                            dplyr::filter(year == 2020) %>%
                                            dplyr::select(player_id, adp = ranking),
                                        by = 'player_id') %>% 
                       dplyr::mutate(diff = adp - ranking) %>%
                       dplyr::arrange(ranking) %>%
                       plyr::dlply('team'),
                     .f = function(x) x %>% dplyr::slice(1:20)) %>%
    dplyr::bind_rows() %>%
    tidyr::unnest() %>% 
    dplyr::arrange(ranking) %>%
    plyr::dlply('pos_list')
  
  data(batprojs_df)
  data(pitchprojs_df)
  
  bat_df <- batprojs_df %>%
    dplyr::filter(year == 2020) %>%
    dplyr::select(-c(player, team, year, pos, pos_list, x2b, x3b, avg, ops, h)) %>%
    dplyr::mutate(e = round(e, 0))
  
  pitch_df <- pitchprojs_df %>%
    dplyr::filter(year == 2020) %>%
    dplyr::select(-c(player, team, year, pos, pos_list, w, l, h, er, bb, cg))
   
  bpos <- pos_[!names(pos_) %in% c('P', 'RP', 'SP')]
  ppos <- pos_[names(pos_) %in% c('P', 'RP', 'SP')]
  
  ppos <- purrr::map(.x = ppos,
                     .f = function(x,y){
                       x %>% 
                         dplyr::left_join(., y, by = 'player_id') %>%
                         dplyr::arrange(desc(rrv)) %>%
                         dplyr::select(-c(player_id, year, pos_list))},
                     y = pitch_df)
  bpos <- purrr::map(.x = bpos,
                     .f = function(x,y){
                       x %>% 
                         dplyr::left_join(., y, by = 'player_id') %>%
                         dplyr::arrange(desc(rrv)) %>%
                         dplyr::filter(rrv > -2) %>%
                         dplyr::select(-c(player_id, year, pos_list))},
                     y = bat_df)
  bpos <- bpos[c('1B', '2B', 'SS', '3B', 'C', 'LF', 'CF', 'RF', 'DH')]
  ppos <- ppos[c('SP', 'RP')]
  
  save(bpos, ppos, team_, full_df,
       file = file.path(getwd(), 'output', 'fordraft.rdata'))
  
  
  
  