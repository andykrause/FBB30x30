
#' @export

setConfigs <- function(nbr_owners = 8,
                       season_year = 2019,
                       team_limit = TRUE,
                       ...){
  
  scoring_df <- data.frame(stat = c('r', 'hr', 'rbi', 'so', 'sb', 'slg', 'obp', 'e', 
                                    'gidp', 'ip', 'k', 'qs', 'whip', 'holds', 'sv'),
                           class = c(rep('hit', 9), rep('pitch', 6)),
                           category = c('+','+','+','-','+','+','+','-','-','+','+','+',
                                        '-','+','+'),
                           method = c(rep('count', 5), 'ratio', 'ratio', rep('count', 5),
                                      'ratio', 'count', 'count'))
  
  roster_df <- tibble(roster = c('C', '1B', '2B', 'SS', '3B', 'LF', 'CF', 'RF', 'OF', 
                                 'DH', 'MI', 'CI', 'IF', 'Util', 'SP', 'RP', 'P'),
                      type = c(rep('hit', 14), rep('pitch', 3)),
                      position = list('C', '1B', '2B', 'SS', '3B', 'LF', 'CF', 'RF',
                                      c('CF', 'RF', 'LF'), 'DH', c('2B', 'SS'), 
                                      c('1B', '3B'), c('1B', '2B', '3B', 'SS'),
                                      c('C', '1B', '2B', '3B', 'SS', 'LF','CF', 'RF', 'DH'),
                                      'SP', 'RP', c('SP', 'RP')),
                      count = c(1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3, 5, 4, 3),
                      funnel = c(1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 2, 2, 4, 9, 1, 1, 2),
                      priority = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 3, 4, 1, 1, 2)) %>%
    dplyr::mutate(population = count * nbr_owners)
  
  structure(list(season = season_year,
                 nbr_owners = nbr_owners,
                 roster = roster_df,
                 scoring = scoring_df,
                 team_limit = team_limit),
            class = 'fbbConfigs')
}



