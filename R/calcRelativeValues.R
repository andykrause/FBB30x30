






#' @export

setRelValue <- function(projs_,
                        stats_,
                        configs){
  
  batrv_df <- setBattingRelValue(projs_$bat, stats_$bat, stats_$field, configs)
  pitrv_df <- setPitchingRelValue(projs_$pitch, stats_$pitch, configs)
  
  batrv_df %>%
    dplyr::bind_rows(., pitrv_df) %>%
    dplyr::arrange(desc(total))
  
}

#' @export
#' 
setBattingRelValue <- function(projs_df,
                               batting_df,
                               fielding_df,
                               configs){
  
  ## Set parameters
  
  bat_per_team <- sum(configs$roster$count[configs$roster$type == 'hit'])
  roster_count <- sum(configs$roster$population[configs$roster$type == 'hit'])
  
  stat_categ <- configs$scoring[configs$scoring$class == 'hit', ] %>%
    dplyr::arrange(stat)
  
  
  ## Prep Statistics
  
  stats_df <- batting_df %>%
    dplyr::filter(year == configs$season - 1)
  
  fieldstats_df <- fielding_df %>%
    dplyr::filter(year == configs$season - 1)
  
  stats_df <- stats_df %>%
    dplyr::left_join(., fieldstats_df %>%
                       dplyr::select(player_id, e),
                     by = 'player_id')
  
  # Limit stats by plate appearances
  pa_limit <- sort(stats_df$pa, decreasing = TRUE)[roster_count * 2]
  stats_df <- stats_df %>%
    dplyr::filter(pa >= pa_limit)
  
  ## Prep Projections
  
  proj_df <- batprojs_df %>%
    dplyr::filter(year == configs$season)
  
  
  ## Find the Relative Values by Stat
  
  # Limit to just roster players and the scored stats
  rstats_df <- stats_df %>%
    dplyr::mutate(ops = obp + slg + (sb/500) + (r/2000)) %>%
    dplyr::arrange(desc(ops)) %>%
    dplyr::slice(1:roster_count)
  rstats_df <- rstats_df[, as.character(stat_categ$stat)]
  
  # Simulate
  sim_hits_ <- purrr::map(.x = as.list(1:1000),
                          .f = simHitSeason,
                          rosterstats_df = rstats_df,
                          configs = configs)
  
  # Calculate relative value statistic contribution levels
  rv_df <- purrr::map(.x = sim_hits_,
                      .f = seasonAnalysis) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(category) %>%
    dplyr::summarize(median = median(diff),
                     mean = mean(diff)) %>%
    dplyr::select(category, mean) %>%
    tidyr::spread(category, mean)
  
  rv_df$hr <- ifelse(rv_df$hr < 8, 8, rv_df$hr)
  rv_df$hr <- ifelse(rv_df$hr > 16, 16, rv_df$hr)
  rv_df$slg <- ifelse(rv_df$slg < .004, .004, rv_df$slg)
  rv_df$gidp <- ifelse(rv_df$gidp < 4, 4, rv_df$gidp)
  rv_df$sb <- ifelse(rv_df$sb > 25, 25, rv_df$sb)
  rv_df$sb <- ifelse(rv_df$sb < 10, 10, rv_df$sb)
  rv_df$rbi <- ifelse(rv_df$rbi < 25, 25, rv_df$rbi)
  rv_df$rbi <- ifelse(rv_df$rbi > 60, 60, rv_df$rbi)
  
  ## Find the Replacement level
  
  rp_df <- purrr::map2(.x = stat_categ %>% plyr::dlply(., 'stat'),
                       .y = stats_df[, as.character(stat_categ$stat)],
                       .f = findReplacementValue,
                       rep_nbr = roster_count) %>% 
    as.data.frame()
  
  ## Calculate player value above replacement
  
  bystat_df <- purrr::map2(.x = proj_df[, sort(as.character(stat_categ$stat))],
                           .y = as.list(as.character(stat_categ$stat)),
                           .f = calRelValue,
                           rp = rp_df,
                           rv = rv_df,
                           configs = configs) %>%
    as.data.frame() %>%
    as.tibble() %>%
    dplyr::mutate(obp = obp / 18,
                  slg = slg / 18)
  bystat_df$total = rowSums(bystat_df)
  
  # Add to players
  var_df <- proj_df %>%
    dplyr::select(player_id, player) %>%
    dplyr::mutate(total = (bystat_df$total - 
                             median(sort(bystat_df$total, 
                                         decreasing=TRUE)[1:(roster_count*2)]))) %>%
    dplyr::arrange(desc(total))
  
  var_df
  
}


#' @export

setPitchingRelValue <- function(projs_df,
                                pitching_df,
                                configs){
  
  ## Set parameters
  
  pitch_per_team <- sum(configs$roster$count[configs$roster$type == 'pitch'])
  roster_count <- sum(configs$roster$population[configs$roster$type == 'pitch'])
  
  stat_categ <- configs$scoring[configs$scoring$class == 'pitch', ] %>%
    dplyr::arrange(stat)
  
  
  ## Prep Statistics
  
  stats_df <- pitching_df %>%
    dplyr::filter(year == configs$season - 1) %>%
    dplyr::mutate(SP = ifelse(gs/g >= .5, T, F))
  
  
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
  
  stats_df <- stats_df %>%
    dplyr::filter((gs >= gs_limit & SP) | (sv >= sv_limit & !SP) | 
                    (holds >= hd_limit & !SP))
  
  ## Prep Projections
  
  proj_df <- pitchprojs_df %>%
    dplyr::filter(year == configs$season)%>%
    dplyr::mutate(SP = ifelse(gs/g >= .5, T, F))
  
  ## Find the Relative Values by Stat
  
  # Create SP Starting universe
  spstats_df <- stats_df %>%
    dplyr::mutate(qsk = qs + k/10) %>%
    dplyr::arrange(desc(qsk)) %>%
    dplyr::slice(1:(round(roster_count * 7/12, 0))) %>%
    dplyr::select(ip, qs, k, whip, sv, holds)
  
  # Create SP Starting universe
  rpstats_df <- stats_df %>%
    dplyr::mutate(svh = sv + holds + k/25) %>%
    dplyr::arrange(desc(svh)) %>%
    dplyr::slice(1:(round(roster_count * 5/12, 0))) %>%
    dplyr::select(ip, qs, k, whip, sv, holds)
  
  rstats_df <- rbind(spstats_df, rpstats_df)
  
  # Simulate
  sim_pitch_ <- purrr::map(.x = as.list(1:1000),
                           .f = simPitchSeason,
                           rosterstats_df = rstats_df,
                           configs = configs)
  
  # Calculate relative value statistic contribution levels
  rv_df <- purrr::map(.x = sim_pitch_,
                      .f = seasonAnalysis) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(category) %>%
    dplyr::summarize(median = median(diff),
                     mean = mean(diff)) %>%
    dplyr::select(category, mean) %>%
    tidyr::spread(category, mean)
  
  rv_df$qs <- ifelse(rv_df$qs < 4, 4, rv_df$qs)
  rv_df$ip <- ifelse(rv_df$ip < 25, 25, rv_df$ip)
  rv_df$k <- ifelse(rv_df$k < 35, 35, rv_df$k)
  rv_df$whip <- ifelse(rv_df$whip < .01, .01, rv_df$whip)
  rv_df$holds <- ifelse(rv_df$holds < 6, 6, rv_df$holds)
  rv_df$sv <- ifelse(rv_df$sv < 10, 10, rv_df$sv)
  
  
  ## Find the Replacement level
  
  # Starting Pitchings
  srp_df <- purrr::map2(.x = stat_categ %>%
                          dplyr::filter(stat %in% c('ip','k','qs', 'whip')) %>%
                          plyr::dlply(., 'stat'),
                        .y = stats_df[stats_df$SP, c('ip','k','qs', 'whip')],
                        .f = findReplacementValue,
                        rep_nbr = round(roster_count * 7/12, 0)) %>% 
    as.data.frame()
  
  # Starting Pitchings
  rrp_df <- purrr::map2(.x = stat_categ %>%
                          dplyr::filter(stat %in% c('holds', 'ip', 'k','sv', 'whip')) %>%
                          plyr::dlply(., 'stat'),
                        .y = stats_df[!stats_df$SP, c('holds', 'ip','k','sv', 'whip')],
                        .f = findReplacementValue,
                        rep_nbr = round(roster_count * 5/12, 0)) %>% 
    as.data.frame()
  
  ## Calculate player value above replacement
  
  sbystat_df <- purrr::map2(.x = proj_df[proj_df$SP, c('ip','k','qs', 'whip')],
                            .y = c('ip','k','qs', 'whip'),
                            .f = calRelValue,
                            rp = srp_df,
                            rv = rv_df,
                            configs = configs) %>%
    as.data.frame() %>%
    as.tibble() %>%
    dplyr::filter(!is.na(ip)) %>%
    dplyr::mutate(whip = whip / 12)
  sbystat_df$total = rowSums(sbystat_df)
  
  rbystat_df <- purrr::map2(.x = proj_df[!proj_df$SP, c('holds', 'ip', 'k','sv', 'whip')],
                            .y = c('holds', 'ip', 'k','sv', 'whip'),
                            .f = calRelValue,
                            rp = rrp_df,
                            rv = rv_df,
                            configs = configs) %>%
    as.data.frame() %>%
    as.tibble() %>%
    dplyr::filter(!is.na(ip)) %>%
    dplyr::mutate(whip = whip / 12)
  rbystat_df$total = rowSums(rbystat_df)
  
  # Add to players
  svar_df <- proj_df %>%
    dplyr::filter(SP)%>%
    dplyr::select(player_id, player) %>%
    dplyr::mutate(total = (sbystat_df$total - 
                             median(sort(sbystat_df$total, 
                                         decreasing=TRUE)[1:round(roster_count*2*7/12, 0)]))) %>%
    dplyr::arrange(desc(total))
  
  # Add to players
  rvar_df <- proj_df %>%
    dplyr::filter(!SP)%>%
    dplyr::select(player_id, player) %>%
    dplyr::mutate(total = (rbystat_df$total - 
                             median(sort(rbystat_df$total, 
                                         decreasing=TRUE)[1:round(roster_count*2*5/12, 0)]))) %>%
    dplyr::arrange(desc(total))
  
  svar_df %>%
    dplyr::bind_rows(., rvar_df) %>%
    dplyr::arrange(desc(total))
  
}









#' @export

simPitchSeason <- function(iter, 
                           rosterstats_df,
                           configs){
  
  # Build Teams
  set.seed(iter)
  rand_id <- rep(1:configs$nbr_owners, 
                 sum(configs$roster$count[configs$roster$type == 'pitch']))
  teams_ <- split(rosterstats_df, rand_id)
  
  teams_ <- lapply(teams_, adjustWhip)
  
  # Sum, flatten, adjust and return
  lapply(teams_, function(x) as.data.frame(t(colSums(x))))%>%
    dplyr::bind_rows() 
}

#' @export
adjustWhip <- function(team_df){
  
  team_df$whip <- (team_df$ip * team_df$whip)/sum(team_df$ip)
  team_df
}

#' @export
adjustObp <- function(team_df){
  
  team_df$obp <- (team_df$pa * team_df$obp)/sum(team_df$pa)
  team_df
}

#' @export
adjustSlg <- function(team_df){
  
  team_df$slg <- (team_df$ab * team_df$slg)/sum(team_df$ab)
  team_df
}

#' @export
simHitSeason <- function(iter, 
                         rosterstats_df,
                         configs){
  
  # Build Teams
  set.seed(iter)
  rand_id <- rep(1:configs$nbr_owners, 
                 sum(configs$roster$count[configs$roster$type == 'hit']))
  teams_ <- split(rosterstats_df, rand_id)
  # 
  # teams_ <- lapply(teams_, adjustObp)
  # teams_ <- lapply(teams_, adjustSlg)
  
  # Sum, flatten, adjust and return
  lapply(teams_, function(x) as.data.frame(t(colSums(x))))%>%
    dplyr::bind_rows() %>%
    dplyr::mutate(obp = obp / 18,
                  slg = slg / 18)
}

## Find the replacement level value for each field
#' @export
findReplacementValue <- function(s_df,
                                 stats_v,
                                 rep_nbr){
  
  if (s_df$category == '-'){
    rep_value <- sort(stats_v)[rep_nbr]
  } else {
    rep_value <- sort(stats_v, decreasing = TRUE)[rep_nbr]
  }
  rep_value
}

# Calculate the relative value of each players performance in a single stat
#' @export
calRelValue <- function(data, name, rp, rv, configs){
  
  score_type <- configs$scoring$category[configs$scoring$stat == name]
  
  if (score_type == '+'){
    return((data - rp[[name]]) / rv[[name]])  
  } else {
    return((rp[[name]] - data) / rv[[name]])
  } 
}











