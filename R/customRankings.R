#*****************************************************************************************
#
#   Create Custom Player Rankings
#
#*****************************************************************************************

#' 
#' Generic method for custom ranking processes
#' 
#' Create custom rankings based on a variety of methods and means
#' 
#' @param type Type of custom ranking system to use ('rrv' for now)
#' @param configs An `fbbConfigs` object
#' @param verbose [1] The level of reponse statements you'd like (higher = more)
#' @return A custom ranking set
#' @export

customRankings <- function(type,
                           configs,
                           verbose = 1){
  
  type <- structure(type, class = type)
  UseMethod('customRankings', type)  
 
}

#' 
#' Fantasy pros (fp)
#' 
#' Custom rankings from fantasypros.com
#' 
#' @param type Type of custom ranking system to use ('xxx' for now)
#' @param configs An `fbbConfigs` object
#' @param verbose [1] The level of reponse statements you'd like (higher = more)
#' @return A custom ranking set
#' @method customRankings fp
#' @export

customRankings.fp <- function(type,
                              configs, 
                              verbose = 1){
  
  get(data(rankings_df)) %>%
    dplyr::filter(year == configs$season) %>%
    dplyr::filter(team != '')
  
}

#' 
#' Relative rotisserie values (rrv)
#' 
#' Custom rankings that takes into account relative rotisserie point values
#' 
#' @param type Type of custom ranking system to use ('xxx' for now)
#' @param configs An `fbbConfigs` object
#' @param verbose [1] The level of reponse statements you'd like (higher = more)
#' @return A custom ranking set
#' @method customRankings rrv
#' @export

customRankings.rrv <- function(type,
                               configs,
                               verbose = 1){

  ## Build Projs and stats data
  if (verbose >= 1) message('Building raw data objects.\n')
  
  # Projections
  projs_ <- list(bat = get(data(batprojs_df)),
                 pitch = get(data(pitchprojs_df)))
  
  # Stats
  stats_ <- list(bat = get(data(batting_df)),
                 pitch = get(data(pitching_df)),
                 field = get(data(fielding_df)))
  
  ## Build Projs and stats data
  if (verbose >= 1) message('Calculating relative player values.\n')
  rrval_df <- crRelativeRotisserieValue(projs_ = projs_, 
                                        stats_ = stats_, 
                                        configs = configs)

  ## Convert to ranking and return
  rrvToRankings(rrv_obj = rrval_df,
                projs_ = projs_,
                configs = configs)
}

#' 
#' Relative rotisserie values (rrv)
#' 
#' Custom rankings that takes into account relative rotisserie point values
#' 
#' @param type Type of custom ranking system to use ('xxx' for now)
#' @param configs An `fbbConfigs` object
#' @param verbose [1] The level of reponse statements you'd like (higher = more)
#' @return A custom ranking set
#' @method customRankings rrvt
#' @export

customRankings.rrvt <- function(type,
                                configs,
                                verbose = 1){
  
  ## Build Projs and stats data
  if (verbose >= 1) message('Building raw data objects.\n')
  
  # Projections
  projs_ <- list(bat = get(data(batprojs_df)),
                 pitch = get(data(pitchprojs_df)))
  
  # Stats
  stats_ <- list(bat = get(data(batting_df)),
                 pitch = get(data(pitching_df)),
                 field = get(data(fielding_df)))
  
  ## Build Projs and stats data
  if (verbose >= 1) message('Calculating relative player values.\n')
  rrval_df <- crRelativeRotisserieValue(projs_ = projs_, 
                                        stats_ = stats_, 
                                        configs = configs)
  
  # if (verbose >= 1) message('Adjusting for team quality)
  rrvt_df <- crRelativeTeamAdj(rrv_df = rrval_df,
                               configs = configs)

  ## Convert to ranking and return
  rrvToRankings(rrv_obj = rrval_df,
                projs_ = projs_,
                configs = configs,
                tv_obj = rrvt_df)
}

#' 
#' Relative rotisserie values (rrv)
#' 
#' Custom rankings that takes into account relative rotisserie point values
#' 
#' @param type Type of custom ranking system to use ('xxx' for now)
#' @param configs An `fbbConfigs` object
#' @param verbose [1] The level of reponse statements you'd like (higher = more)
#' @return A custom ranking set
#' @method customRankings rrvp
#' @export

customRankings.rrvp <- function(type,
                                configs,
                                verbose = 1){
  
  ## Build Projs and stats data
  if (verbose >= 1) message('Building raw data objects.\n')
  
  # Projections
  projs_ <- list(bat = get(data(batprojs_df)),
                 pitch = get(data(pitchprojs_df)))
  
  # Stats
  stats_ <- list(bat = get(data(batting_df)),
                 pitch = get(data(pitching_df)),
                 field = get(data(fielding_df)))
  
  ## Build Projs and stats data
  if (verbose >= 1) message('Calculating relative player values.\n')
  rrval_df <- crRelativeRotisserieValue(projs_ = projs_, 
                                        stats_ = stats_, 
                                        configs = configs)
  
  # if (verbose >= 1) message('Adjusting for team quality)
  rrvp_df <- crRelativePosAdj(rrv_df = rrval_df,
                              configs = configs)
  
  ## Convert to ranking and return
  rrvToRankings(rrv_obj = rrval_df,
                projs_ = projs_,
                configs = configs,
                pv_obj = rrvp_df)
  
}

#' 
#' Relative rotisserie values (rrv)
#' 
#' Custom rankings that takes into account relative rotisserie point values
#' 
#' @param type Type of custom ranking system to use ('xxx' for now)
#' @param configs An `fbbConfigs` object
#' @param verbose [1] The level of reponse statements you'd like (higher = more)
#' @return A custom ranking set
#' @method customRankings rrvpt
#' @export

customRankings.rrvpt <- function(type,
                                 configs,
                                 verbose = 1){
  
  ## Build Projs and stats data
  if (verbose >= 1) message('Building raw data objects.\n')
  
  # Projections
  projs_ <- list(bat = get(data(batprojs_df)),
                 pitch = get(data(pitchprojs_df)))
  
  # Stats
  stats_ <- list(bat = get(data(batting_df)),
                 pitch = get(data(pitching_df)),
                 field = get(data(fielding_df)))
  
  ## Build Projs and stats data
  if (verbose >= 1) message('Calculating relative player values.\n')
  rrval_df <- crRelativeRotisserieValue(projs_ = projs_, 
                                        stats_ = stats_, 
                                        configs = configs)
  
  # if (verbose >= 1) message('Adjusting for team quality)
  rrvp_df <- crRelativePosAdj(rrv_df = rrval_df,
                              configs = configs)
  
  # if (verbose >= 1) message('Adjusting for team quality)
  rrvt_df <- crRelativeTeamAdj(rrv_df = rrval_df,
                              configs = configs)
  
  ## Convert to ranking and return
  rrvToRankings(rrv_obj = rrval_df,
                projs_ = projs_,
                configs = configs,
                pv_obj = rrvp_df,
                tv_obj = rrvt_df)
  
}

#' 
#' Calculate relative rotisserie values
#' 
#' Custom player values based on relative rotisserie point values
#' 
#' @param projs_ List of batting and pitching projections
#' @param stats_ List of batting, fielding and pitching statistics
#' @param configs An `fbbConfigs` object
#' @return An RRV object with custom player values
#' @importFrom dplyr bind_rows arrange
#' @export

crRelativeRotisserieValue <- function(projs_,
                                      stats_,
                                      configs){
  
  # Calc batting RRVs
  batrv_df <- crRelativeBattingValue(projs_df = projs_$bat, 
                                     batting_df = stats_$bat,
                                     fielding_df = stats_$field, 
                                     configs = configs)
  
  # Calculate pitching RRVs
  pitrv_df <- crRelativePitchingValue(projs_df = projs_$pitch, 
                                      pitching_df = stats_$pitch, 
                                      configs = configs)

  # Bind all together and retrun
  structure(batrv_df %>%
    dplyr::bind_rows(., pitrv_df) %>%
    dplyr::arrange(desc(total)) %>%
    dplyr::rename(rrv = total),
    class = c('rrv', 'tbl_df', 'tbl', 'data.frame'))
  
}

#' 
#' Calculate relative rotisserie values for batters
#' 
#' Custom batter values based on relative rotisserie point values
#' 
#' @param projs_df Data.frame of batting projections
#' @param batting_df Data.frame of batting stats
#' @param fielding_df Data.frame of fielding stats
#' @param configs An `fbbConfigs` object
#' @return An RRV object with custom batter values
#' @importFrom dplyr bind_rows arrange filter left_join select mutate slice group_by
#' @importFrom dplyr summarize
#' @importFrom purrr map map2
#' @importFrom tidyr spread
#' @importFrom stats median
#' @importFrom plyr dlply
#' @importFrom tibble as.tibble
#' @export

crRelativeBattingValue <- function(projs_df,
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
  
  # Limit to just likely rostered players and the scored stats
  rstats_df <- stats_df %>%
    dplyr::mutate(ops = obp + slg + (sb/500) + (r/2000)) %>%
    dplyr::arrange(desc(ops)) %>%
    dplyr::slice(1:roster_count)
  rstats_df <- rstats_df[, as.character(stat_categ$stat)]
  
  # Simulate a bunch of seasons to determine the potential relative point values
  sim_hits_ <- purrr::map(.x = as.list(1:100),
                          .f = simBattingSeason,
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
    tidyr::spread(category, mean) %>%
    crExpertRVCorrections(., type = 'hit')

  ## Find the Replacement level
  
  rp_df <- purrr::map2(.x = stat_categ %>% plyr::dlply(., 'stat'),
                       .y = stats_df[, as.character(stat_categ$stat)],
                       .f = findReplacementValue,
                       rep_nbr = roster_count) %>% 
    as.data.frame()
  
  ## Calculate player value above replacement
  
  bystat_df <- purrr::map2(.x = proj_df[, sort(as.character(stat_categ$stat))],
                           .y = as.list(as.character(stat_categ$stat)),
                           .f = calcRelativeRotisserieValue,
                           rp = rp_df,
                           rv = rv_df,
                           configs = configs) %>%
    as.data.frame() %>%
    tibble::as.tibble() %>%
    dplyr::mutate(obp = obp / bat_per_team * (proj_df$ab/500),
                  slg = slg / bat_per_team * (proj_df$ab/500))
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

#' 
#' Calculate relative rotisserie values for pitchers
#' 
#' Custom pitcher values based on relative rotisserie point values
#' 
#' @param projs_df Data.frame of batting projections
#' @param pitching_df Data.frame of pitching stats
#' @param configs An `fbbConfigs` object
#' @return An RRV object with custom pitcher values
#' @importFrom dplyr bind_rows arrange filter left_join select mutate slice group_by
#' @importFrom dplyr summarize 
#' @importFrom purrr map map2
#' @importFrom tidyr spread
#' @importFrom stats median
#' @importFrom plyr dlply
#' @importFrom tibble as.tibble
#' @export

crRelativePitchingValue <- function(projs_df,
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
  
  ## Find the Relative Values by Stat
  
  # Create SP Starting universe
  spstats_df <- stats_df %>%
    dplyr::mutate(qsk = qs + k / 10) %>%
    dplyr::arrange(desc(qsk)) %>%
    dplyr::slice(1:(round(roster_count * 7/12, 0))) %>%
    dplyr::select(ip, qs, k, whip, sv, holds)
  
  # Create SP Starting universe
  rpstats_df <- stats_df %>%
    dplyr::mutate(svh = sv + holds + k / 25) %>%
    dplyr::arrange(desc(svh)) %>%
    dplyr::slice(1:(round(roster_count * 5/12, 0))) %>%
    dplyr::select(ip, qs, k, whip, sv, holds)
  
  rstats_df <- rbind(spstats_df, rpstats_df)
  
  # Simulate
  sim_pitch_ <- purrr::map(.x = as.list(1:100),
                           .f = simPitchingSeason,
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
    tidyr::spread(category, mean)%>%
    crExpertRVCorrections(., type = 'pitch')
 
  ## Find the Replacement level
  
  # Starting Pitchings
  srp_df <- purrr::map2(.x = stat_categ %>%
                          dplyr::filter(stat %in% c('ip','k','qs', 'whip')) %>%
                          plyr::dlply(., 'stat'),
                        .y = stats_df[stats_df$SP, c('ip','k','qs', 'whip')],
                        .f = findReplacementValue,
                        rep_nbr = round(roster_count * 7/12, 0)) %>% 
    as.data.frame()
  
  # Relief Pitchings
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
                            .f = calcRelativeRotisserieValue,
                            rp = srp_df,
                            rv = rv_df,
                            configs = configs) %>%
    as.data.frame() %>%
    as.tibble() %>%
    dplyr::filter(!is.na(ip)) %>%
    dplyr::mutate(whip = whip / 12* (ip/200))
  sbystat_df$total = rowSums(sbystat_df)
  
  rbystat_df <- purrr::map2(.x = proj_df[!proj_df$SP, c('holds', 'ip', 'k','sv', 'whip')],
                            .y = c('holds', 'ip', 'k','sv', 'whip'),
                            .f = calcRelativeRotisserieValue,
                            rp = rrp_df,
                            rv = rv_df,
                            configs = configs) %>%
    as.data.frame() %>%
    as.tibble() %>%
    dplyr::filter(!is.na(ip)) %>%
    dplyr::mutate(whip = whip / 12 * (ip/200))
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
 
#' 
#' Expert correction of rv values from simulations
#' 
#' As simulations results can be extreme (usually on the small end), this makes some
#' corrections to ensure the values are within reasonable ranges
#' 
#' @param type 'hit' or 'pitch'
#' @param rv_df The raw category values from `seasonAnalysis`
#' @return An corrected rv data.frame
#' @export

crExpertRVCorrections <- function(type, 
                                  rv_df){
  
  if (type == 'hit'){
    rv_df$hr <- ifelse(rv_df$hr < 8, 8, rv_df$hr)
    rv_df$hr <- ifelse(rv_df$hr > 16, 16, rv_df$hr)
    rv_df$slg <- ifelse(rv_df$slg < .004, .004, rv_df$slg)
    rv_df$gidp <- ifelse(rv_df$gidp < 4, 4, rv_df$gidp)
    rv_df$sb <- ifelse(rv_df$sb > 25, 25, rv_df$sb)
    rv_df$sb <- ifelse(rv_df$sb < 10, 10, rv_df$sb)
    rv_df$rbi <- ifelse(rv_df$rbi < 25, 25, rv_df$rbi)
    rv_df$rbi <- ifelse(rv_df$rbi > 60, 60, rv_df$rbi)
  }
  if (type == 'pitch'){
    rv_df$qs <- ifelse(rv_df$qs < 4, 4, rv_df$qs)
    rv_df$ip <- ifelse(rv_df$ip < 25, 25, rv_df$ip)
    rv_df$k <- ifelse(rv_df$k < 35, 35, rv_df$k)
    rv_df$whip <- ifelse(rv_df$whip < .01, .01, rv_df$whip)
    rv_df$holds <- ifelse(rv_df$holds < 6, 6, rv_df$holds)
    rv_df$sv <- ifelse(rv_df$sv < 10, 10, rv_df$sv)
  }
  rv_df
}

#' 
#' Find replacement level value for a given category
#' 
#' Find the nth value of a category where that Nth level is considered the replacement 
#' player value
#' 
#' @param s_df stat information data.frame
#' @param stats_v A vector of the stat category values
#' @param rep_nbr THe number of the replacement players
#' @return A scalar value representing the replacement value of that stat
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

#' 
#' Calculate relative rotisserie value for a stat
#' 
#'  Given the replacement level and the relative worth per category increase, 
#'  calculate the relative value of each of the stats/projections
#' 
#' @param data Vector of stats or projections
#' @param name Name of stat
#' @param rp Replacement level value data.frame
#' @param rv Relative point value per stat
#' @param configs An `fbbConfigs` object
#' @return Vector of relative values for a stat
#' @export

calcRelativeRotisserieValue <- function(data, name, rp, rv, configs){
  
  score_type <- configs$scoring$category[configs$scoring$stat == name]
  
  if (score_type == '+'){
    return((data - rp[[name]]) / rv[[name]])  
  } else {
    return((rp[[name]] - data) / rv[[name]])
  } 
}

#' 
#' Simulate a batting season
#' 
#' Sum up all batting stats or projections to get total rotisserie raw category counts and
#' ratios
#' 
#' @param iter Iteration number of the simulation
#' @param rosterstats_df Individual player stats or projections
#' @param configs An `fbbConfigs` object
#' @return Summed total of hitting statistics/projections
#' @importFrom dplyr bind_rows mutate
#' @export
#' 

simBattingSeason <- function(iter, 
                             rosterstats_df,
                             configs){
  
  # Build Teams
  set.seed(iter)
  rand_id <- rep(1:configs$nbr_owners, 
                 sum(configs$roster$count[configs$roster$type == 'hit']))
  teams_ <- split(rosterstats_df, rand_id[order(runif(length(rand_id), 0 ,1))])

  # Sum, flatten, adjust and return
  lapply(teams_, function(x) as.data.frame(t(colSums(x))))%>%
    dplyr::bind_rows() %>%
    dplyr::mutate(obp = obp / sum(configs$roster$count[
                    configs$roster$type == 'hit']),
                  slg = slg / sum(configs$roster$count[
                    configs$roster$type == 'hit']))
}

#' 
#' Simulate a pitching season
#' 
#' Sum up all pitching stats or projections to get total rotisserie raw category counts and
#' ratios
#' 
#' @param iter Iteration number of the simulation
#' @param rosterstats_df Individual player stats or projections
#' @param configs An `fbbConfigs` object
#' @return Summed total of hitting statistics/projections
#' @importFrom dplyr bind_rows 
#' @export

simPitchingSeason <- function(iter, 
                              rosterstats_df,
                              configs){
  
  # Build Teams
  set.seed(iter)
  rand_id <- rep(1:configs$nbr_owners, 
                 sum(configs$roster$count[configs$roster$type == 'pitch']))
  teams_ <- split(rosterstats_df, rand_id[order(runif(length(rand_id), 0 ,1))])
  teams_ <- lapply(teams_, adjustWhip)
  
  # Sum, flatten, adjust and return
  lapply(teams_, function(x) as.data.frame(t(colSums(x))))%>%
    dplyr::bind_rows() 
}

#' 
#' Adjust whip values by team numbers
#' 
#' @param team_df Team of pitching stats
#' @return Adjusted WHIP values
#' @export

adjustWhip <- function(team_df){
  
  team_df$whip <- (team_df$ip * team_df$whip)/sum(team_df$ip)
  team_df
}

#' 
#' Convert RRV data (and other relative measure) to a set of rankings
#' 
#' @param rrv_obj RRV data from crRelativeRotisserieValue()
#' @param projs_ Projection list of bat and pitch projections
#' @param configs an `fbbConfigs` object
#' @return A 'draftRankings' object
#' @export

rrvToRankings <- function(rrv_obj,
                          projs_,
                          configs,
                          ...){
  
  cr_df <- projs_$bat %>%
    dplyr::select(player_id, player, team, year, pos, pos_list) %>%
    dplyr::filter(year == configs$season) %>%
    dplyr::bind_rows(., projs_$pitch %>%
                       dplyr::select(player_id, player, team, year,
                                     pos, pos_list) %>%
                       dplyr::filter(year == configs$season)) %>%
    dplyr::left_join(., rrv_obj %>%
                       dplyr::select(-player),
                     by='player_id')
  
  if ('tv_obj' %in% names(list(...))){
    cr_df <- cr_df %>%
      dplyr::left_join(., list(...)$tv_obj, by = 'team') %>% 
      dplyr::rename(rrv_origt = rrv) %>%
      dplyr::mutate(rrv = rrv_origt + team_adj)
  }
  
  if ('pv_obj' %in% names(list(...))){
    
    cr_df <- cr_df %>%
      tidyr::unnest() %>%
      dplyr::left_join(., list(...)$pv_obj, by = 'pos_list') %>%
      dplyr::rename(rrv_origp = rrv) %>%
      dplyr::mutate(rrv = rrv_origp + pos_adj) %>%
      dplyr::group_by(player_id) %>%
      dplyr::mutate(rrv = max(rrv)) %>%
      dplyr::ungroup() %>%
      tidyr::nest(pos_list, pos_adj, .key = 'pos_list') 
  }
  
  if ('rv_obj' %in% names(list(...))){
    ## NEEDS WORK
    # cr_df <- cr_df %>%
    #   dplyr::left_join(., pv_obj, by = 'team')
    # Do a mutate to add value to existing
  }
  
  structure(cr_df %>%
              dplyr::arrange(desc(rrv)) %>%
              dplyr::distinct(player_id, .keep_all = TRUE) %>%
              dplyr::mutate(ranking = as.numeric(rownames(.))) %>%
              dplyr::filter(team != '') %>%
              dplyr::select(player_id, player, team, year, pos, pos_list, rrv, ranking,
                            tidyselect::everything()),
            class = c('draftRankings', 'tbl_df', 'tbl', 'data.frame'),
            team_values = list(...)$tv_obj,
            pos_values = list(...)$pv_obj,
            roster_values = list(...)$rv_obj)
}


#' @export

crRelativeTeamAdj <- function(rrv_df,
                              configs){  
  
  projs_udf <- get(data(batprojs_df)) %>%
    dplyr::select(player_id, player, team, year, pos) %>%
    dplyr::filter(year == configs$season) %>%
    dplyr::bind_rows(., get(data(pitchprojs_df)) %>%
                       dplyr::select(player_id, player, team, year, pos) %>%
                       dplyr::filter(year == configs$season)) %>%
    dplyr::left_join(., rrv_df %>%
                       dplyr::select(-player), 
                     by='player_id') %>%
    dplyr::filter(team != '')
  
  projs_udf %>%
    dplyr::group_by(team) %>%
    dplyr::arrange(desc(rrv)) %>%
    dplyr::slice(configs$nbr_owner) %>%
    dplyr::mutate(team_adj = -rrv) %>%
    dplyr::select(team, team_adj) %>%
    dplyr::arrange(team_adj)

}

#' @export

crRelativePosAdj <- function(rrv_df,
                             configs){  
  
  projs_udf <- get(data(batprojs_df)) %>%
    dplyr::select(player_id, player, team, year, pos, pos_list) %>%
    dplyr::filter(year == configs$season) %>%
    dplyr::bind_rows(., get(data(pitchprojs_df)) %>%
                       dplyr::select(player_id, player, team, year, pos, pos_list) %>%
                       dplyr::filter(year == configs$season)) %>%
    dplyr::left_join(., rrv_df %>%
                       dplyr::select(-player), 
                     by='player_id') %>%
    dplyr::filter(team != '') %>%
    tidyr::unnest()
  
  poscount_df <- configs$roster %>%
    dplyr::filter(priority == 1) %>%
    dplyr::mutate(pos_count = paste0(roster, "_", population))
  
  projs_udf %>%
    dplyr::group_by(pos_list) %>%
    dplyr::arrange(desc(rrv)) %>%
    dplyr::mutate(count = 1:n(),
                  pos_count = paste0(pos_list, "_", count)) %>%
    dplyr::filter(pos_count %in% poscount_df$pos_count) %>%
    dplyr::select(pos_list, pos_adj = rrv) %>%
    dplyr::mutate(pos_adj = -pos_adj) %>%
    dplyr::arrange(pos_adj)
  
}


