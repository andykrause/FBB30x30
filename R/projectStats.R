#*****************************************************************************************
#
#  Functions to help with projecting statistics
#
#*****************************************************************************************

#' 
#' Project errors
#' 
#' Project errors based on last season and position averages
#' 
#' @param fielding_df Fielding stats (data(fielding_df))
#' @param batprojs_df Batting projections (data(batprojs_df)) 
#' @param proj_year Year of projection
#' @return Data.frame of player_id and projected errors
#' @importFrom dplyr filter select mutate left_join summarize
#' @importFrom tidyr unnest
#' @export

projectErrors <- function(fielding_df,
                          batprojs_df,
                          proj_year){
  
  
  # Estimate games played
  proj_df <- batprojs_df %>%
    dplyr::filter(year == proj_year) %>%
    dplyr::select(player_id, player, pos, ab) %>%
    dplyr::mutate(g = round(ab / 3.9, 0)) %>%
    dplyr::mutate(g = ifelse(g > 162, 162, g)) %>%
    dplyr::select(-ab)
  
  # Extract errors and errors/game from prior year
  error_df <- fielding_df %>% 
    dplyr::filter(year == (proj_year - 1)) %>%
    dplyr::mutate(e_g = e/g) %>%
    dplyr::select(player_id, e, e_g)
  
  # Create a position-wise dataset
  pos_df <- proj_df %>% 
    dplyr::mutate(pos = gsub('IF', '2B', pos),
                  pos = gsub('OF', 'LF', pos)) %>%
    dplyr::left_join(error_df, by= 'player_id') %>%
    dplyr::mutate(pos1 = strsplit(pos, ' | ')) %>%
    tidyr::unnest() %>%
    dplyr::filter(pos1 != '|')
  
  # Calculate avg errors by position
  epos_df <- pos_df %>% 
    dplyr::group_by(pos1) %>%
    dplyr::summarize(pos_e = mean(e_g, na.rm=T)) %>%
    dplyr::bind_rows(data.frame(pos1 = 'DH', pos_e = 0, stringsAsFactors=FALSE))
  
  # Bring all together
  pos_df <- pos_df %>%
    dplyr::left_join(., epos_df, by = 'pos1') %>%
    dplyr::group_by(player_id) %>% 
    dplyr::summarize(pos_e = mean(pos_e, na.rm=T))
  
  proj_df <- proj_df %>% 
    dplyr::left_join(., pos_df, by = 'player_id') %>%
    dplyr::left_join(error_df, by = 'player_id') %>%
    dplyr::mutate(pos_e = ifelse(is.na(pos_e), mean(pos_e, na.rm=T), pos_e))
  
  # Calculate expected errors
  proj_df %>% 
    dplyr::mutate(e_proj = round(g * (pos_e / 3 + (2 * e_g / 3)), 0)) %>%
    dplyr::mutate(e_proj = ifelse(is.na(e_proj), g * pos_e, e_proj)) %>%
    dplyr::select(player_id, e = e_proj)
  
}


#' 
#' Project ground into double plays
#' 
#' Project GIDP based on last season and position averages
#' 
#' @param batting_df Fielding stats (data(fielding_df))
#' @param batprojs_df Batting projections (data(batprojs_df)) 
#' @param proj_year Year of projection
#' @return Data.frame of player_id and projected GIDP
#' @importFrom dplyr filter select mutate left_join summarize
#' @importFrom tidyr unnest
#' @export

projectGIDP <- function(batting_df,
                        batprojs_df,
                        proj_year){
  
  gidp_df <- batting_df %>%
    dplyr::filter(year == proj_year - 1 & 
                    ab > 0) %>%
    dplyr::select(player_id, ab, gidp) %>%
    dplyr::mutate(gidp_ab = gidp / ab)
  
  pos_df <- 
    batprojs_df %>% dplyr::filter(year == proj_year)%>%
    dplyr::select(player_id, pos) %>%
    dplyr::mutate(pos = gsub('IF', '2B', pos),
                  pos = gsub('OF', 'LF', pos)) %>%
    dplyr::left_join(gidp_df, by= 'player_id') %>%
    dplyr::mutate(pos1 = strsplit(pos, ' | ')) %>%
    tidyr::unnest() %>%
    dplyr::filter(pos1 != '|') 
  
  # Calculate avg errors by position
  gpos_df <- pos_df %>% 
    dplyr::group_by(pos1) %>%
    dplyr::summarize(pos_gidp_ab = mean(gidp_ab, na.rm = T))
  
  # Bring all together
  pos_df <- pos_df %>%
    dplyr::left_join(., gpos_df, by = 'pos1') %>%
    dplyr::group_by(player_id) %>% 
    dplyr::summarize(pos_gidp = mean(pos_gidp_ab, na.rm=T))
  
  pos_df <- pos_df %>%
    dplyr::left_join(batprojs_df %>%
                       dplyr::filter(year == proj_year) %>%
                       dplyr::select(player_id, ab),
                     by = 'player_id') %>%
    dplyr::left_join(., gidp_df %>% 
                       dplyr::select(player_id, gidp_ab), 
                     by = 'player_id')
  
  # Calculate expected GIDP
  pos_df %>% 
    dplyr::mutate(gidp_proj = round((ab * pos_gidp / 3) + (2 * ab * gidp_ab / 3), 0)) %>%
    dplyr::mutate(gidp_proj = ifelse(is.na(gidp_proj), round(ab * pos_gidp, 0), gidp_proj)) %>%
    dplyr::select(player_id, gidp = gidp_proj)
  
} 

#' 
#' Project quality starts
#' 
#' Project QS based on last season and simple linear model
#' 
#' @param pitching_df Pitching stats (data(pitching_df))
#' @param pitchprojs_df Pitching projections (data(pitchprojs_df)) 
#' @param proj_year Year of projection
#' @return Data.frame of player_id and projected QS
#' @importFrom dplyr filter select mutate left_join summarize
#' @export

projectQS <- function(pitching_df,
                      pitchprojs_df,
                      proj_year){
  
  # Extract stats
  qs_df <- pitching_df %>%
    dplyr::filter(year == proj_year - 1 & 
                    gs > 0) %>%
    dplyr::select(player_id, gs, whip, k, qs) %>%
    dplyr::mutate(qs_gs = qs / gs)
  
  # Calc predictive model
  pos_model <- lm(qs_gs ~ gs + whip + log(I(k + 1)), data = qs_df)
  
  # Add basic stats to projection universe
  projs_df <- pitchprojs_df %>%
    dplyr::filter(year == proj_year) %>%
    dplyr::select(player_id, gs, whip, k) %>%
    dplyr::left_join(qs_df %>%
                       dplyr::select(player_id, qs_gs),
                     by = 'player_id')
  
  # Estimate via model
  projs_df$qs_gs_model <- predict(pos_model, projs_df)
  
  # Calculate expected GIDP
  projs_df %>% 
    dplyr::mutate(qs_proj = round((gs * qs_gs_model / 3) + (2 * gs * qs_gs / 3), 0)) %>%
    dplyr::mutate(qs_proj = ifelse(is.na(qs_proj), round(gs * qs_gs_model, 0), qs_proj)) %>%
    dplyr::select(player_id, qs = qs_proj)
  
} 

#' 
#' Project holds
#' 
#' Project holds based on last season and simple linear model
#' 
#' @param pitching_df Pitching stats (data(pitching_df))
#' @param pitchprojs_df Pitching projections (data(pitchprojs_df)) 
#' @param proj_year Year of projection
#' @return Data.frame of player_id and projected Holds
#' @importFrom dplyr filter select mutate left_join summarize
#' @export

projectHolds <- function(pitching_df,
                         pitchprojs_df,
                         proj_year){
  
  # Extract stats
  holds_df <- pitching_df %>%
    dplyr::filter(year == proj_year - 1 & 
                    holds > 0) %>%
    dplyr::select(player_id, g, gs, ip, sv, whip, holds) %>%
    dplyr::mutate(holds_gns = holds / (g - gs),
                  gns = g -gs) %>%
    dplyr::filter(gns > 0)
  
  # Calc predictive model
  holds_model <- lm(holds_gns ~ gns + whip + sv + whip + I(ip/gns), 
                    data = holds_df)
  
  # Add basic stats to projection universe
  projs_df <- pitchprojs_df %>%
    dplyr::filter(year == proj_year) %>%
    dplyr::select(player_id, g, gs, ip, sv, whip) %>%
    dplyr::mutate(gns = g - gs) %>%
    dplyr::left_join(holds_df %>%
                       dplyr::select(player_id, holds_gns),
                     by = 'player_id')
  
  # Estimate via model
  projs_df$holds_gns_model <- pmax(predict(holds_model, projs_df), 0)
  projs_df$holds_gns_model <- ifelse(!is.finite(projs_df$holds_gns_model), NA, 
                                     projs_df$holds_gns_model)
  
  # Calculate expected GIDP
  projs_df %>% 
    dplyr::mutate(holds_proj = round((gns * holds_gns_model / 3) + 
                                       (2 * gns * holds_gns / 3), 0)) %>%
    dplyr::mutate(holds_proj = ifelse(is.na(holds_proj), 
                                      round(gns * holds_gns_model, 0), holds_proj)) %>%
    dplyr::mutate(holds_proj = ifelse(is.na(holds_proj), 0, holds_proj)) %>%
    dplyr::select(player_id, holds = holds_proj)
  
} 
