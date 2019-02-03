#*****************************************************************************************
#
#   Draft Teams
#
#*****************************************************************************************



#' @export
#' 
draftSetup <- function(configs,
                       rankings_list,
                       team_strategy = rep('ba', configs$nbr_owners),
                       ...){
  
  # Create roster spots
  roster_spots <- purrr::map2(.x = configs$roster$roster,
                              .y = configs$roster$count,
                              .f = rep) %>% unlist()
  
  # Roster to position mapping
  r2p_map <- configs$roster %>%
    dplyr::select(roster, position, priority) %>%
    tidyr::unnest() 
  
  # Set draft order and position
  draft_id <- rep(c(1:configs$nbr_owners, configs$nbr_owners:1),
                  ceiling(length(roster_spots) / 2))
  draft_pos <- 1:length(draft_id)
  
  ## Set up teams
  blank_roster <- data.frame(roster = roster_spots,
                             player_id = NA,
                             team = NA,
                             rank = 0,
                             round = 0,
                             pick = 0)
  
  ## Add rankings list
  teams_ <- list()
  for(ti in 1:configs$nbr_owners){
    
    if (length(rankings_list) == 1){
      rankings = rankings_list[[1]]
    } else {
      rankings <- rankings_list[[ti]]
    }
    
    # Prepare rankings object
    rankings <- rankings %>%
      tidyr::unnest() %>%
      dplyr::left_join(., r2p_map, by = c('pos_list' = 'position')) %>%
      dplyr::distinct(player_id, roster, .keep_all = TRUE) %>%
      dplyr::rename(position = pos_list) %>%
      tidyr::nest(roster, position, priority, .key = 'roster_link') %>%
      dplyr::select(player_id, player, year, team, pos, roster_link, 
                    tidyselect::everything())
    
    # Add all into a team object
    teams_[[ti]] <- structure(list(roster = blank_roster,
                                   r2p_map = r2p_map,
                                   rankings = rankings,
                                   strategy = structure(team_strategy[ti], 
                                                        class = team_strategy[ti]),
                                   class = 'teamDraftInfo'))
  }  
  
  ## Set up picks
  pick_df <- data.frame(pick_nbr = 0,
                        round = 0,
                        team = 0,
                        player_id = 'x',
                        ranking = 0,
                        strategy = 'x')
  
  structure(list(picks = draft_id,
                 slot = draft_pos,
                 teams = teams_,
                 picked = pick_df[0, ]),
            class = 'draftInfo')
}

#' @export

draftTeams <- function(configs,
                       verbose = 1,
                       ...){

  rankings_list <- buildRankingsList(configs,
                                    verbose = verbose,
                                    ...)
  
  draft_info <- draftSetup(configs,
                           rankings_list = rankings_list)
  
  executeDraft(draft_info = draft_info,
               verbose = verbose)
}

#' @export

buildRankingsList <- function(configs,
                              verbose = 1,
                              ...){
  
  if ('rankings_' %in% names(list(...))){
    rankings_ <- list(...)$rankings_
  } else {
    rankings_ <- purrr::map(.x = unique(configs$behavior$rankings),
                            .f = customRankings,
                            configs = configs) %>%
      purrr::set_names(unique(configs$behavior$rankings))
  }
  rankings_[configs$behavior$rankings]
  
}



#' @export

executeDraft <- function(draft_info,
                         verbose = 1){
  
  draft_round <- 0
  
  for (pick in draft_info$slot){
    #for (pick in 1:85){
    
    
    # Update Round
    if (pick %% length(draft_info$teams) == 1){
      draft_round <- draft_round + 1
      if (verbose >= 1) message('\n----------- Round ', draft_round, ' ----------------')
    }
    
    # Get team
    i_team <- draft_info$teams[[draft_info$picks[pick]]]
    
    # UPdate ranking rankings
    i_team$rankings <- draftUpdateRankings(rankings_df = i_team$rankings,
                                           picked_df = draft_info$picked,
                                           roster_df = i_team$roster,
                                           configs = configs)
    
    # Make the pick
    i_pick <- makePick(i_team$strategy,
                       i_team,
                       configs)
    
    # Hack for when no pick is maed
    if (nrow(i_pick) == 0) i_pick <- draft_info$picked[nrow(draft_info$picked), ]
    
    # Find roster spot
    i_spot <- which(i_team$roster$roster == i_pick$roster & 
                      is.na(i_team$roster$player_id))[1]
    
    if (verbose >= 2) message('..', i_pick$player, ' - ', i_pick$team, ' @ ',
                              i_team$roster$roster[i_spot])
    
    i_team$roster$player_id[i_spot] <- i_pick$player_id[1]
    i_team$roster$round[i_spot] <- draft_round
    i_team$roster$pick[i_spot] <- pick
    i_team$roster$rank[i_spot] <- i_pick$ranking[1]
    i_team$roster$team[i_spot] <- i_pick$team[1]
    
    if (pick == 1){
      draft_info$picked <- data.frame(pick_nbr = 1,
                                      round = 1,
                                      team = 1,
                                      player_id = i_pick$player_id,
                                      ranking = i_pick$ranking,
                                      strategy = as.character(i_team$strategy))
    } else {
      draft_info$picked <- rbind(draft_info$picked,
                                 data.frame(pick_nbr = pick,
                                            round = draft_round,
                                            team = draft_info$picks[pick],
                                            player_id =  i_pick$player_id,
                                            ranking = i_pick$ranking,
                                            strategy = as.character(i_team$strategy)))
    }
    
    # UPdate ranking rankings
    i_team$rankings <- draftUpdateRankings(rankings_df = i_team$rankings,
                                           picked_df = draft_info$picked,
                                           roster_df = i_team$roster,
                                           configs = configs)
    
    # Add team back to draft info object
    draft_info$teams[[draft_info$picks[pick]]] <- i_team
    
  }
  
  draft_info
}


#' 
#' Update a teams available rankings
#' 
#' @param rankings_df The rankings data
#' @param picked_df Data on those players already picked
#' @param roster_df The teams current roster
#' @param configs An `fbbConfigs` object
#' @return A custom ranking set
#' @importFrom dplyr filter
#' @export

draftUpdateRankings <- function(rankings_df,
                                picked_df,
                                roster_df,
                                configs){
  
  r_df <- rankings_df %>%
    dplyr::filter(!player_id %in% picked_df$player_id)
  
  if (configs$team_limit){
    r_df <- r_df %>% 
      dplyr::filter(!team %in% roster_df$team)
  }
  
  r_df
}

#' @export

makePick <- function(strategy, team_obj, configs){
  
  UseMethod('makePick', strategy)
}

#' @export

makePick.ba <- function(strategy, team_obj, configs){
  
  # Get available positions
  avail_pos <- configs$roster %>%
    dplyr::select(roster, position, priority) %>%
    tidyr::unnest() %>%
    dplyr::filter(roster %in% team_obj$roster$roster[is.na(team_obj$roster$player_id)])
  
  # Make pick and return
  structure(team_obj$rankings %>%
      tidyr::unnest() %>%
      dplyr::filter(roster %in% avail_pos$roster) %>%
      dplyr::arrange(ranking, priority) %>%
      dplyr::slice(1) %>%
      dplyr::select(player_id, player, team, roster, ranking),
    class = c('draftPick', 'tbl_df', 'tbl', 'data.frame'))
  
}

