
  library(FBB30x30)
  library(tidyverse)

  configs <- setConfigs(nbr_owners = 8,
                        season_year = 2019)

  pv_2019 <- setPlayerValues(configs, verbose = 9)


#####
  
  draft_info <- draftSetup(configs,
                           rankings_list = list(pv_2019))
  
  draft_obj <- executeDraft(draft_info, verbose = 2)
  
  
executeDraft <- function(draft_info,
                         verbose = 1){
  
  draft_round <- 0

  for (pick in draft_info$slot){
  #for (pick in 1:145){
      
    
    # Update Round
    if (pick %% length(draft_info$teams) == 1){
      draft_round <- draft_round + 1
      if (verbose >= 1) message('\n----------- Round ', draft_round, ' ----------------')
    }
    
    # Get team
    i_team <- draft_info$teams[[draft_info$picks[pick]]]
    
    # UPdate ranking rankings
    i_team$rankings <- i_team$rankings %>%
      dplyr::filter(!player_id %in% draft_info$picked$player_id & 
                      !team %in% i_team$roster$team)
    
    # Remove those already picked
    i_pick <- makePick(i_team$strategy,
                       i_team,
                       configs)

    i_r2p <- i_team$r2p_map %>% 
      dplyr::filter(roster %in% i_team$roster$roster[is.na(i_team$roster$player_id)] & 
                      position %in% i_pick$pos1) %>%
      dplyr::arrange(priority) %>%
      dplyr::slice(1)
    
    i_spot <- which(i_team$roster$roster == i_r2p$roster & 
                      is.na(i_team$roster$player_id))[1]
    
    if (verbose >= 2) message('..', i_pick$player[1], ' - ', i_pick$team[1], ' @ ',
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
                              player_id = i_pick$player_id[1])
    } else {
      draft_info$picked <- rbind(draft_info$picked,
                                 data.frame(pick_nbr = pick,
                                            round = draft_round,
                                            team = draft_info$picks[pick],
                                            player_id =  i_pick$player_id[1]))
    }
    
    # UPdate ranking rankings
    i_team$rankings <- i_team$rankings %>%
      dplyr::filter(!player_id %in% draft_info$picked$player_id & 
                      !team %in% i_team$roster$team)
    
    draft_info$teams[[draft_info$picks[pick]]] <- i_team
    
  }
  
  draft_info
}

makePick <- function(strategy, team_obj, configs){
  
  UseMethod('makePick', strategy)
}

makePick.ba <- function(strategy, team_obj, configs){
 
  avail_pos <- configs$roster %>%
    dplyr::select(roster, position, priority) %>%
    tidyr::unnest() %>%
    dplyr::filter(roster %in% team_obj$roster$roster[is.na(team_obj$roster$player_id)])
    
  rankings <- team_obj$rankings %>%
    dplyr::mutate(pos1 = strsplit(pos, ' | ')) %>%
    tidyr::unnest() %>%
    dplyr::filter(pos1 != '|') %>%
    dplyr::filter(pos1 %in% avail_pos$position) %>%
    dplyr::arrange(desc(value))
  
  pick <- rankings %>% slice(1)
  
  rankings %>%
    dplyr::filter(player_id %in% pick)
}

