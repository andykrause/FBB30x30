
  library(FBB30x30)
  library(tidyverse)

  configs <- setConfigs(nbr_owners = 8,
                        season_year = 2019)

  pv_2019 <- setPlayerValues(configs)


#####
  
  draft_info <- draftSetup(configs,
                           rankings_list = list(pv_2019))
  
  
executeDraft <- function(draft_info){
  
  picked_df <- draft_info$picked[0, ] 
  
  for (pick in draft_info$slot){
  #for (pick in 1:99){  
    # Get team
    i_team <- draft_info$teams[[draft_info$picks[pick]]]
    
    # UPdate ranking rankings
    i_team$rankings <- i_team$rankings %>%
      dplyr::filter(!player_id %in% picked_df$player_id)
    
    # Remove those already picked
    i_pick <- makePick(i_team$strategy,
                       i_team,
                       configs)
    
    ## TODO Need to determine which available spots can take this position!!
    
    i_spot <- which(i_team$roster$roster %in% i_pick$pos1 &
                      is.na(i_team$roster$player_id))[1]
    
    i_team$roster$player_id[i_spot] <- i_pick$player_id[1]
    i_team$roster$round[i_spot] <- length(which(!is.na(i_team$roster$player_id)))
    i_team$roster$pick[i_spot] <- pick
    i_team$roster$rank[i_spot] <- i_pick$ranking[1]
    
    draft_info$teams[[draft_info$picks[pick]]] <- i_team
    
    if (pick == 1){
      picked_df <- data.frame(pick_nbr = 1,
                              round = 1,
                              team = 1,
                              player_id = i_pick$player_id[1])
    } else {
      picked_df <- rbind(picked_df,
                         data.frame(pick_nbr = pick,
                                    round = length(which(!is.na(i_team$roster$player_id))),
                                    team = draft_info$picks[pick],
                                    player_id =  i_pick$player_id[1]))
    }
  }
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

