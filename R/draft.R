
#' @export

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
    tidyr::unnest() %>%
    dplyr::mutate(filled = FALSE)
  
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
  teams_ <- list()
  for(ti in 1:configs$nbr_owners){
    
    if (length(rankings_list) == 1){
      rankings = rankings_list[[1]]
    } else {
      rankings <- ranking_list[[ti]]
    }
    
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
                        player_id = 'x')
  
  structure(list(picks = draft_id,
                 slot = draft_pos,
                 teams = teams_,
                 picked = pick_df[0, ]),
            class = 'draftInfo')
}