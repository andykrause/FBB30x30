
library(FBB30x30)
library(tidyverse)


## Make OBP and SLG adjustments
## Document functions
## Pass rv, rp etc. up through results
## Make constraints for all RV values


  setConfigs <- function(nbr_owners = 8,
                       season_year = 2019,
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
                      count = c(2, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 2, 5, 4, 3),
                      funnel = c(1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 2, 2, 4, 9, 1, 1, 2),
                      priority = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 3, 4, 1, 1, 2)) %>%
    dplyr::mutate(population = count * nbr_owners)
  
  structure(list(season = season_year,
                 nbr_owners = nbr_owners,
                 roster = roster_df,
                 scoring = scoring_df),
            class = 'fbbConfigs')
}

  configs <- setConfigs(season_year = 2019)


  projs_ <- list(bat = batprojs_df,
                pitch = pitchprojs_df)
  stats_ <- list(bat = batting_df,
                 pitch = pitching_df,
                 field = fielding_df)

  relval_df <- setRelValue(projs_, stats_, configs)
  relval_df

  
### --- Working
  
setPosValueAdj <- function(projs_,
                           rv_df,
                           configs){  
  
  # Combine into a single data.frame unnested by positions
  
  projs_udf <- projs_$bat %>%
    dplyr::select(player_id, player, team, year, pos) %>%
    dplyr::filter(year == configs$season) %>%
    dplyr::bind_rows(., projs_$pitch %>%
                       dplyr::select(player_id, player, team, year, pos) %>%
                       dplyr::filter(year == configs$season)) %>%
    dplyr::left_join(rv_df %>%
                       dplyr::select(-player), 
                     by = 'player_id') %>%
    dplyr::mutate(pos1 = strsplit(pos, ' | ')) %>%
    tidyr::unnest() %>%
    dplyr::filter(pos1 != '|')

 hitp1_df <- assignPositionPriority(type = 'hit',
                                    priority = 1,
                                    projs_udf = projs_udf,
                                    configs = configs)
 
 
}
 

assignPositionPriority <- function(type,
                                   priority,
                                   projs_udf,
                                   used_ids = NULL,
                                   configs){
  
   # Extract roster info
   ros_info <- configs$roster[configs$roster$priority == priority & 
                                configs$roster$type == type, ]
   
   pos_df <- ros_info %>%
     dplyr::select(roster, position) %>%
     tidyr::unnest()
   
   # Extact possible players
   player_df <- projs_udf %>%
     dplyr::filter(pos1 %in% pos_df$position)
   
   if (priority > 1){
     player_df <- player_df %>%
       dplyr::left_join(., pos_df %>%
                          select(pos1 = position,
                                 posx = roster))
   } else {
     player_df$posx <- player_df$pos1
     player_df <- player_df %>%
       dplyr::filter(!player_id %in% used_ids)
   }
   
  ## Assign to likely drafted position
  
   # Setup - Calculate starting pos means
   p_ <- list()
   
   for (i in 1:nrow(ros_info)){
     
     i_pos <- unlist(pos_df$position[pos_df$roster == ros_info$roster[i]])
     
     p_[[i]] <- player_df %>%
       dplyr::filter(pos1 %in% i_pos) %>%
       dplyr::arrange(desc(total)) 
   
     p_[[i]] <- p_[[i]] %>%
       dplyr::mutate(pos_rank = 1:nrow(p_[[i]]),
                     pos_mean = mean(p_[[i]]$total[1:ros_info$population[i]]))
   }
   
   player_df <- p_ %>% 
     dplyr::bind_rows() %>%
     dplyr::arrange(pos_rank, pos_mean) %>%
     dplyr::distinct(player_id, .keep_all = TRUE)
 
   assign_df <- positionAssign(player_df,
                               configs)
   
   assign_df
   
}

  ## Make assignment
   
  positionAssign <- function(player_df, 
                             configs){
    
    # Setup
    go <- 1
    pa_ <- list()
    all_pos <- unique(player_df$posx)
    
    # Make Assignments
    while (go == 1){
      
      p_df <- player_df[1, ]
      cat(p_df$player, '...\n')
      roster_pop <- configs$roster$population[which(configs$roster$roster == p_df$posx)]  
      
      if (is.null(pa_[[p_df$posx]])){
        pa_[[p_df$posx]] <- p_df
        player_df <- player_df %>% 
          dplyr::filter(!player_id %in% p_df$player_id)
     
      } else {
     
        if (nrow(pa_[[p_df$posx]]) == roster_pop){
          player_df <- player_df %>% 
            dplyr::filter(!posx %in% p_df$posx) 
        } else {
           pa_[[p_df$posx]] <- rbind(pa_[[p_df$posx]], p_df)
           player_df <- player_df %>% 
             dplyr::filter(!player_id %in% p_df$player_id)
        }
      }
   
     ## Recalculate pos_mean and re-arrange
     pm_df <- player_df %>%
       dplyr::filter(posx %in% p_df$posx) %>%
       dplyr::slice(1:roster_pop) %>%
       dplyr::summarize(pos_mean = mean(total))
   
     player_df$pos_mean[player_df$posx == p_df$pos] <- pm_df$pos_mean
     player_df <- player_df %>%
       dplyr::arrange(pos_rank, pos_mean)
   
   if (nrow(dplyr::bind_rows(pa_)) == 
       sum(configs$roster$population[which(configs$roster$roster %in% all_pos)])) go <- 0
 }
 
   pa_df <- pa_ %>% dplyr::bind_rows()
 
   pav_df <- pa_df %>%
     dplyr::group_by(posx) %>%
     dplyr::summarize(min = -min(total))
 
   pa_df %>%
     dplyr::left_join(., pav_df,
                      by = 'posx') %>%
     dplyr::rename(pos_adj = min) %>%
     dplyr::mutate(priority = priority)
   
  }
  
  
  
  
 ## Next prioerity level ------------------------------------------------------------

 p2h_ros <- configs$roster %>%
   dplyr::filter(priority == 2 & type == 'hit')
 p2hpos <- unique(unlist(p2h_ros$position))
 
 p2h_map <- p2h_ros %>%
   dplyr::select(roster, position) %>%
   tidyr::unnest()
 
 p2h_df <- projs %>%
   dplyr::filter(pos1 %in% p2hpos) %>%
   dplyr::filter(!player_id %in% p1hf_df$player_id) %>%
   dplyr::left_join(., p2h_map %>% 
                      dplyr::select(pos1 = position, pos2 = roster),
                    by = 'pos1') %>%
   dplyr::distinct(player_id, pos2, .keep_all = TRUE)
 
 ## Recalculate pos_mean and re-arrange
 pm_df <- p2h_df %>%
   dplyr::group_by(pos2) %>%
   dplyr::slice(1:configs$nbr_owners) %>%
   dplyr::summarize(pos_mean = mean(total))
 
 p2_df <- p2h_df %>%
   dplyr::group_by(pos2) %>%
   dplyr::arrange(desc(total)) %>%
   dplyr::mutate(pos_rank = rank(-total)) %>%
   dplyr::ungroup() %>%
   dplyr::left_join(pm_df, by = 'pos2') %>%
   dplyr::arrange(pos_rank, pos_mean)
 
 
 ### NEED A SELECTION PROCESS THAT DOESN"T REMOVE TOO MANY CFS
 go <- 1
 p2_ <- list()

 ros <- c('MI', 'CI', 'OF')
 
 while (go == 1){
   p_df <- p2_df[1, ]
   roster_pop <- configs$roster$population[which(configs$roster$roster == p_df$pos2)]  
   cat(p_df$player, '...\n')
   
   if (is.null(p2_[[p_df$pos2]])){
     p2_[[p_df$pos2]] <- p_df
     p2_df <- p2_df %>% dplyr::filter(!player_id %in% p_df$player_id)
   } else {
   
     if (nrow(p2_[[p_df$pos2]]) == roster_pop){
       p2_df <- p2_df %>% dplyr::filter(!pos2 %in% p_df$pos2) 
       ros <- ros[which(ros != p_df$pos2)]
       
     } else {
       p2_[[p_df$pos2]] <- rbind(p2_[[p_df$pos2]], p_df)
       p2_df <- p2_df %>% dplyr::filter(!player_id %in% p_df$player_id)
     }
   }
   
   ## Recalculate pos_mean and re-arrange
   pm_df <- p2_df %>%
     dplyr::group_by(pos2) %>%
     dplyr::slice(1:roster_pop) %>%
     dplyr::summarize(pos_mean = mean(total))
   
   p2_df <- p2_df %>%
     dplyr::select(-pos_mean) %>%
     dplyr::left_join(pm_df, by = 'pos2') %>%
     dplyr::arrange(pos_rank, pos_mean)
  
   if (length(ros) == 0) go <- 0
 }
 
 p2hf_df <- p2_ %>% dplyr::bind_rows()
 
 p2val_df <- p2hf_df %>%
   dplyr::group_by(pos2) %>%
   dplyr::summarize(min = -min(total))
 
 p2hf_df <- p2hf_df %>%
   dplyr::left_join(., p2val_df,
                    by = 'pos2') %>%
   dplyr::rename(pos_adj = min) %>%
   dplyr::mutate(priority = 2)
 
 ## Next prioerity level ------------------------------------------------------------
 
 ## Next prioerity level ------------------------------------------------------------
 
 p3h_ros <- configs$roster %>%
   dplyr::filter(priority == 3 & type == 'hit')
 p3hpos <- unique(unlist(p3h_ros$position))
 
 p3h_map <- p3h_ros %>%
   dplyr::select(roster, position) %>%
   tidyr::unnest()
 
 p3h_df <- projs %>%
   dplyr::filter(pos1 %in% p4hpos) %>%
   dplyr::filter(!player_id %in% p1hf_df$player_id & 
                   !player_id %in% p2hf_df$player_id) %>%
   dplyr::left_join(., p3h_map %>% 
                      dplyr::select(pos1 = position, pos3 = roster),
                    by = 'pos1') %>%
   dplyr::distinct(player_id, pos3, .keep_all = TRUE) %>%
   dplyr::filter(!is.na(pos3))
 
 ## Recalculate pos_mean and re-arrange
 pm_df <- p3h_df %>%
   dplyr::group_by(pos3) %>%
   dplyr::slice(1:configs$nbr_owners) %>%
   dplyr::summarize(pos_mean = mean(total))
 
 p3_df <- p3h_df %>%
   dplyr::group_by(pos3) %>%
   dplyr::arrange(desc(total)) %>%
   dplyr::mutate(pos_rank = rank(-total)) %>%
   dplyr::ungroup() %>%
   dplyr::left_join(pm_df, by = 'pos3') %>%
   dplyr::arrange(pos_rank, pos_mean)
 
 ### NEED A SELECTION PROCESS THAT DOESN"T REMOVE TOO MANY CFS
 go <- 1
 p3_ <- list()
 
 ros <- c('IF')
 
 while (go == 1){
   p_df <- p3_df[1, ]
   roster_pop <- configs$roster$population[which(configs$roster$roster == p_df$pos3)]  
   cat(p_df$player, '...\n')
   
   if (is.null(p3_[[p_df$pos3]])){
     p3_[[p_df$pos3]] <- p_df
     p3_df <- p3_df %>% dplyr::filter(!player_id %in% p_df$player_id)
   } else {
     
     if (nrow(p3_[[p_df$pos3]]) == roster_pop){
       p3_df <- p3_df %>% dplyr::filter(!pos3 %in% p_df$pos3) 
       ros <- ros[which(ros != p_df$pos3)]
       
     } else {
       p3_[[p_df$pos3]] <- rbind(p3_[[p_df$pos3]], p_df)
       p3_df <- p3_df %>% dplyr::filter(!player_id %in% p_df$player_id)
     }
   }
   
   ## Recalculate pos_mean and re-arrange
   pm_df <- p3_df %>%
     dplyr::group_by(pos3) %>%
     dplyr::slice(1:roster_pop) %>%
     dplyr::summarize(pos_mean = mean(total))
   
   p3_df <- p3_df %>%
     dplyr::select(-pos_mean) %>%
     dplyr::left_join(pm_df, by = 'pos3') %>%
     dplyr::arrange(pos_rank, pos_mean)
   
   if (length(ros) == 0) go <- 0
 }
 
 p3hf_df <- p3_ %>% dplyr::bind_rows()
 
 p3val_df <- p3hf_df %>%
   dplyr::group_by(pos3) %>%
   dplyr::summarize(min = -min(total))
 
 p3hf_df <- p3hf_df %>%
   dplyr::left_join(., p3val_df,
                    by = 'pos3') %>%
   dplyr::rename(pos_adj = min) %>%
   dplyr::mutate(priority = 3) 
 
 ## SOMETHING WRONG>  WHY isn't conforto goin gin the first selection
 
 ## Next prioerity level ------------------------------------------------------------
 
 p4h_ros <- configs$roster %>%
   dplyr::filter(priority == 4 & type == 'hit')
 p4hpos <- unique(unlist(p4h_ros$position))
 
 p4h_map <- p4h_ros %>%
   dplyr::select(roster, position) %>%
   tidyr::unnest()
 
 p4h_df <- projs %>%
   dplyr::filter(pos1 %in% p4hpos) %>%
   dplyr::filter(!player_id %in% p1hf_df$player_id & 
                   !player_id %in% p2hf_df$player_id & 
                    !player_id %in% p3hf_df$player_id) %>%
   dplyr::left_join(., p4h_map %>% 
                      dplyr::select(pos1 = position, pos4 = roster),
                    by = 'pos1') %>%
   dplyr::distinct(player_id, pos4, .keep_all = TRUE) %>%
   dplyr::filter(!is.na(pos4))
 
 ## Recalculate pos_mean and re-arrange
 pm_df <- p4h_df %>%
   dplyr::group_by(pos4) %>%
   dplyr::slice(1:16) %>%
   dplyr::summarize(pos_mean = mean(total))
 
 p4_df <- p4h_df %>%
   dplyr::group_by(pos4) %>%
   dplyr::arrange(desc(total)) %>%
   dplyr::mutate(pos_rank = rank(-total)) %>%
   dplyr::ungroup() %>%
   dplyr::left_join(pm_df, by = 'pos4') %>%
   dplyr::arrange(pos_rank, pos_mean)
 
 ### NEED A SELECTION PROCESS THAT DOESN"T REMOVE TOO MANY CFS
 go <- 1
 p4_ <- list()
 
 ros <- c('Util')
 
 while (go == 1){
   p_df <- p4_df[1, ]
   roster_pop <- configs$roster$population[which(configs$roster$roster == p_df$pos4)]  
   cat(p_df$player, '...\n')
   
   if (is.null(p4_[[p_df$pos4]])){
     p4_[[p_df$pos4]] <- p_df
     p4_df <- p4_df %>% dplyr::filter(!player_id %in% p_df$player_id)
   } else {
     
     if (nrow(p4_[[p_df$pos4]]) == roster_pop){
       p4_df <- p4_df %>% dplyr::filter(!pos4 %in% p_df$pos4) 
       ros <- ros[which(ros != p_df$pos4)]
       
     } else {
       p4_[[p_df$pos4]] <- rbind(p4_[[p_df$pos4]], p_df)
       p4_df <- p4_df %>% dplyr::filter(!player_id %in% p_df$player_id)
     }
   }
   
   ## Recalculate pos_mean and re-arrange
   pm_df <- p4_df %>%
     dplyr::group_by(pos4) %>%
     dplyr::slice(1:roster_pop) %>%
     dplyr::summarize(pos_mean = mean(total))
   
   p4_df <- p4_df %>%
     dplyr::select(-pos_mean) %>%
     dplyr::left_join(pm_df, by = 'pos4') %>%
     dplyr::arrange(pos_rank, pos_mean)
   
   if (length(ros) == 0) go <- 0
 }
 
 p4hf_df <- p4_ %>% dplyr::bind_rows()
 
 p4val_df <- p4hf_df %>%
   dplyr::group_by(pos4) %>%
   dplyr::summarize(min = -min(total))
 
 p4hf_df <- p4hf_df %>%
   dplyr::left_join(., p4val_df,
                    by = 'pos4') %>%
   dplyr::rename(pos_adj = min) %>%
   dplyr::mutate(priority = 4) 
 
#### Pictcher ---------------------------------------------------------------------------- 

 ## Postion adjustments
 
 # Priority 1 hitting
 p1p_ros <- configs$roster %>%
   dplyr::filter(priority == 1 & type == 'pitch')
 
 p1p_df <- projs %>%
   dplyr::filter(pos1 %in% p1p_ros$roster)
 
 ## For each position, give a ranking
 ## Cut player
 
 p1p_ <- list()
 for (i in 1:nrow(p1p_ros)){
   p1p_[[i]] <- p1p_df %>%
     dplyr::filter(pos1 == p1p_ros$roster[i]) %>%
     dplyr::arrange(desc(total)) 
   
   ## TODO MAKE THESE SLICES DYNAMIC
   p1p_[[i]] <- p1p_[[i]] %>%
     dplyr::mutate(pos_rank = 1:nrow(p1p_[[i]]),
                   pos_mean = mean(p1p_[[i]]$total[1:24]))
 }
 p1p_df <- p1p_ %>% 
   dplyr::bind_rows() %>%
   dplyr::arrange(pos_rank, pos_mean) %>%
   dplyr::distinct(player_id, .keep_all = TRUE)
 
 ### NEED A SELECTION PROCESS THAT DOESN"T REMOVE TOO MANY CFS
 go <- 1
 p1_ <- list()
 p1_df <- p1p_df

 while (go == 1){
   p_df <- p1_df[1, ]
   
   cat(p_df$player, '...\n')
   
   if (is.null(p1_[[p_df$pos1]])){
     p1_[[p_df$pos1]] <- p_df
     p1_df <- p1_df %>% dplyr::filter(!player_id %in% p_df$player_id)
     
   } else {
     
     if (nrow(p1_[[p_df$pos1]]) == 24){
       p1_df <- p1_df %>% dplyr::filter(!pos1 %in% p_df$pos1) 
     } else {
       p1_[[p_df$pos1]] <- rbind(p1_[[p_df$pos1]], p_df)
       p1_df <- p1_df %>% dplyr::filter(!player_id %in% p_df$player_id)
     }
   }
   
   ## Recalculate pos_mean and re-arrange
   pm_df <- p1_df %>%
     dplyr::group_by(pos1) %>%
     dplyr::slice(1:24) %>%
     dplyr::summarize(pos_mean = mean(total))
   
   p1_df <- p1_df %>%
     dplyr::select(-pos_mean) %>%
     dplyr::left_join(pm_df, by = 'pos1') %>%
     dplyr::arrange(pos_rank, pos_mean)
   
   if (all(unlist(lapply(p1_, nrow)) >= 24)) go <- 0
 }
 
 p1hf_df <- p1_ %>% dplyr::bind_rows()
 
 p1val_df <- p1hf_df %>%
   dplyr::group_by(pos1) %>%
   dplyr::summarize(min = -min(total))
 
 p1hf_df <- p1hf_df %>%
   dplyr::left_join(., p1val_df,
                    by = 'pos1') %>%
   dplyr::rename(pos_adj = min) %>%
   dplyr::mutate(priority = 1) %>%
   dplyr::arrange(desc(total))
 
 
 
 
 
 
 
 
 
  



pos_ <- projs %>% plyr::dlply(., 'pos1')


g <- list()

for (i in 1:nrow(configs$roster)){
  g[[i]] <- addPosAdjustment(configs$roster[i,],
                              projs, 
                              configs)
}

gg <- g %>% dplyr::bind_rows()

h <- list()
teams <- as.data.frame(table(projs$team))%>%
  dplyr::filter(Var1 != '')

for (i in 1:nrow(teams)){
  h[[i]] <- addTeamAdjustment(teams$Var1[i],
                             projs, 
                             configs)
}

hh <- h %>% dplyr::bind_rows()



addPosAdjustment <- function(roster_df,
                             projs,
                             configs){

projs_df <- projs %>% 
  dplyr::filter(pos1 %in% roster_df$position[[1]]) %>%
  dplyr::arrange(desc(total)) %>%
  dplyr::distinct(player_id, .keep_all = T)

rp_value <- projs_df %>%
  dplyr::slice(roster_df$population)
projs_df$pos_adj <- -rp_value$total

projs_df
}

addTeamAdjustment <- function(team_id,
                              projs,
                              configs){
  
  projs_df <- projs %>% 
    dplyr::filter(team == team_id) %>%
    dplyr::arrange(desc(total)) %>%
    dplyr::distinct(player_id, .keep_all = T)
  
  rp_value <- projs_df %>%
    dplyr::slice(configs$nbr_owners)
  projs_df$team_adj <- -rp_value$total
  
  projs_df
}





