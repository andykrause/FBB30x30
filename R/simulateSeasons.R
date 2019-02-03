#*****************************************************************************************
#
#  Functions to calculate statistical measures
#
#*****************************************************************************************

#' 
#' Calculate SLG
#' 
#' Calculate slugging percentage
#' 
#' @param x_df Data.frame 
#' @return Vector of slugging percentages
#' @export

multiFBBSimulations <- function(sims,
                                nbr_owners = 8,
                                season_year = 2018,
                                rankings = NULL,
                                strategies = NULL,
                                verbose = 1){
  
  sim_objs <- vector('list', sims)
  
  # Set a base config for training rankings
  configs_base <- setConfigs(nbr_owners = nbr_owners,
                             season_year = season_year)
  
  # Create rankings
  if (is.null(rankings)) rankings <- 'rrv'
  rankings_ <- purrr::map(.x = rankings,
                          .f = customRankings,
                          configs = configs) %>%
    purrr::set_names(rankings)
  
  # Simulate Seasons
  for (ss in 1:length(sim_objs)){
    
    if (verbose >= 1) message('Simulation: ', ss)
    
    # Set Randomized rankings types
    set.seed(ss)
    rankings_types <- sample(rankings, nbr_owners, replace = TRUE)
    
    # Update Configs 
    configs_ss <- setConfigs(nbr_owners = nbr_owners,
                             season_year = season_year,
                             rankings_types = rankings_types)
    
    # Run simulation
    sim_objs[[ss]] <- simulateFBB(configs = configs_i, 
                                  verbose = 1, 
                                  rankings_ = rankings_)
  }
  
  # sim_summ <- analyzeSimulations(sim_objs)
  structure(list(raw = sim_objs,
                 summary = analyzeSimulations(sim_objs)),
            'multiSim')
}

#' 
#' Calculate SLG
#' 
#' Calculate slugging percentage
#' 
#' @param x_df Data.frame 
#' @return Vector of slugging percentages
#' @export

simulateFBB <- function(configs,
                        verbose = 1,
                        ...){
  
  draft_obj <- draftTeams(configs = configs,
                          verbose = verbose,
                          ...)
  
  
  season_obj <- playSeason(draft_obj = draft_obj,
                           configs = configs)
  
  structure(list(draft = draft_obj,
                 season = season_obj,
                 configs = configs),
            class = 'fbbSimulation')
  
}

#' 
#' Calculate SLG
#' 
#' Calculate slugging percentage
#' 
#' @param x_df Data.frame 
#' @return Vector of slugging percentages
#' @export

analyzeSimulations <- function(sim_objs){
  
  # Get Totals
  totals_ <- lapply(sim_objs, function(x) x$season$total)
  
  # Get ranking systems
  rankings_ <- lapply(sim_objs, function(x) x$configs$behavior$rankings)
  
  # Get Strategies
  strategies_ <- lapply(sim_objs, function(x) x$configs$behavior$strategy)
  
  # Combine into summary table
  summ_df <- totals_ %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(ranking = unlist(rankings_),
                  strategy = unlist(strategies_))
  
  # Get picked history
  suppressWarnings(
    picked_df <- lapply(sim_objs, function(x) x$draft$picked) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(season = sort(rep(1:length(sim_objs), 
                                      nrow(sim_objs[[1]]$draft$picked)))))
  
  # Get full season stats
  stats_df <- lapply(sim_objs, function(x) x$season$points) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(season = sort(rep(1:length(sim_objs), 
                                    sim_objs[[1]]$configs$nbr_owners)))
  
  
  structure(list(summ = summ_df,
                 picks = picked_df,
                 standings = stats_df),
            class = 'simSummary')
  
}


