#*****************************************************************************************
#
#   Create Data objeccts for 30x30Baseball
#
#*****************************************************************************************

  library(readr)
  library(tidyverse)
  library(FBB30x30)

### People and Teams ---------------------------------------------------------------------
  
  ## Read in Data  
  players_df <- read.csv(file.path(getwd(), 'data', 'raw', 'players', 'people.csv'))
  
  players_df <- players_df %>%
    dplyr::filter(as.Date(as.character(finalGame)) >= as.Date('2014-01-01')) %>%
    dplyr::select(player_id = playerID, retro_id = retroID, bbref_id = bbrefID, 
                  first = nameFirst, last = nameLast, given = nameGiven, 
                  last_game = finalGame) %>%
    dplyr::mutate(full = paste(first, last),
                  last_game = as.Date(as.character(last_game)))
  class(players_df) <- c('players', class(players_df))
  
  teams_df <- read.csv(file.path(getwd(), 'data', 'raw', 'players', 'teams.csv'))
  
  teams_df <- teams_df %>%
    dplyr::filter(yearID == 2017) %>%
    dplyr::select(team_id = teamID, br_id = teamIDBR, lahman_id = teamIDlahman45,
                  retro_id = teamIDretro) %>%
    dplyr::mutate(espn_id = c('ARI', 'ATL', 'BAL', 'BOS', 'CWS', 'CHC', 'CIN', 'CLE',
                              'COL', 'DET', 'HOU', 'KC', 'LAA', 'LAD', 'MIA', 'MIL',
                              'MIN', 'NYY', 'NYM', 'OAK', 'PHI', 'PIT', 'SD', 'SEA',
                              'SF', 'STL', 'TB', 'TEX', 'TOR', 'WAS'))
  class(teams_df) <- c('teams', class(teams_df))
  
  
  usethis::use_data(players_df, overwrite=TRUE)
  usethis::use_data(teams_df, overwrite=TRUE)
  
### Statistics ---------------------------------------------------------------------------
  
  ## Read in Data  
  fielding_df <- read.csv(file.path(getwd(), 'data', 'raw', 'stats', 'fielding.csv'))
  
  ## Limit to necessary
   fielding_df <- fielding_df %>% 
     dplyr::filter(yearID >= 2014) %>%
     dplyr::select(player_id = playerID, year = yearID, team = teamID, POS, G, E)
   names(fielding_df) <- tolower(names(fielding_df))
   class(fielding_df) <- c('fielding', class(fielding_df))
   
  ## Collapse to sum of G, E 
   fielding_ndf <- fielding_df %>%
     tidyr::nest(-player_id, -year, .key = 'fielding_stats') %>%
     tibble::as.tibble()
   
  # Load Data
   batting_df <- read.csv(file.path(getwd(), 'data', 'raw', 'stats', 'batting.csv'))
   
   # Trim
   batting_df <- batting_df %>% 
     dplyr::filter(yearID >= 2014) %>%
     dplyr::select(player_id = playerID, year = yearID, team = teamID, G, AB, R, HR, RBI, 
                   SB, SO, GIDP, SH, SF, H, X2B, X3B, IBB, HBP, BB)
   names(batting_df) <- tolower(names(batting_df))
   class(batting_df) <- c('batting', class(batting_df))
   
   # Add fields
   batting_ndf <- batting_df %>%
     dplyr::mutate(slg = calcSLG(.),
                   pa = calcPA(.),
                   obp = calcOBP(.)) %>%
     tidyr::nest(-player_id, -year, .key = 'batting_stats') %>%
     tibble::as.tibble()
   
 ## Pitching stats
   
   # Load data
   pitching_df <- read.csv(file.path(getwd(), 'data', 'raw', 'stats', 'pitching.csv'))
   
   # Filter and select
   pitching_df <- pitching_df %>% 
     dplyr::filter(yearID >= 2014) %>%
     dplyr::select(player_id = playerID, year = yearID, team = teamID, W, G, GS, IPouts, 
                   H, ER, BB, SO, IBB, SV)
   names(pitching_df) <- tolower(names(pitching_df))
   
   # Summarize and mutate
   pitching_ndf <- pitching_df %>%
     dplyr::mutate(ip = round(ipouts / 3, 1),
                   whip = calcWHIP(.)) %>%
     dplyr::rename(k = so) %>%
     dplyr::select(-ipouts) %>%
     tidyr::nest(-player_id, -year, .key = 'pitching_stats') %>%
     tibble::as.tibble()
   class(pitching_df) <- c('pitching', class(pitching_df))
   
   ## Write data objects
   
   usethis::use_data(fielding_ndf, overwrite=TRUE)
   usethis::use_data(batting_ndf, overwrite=TRUE)
   usethis::use_data(pitching_ndf, overwrite=TRUE)
  
### Projections --------------------------------------------------------------------------

   ## Load Projection Data

    # Hitters
    hp_2014 <- read.csv(file.path(getwd(), 'data', 'raw', 'projections',
                                  'fp_hitters_2014.csv'))
    hp_2015 <- read.csv(file.path(getwd(), 'data', 'raw', 'projections',
                                  'fp_hitters_2015.csv'))
    hp_2016 <- read.csv(file.path(getwd(), 'data', 'raw', 'projections',
                                  'fp_hitters_2016.csv'))
    hp_2017 <- read.csv(file.path(getwd(), 'data', 'raw', 'projections',
                                  'fp_hitters_2017.csv'))
    hp_2018 <- read.csv(file.path(getwd(), 'data', 'raw', 'projections',
                                  'fp_hitters_2018.csv'))

    # Hitters
    pp_2014 <- read.csv(file.path(getwd(), 'data', 'raw', 'projections',
                                  'fp_pitchers_2014.csv'))
    pp_2015 <- read.csv(file.path(getwd(), 'data', 'raw', 'projections',
                                  'fp_pitchers_2015.csv'))
    pp_2016 <- read.csv(file.path(getwd(), 'data', 'raw', 'projections',
                                  'fp_pitchers_2016.csv'))
    pp_2017 <- read.csv(file.path(getwd(), 'data', 'raw', 'projections',
                                  'fp_pitchers_2017.csv'))
    pp_2018 <- read.csv(file.path(getwd(), 'data', 'raw', 'projections',
                                  'fp_pitchers_2018.csv'))

   ## Standardize Projection Data

    # 2014
    names(hp_2014) <- tolower(names(hp_2014))
    hp_2014 <- hp_2014 %>% 
      dplyr::rename(pos = position,
                    avg = ave,
                    h = hits,
                    hr = hrs,
                    player = name,
                    r = runs) %>%
      dplyr::mutate(pos = gsub(',', '/', pos),
                    pos = gsub('/', ' | ', pos),
                    year = 2014) %>%
      dplyr::select(-c(cs, type))

    # 2015
    names(hp_2015) <- tolower(names(hp_2015))
    hp_2015 <- hp_2015 %>%
      dplyr::mutate(player = paste0(substr(name, str_locate(name, ',')+2, 100),
                                    " ", substr(name, 1, str_locate(name, ',') - 1)),
                    pos = gsub(',', '/', pos),
                    pos = gsub('/', ' | ', pos),
                    year = 2015) %>%
      dplyr::rename(so = k, 
                    ops = obps) %>%
      dplyr::select(-c(cs, g, al.nl.., mixed.., name))
  
    # 2016
    names(hp_2016) <- tolower(names(hp_2016))
    hp_2016 <- hp_2016 %>%
      dplyr::mutate(pos = gsub(',', '/', pos),
                    pos = gsub('/', ' | ', pos),
                    year = 2016) %>%
      dplyr::rename(avg = ave,
                    h = hits,
                    hr = hrs,
                    player = name,
                    r = runs)

    # 2017
    names(hp_2017) <- tolower(names(hp_2017))
    hp_2017 <- hp_2017 %>%
      fixFPName(.) %>%
      dplyr::mutate(year = 2017)
    
    # 2018
    names(hp_2018) <- tolower(names(hp_2018))
    hp_2018 <- hp_2018 %>%
      dplyr::rename(pos = positions) %>%
      dplyr::mutate(year = 2018,
                    pos = gsub(',', '/', pos),
                    pos = gsub('/', ' | ', pos))

    # Combine
    batprojs_df <- rbind(hp_2014, hp_2015, hp_2016, hp_2017, hp_2018) %>%
      tibble::as.tibble()%>%
      dplyr::select(player, team, year, tidyselect::everything())
    

   ## Pitchers

    # 2014
    names(pp_2014) <- tolower(names(pp_2014))
    pp_2014 <- pp_2014 %>% 
      dplyr::rename(pos = position,
                    player = name,
                    bb = bbi) %>%
      dplyr::mutate(pos =  gsub(',', '/', pos),
                    pos =  gsub('/', ' | ', pos),
                    year = 2014) %>%
      dplyr::select(-c(hd, hr, type, bs))
                    
    # 2015
    names(pp_2015) <- tolower(names(pp_2015))
    pp_2015 <- pp_2015 %>%
      dplyr::mutate(gsp = gs/g,
                    player = paste0(substr(name, str_locate(name, ',')+2, 100),
                                   " ", substr(name, 1, str_locate(name, ',') - 1)),
                    pos = 'SP, RP',
                    pos = ifelse(gsp == 0, 'RP', pos),
                    pos = ifelse(gsp >= .85, 'SP', pos),
                    pos = gsub(',', '/', pos),
                    pos = gsub('/', ' | ', pos),
                    year = 2015) %>%
      dplyr::select(-c(al.nl.., mixed.., sho, name, gsp))
    
    # 2016
    names(pp_2016) <- tolower(names(pp_2016))
    pp_2016 <- pp_2016 %>%
      dplyr::mutate(pos = gsub(',', '/', pos),
                    pos = gsub('/', ' | ', pos),
                    year = 2016) %>%
      dplyr::rename(player = name,
                    bb = bbi) %>%
      dplyr::select(-c(hr))
    
    # 2017
    names(pp_2017) <- tolower(names(pp_2017))
    pp_2017 <- pp_2017 %>%
      fixFPName() %>%
      dplyr::mutate(year = 2017) %>%
      dplyr::select(-c(hr))
    
    # 2018
    names(pp_2018) <- tolower(names(pp_2018))
    pp_2018 <- pp_2018 %>%
      dplyr::rename(pos = positions) %>%
      dplyr::mutate(pos = gsub(',', '/', pos),
                    pos = gsub('/', ' | ', pos),
                    year = 2018) %>%
      dplyr::select(-c(hr))
    

    # Combine Al
    pitchprojs_df <- rbind(pp_2014, pp_2015, pp_2016, pp_2017, pp_2018) %>%
      tibble::as.tibble() %>%
      dplyr::select(player, team, year, tidyselect::everything())

  ## Save
    
  usethis::use_data(batprojs_df, overwrite=TRUE)
  usethis::use_data(pitchprojs_df, overwrite=TRUE) 
    
### Rankings -----------------------------------------------------------------------------    
    
  ## Read in Data  
  rank_2016 <- read.csv(file.path(getwd(), 'data', 'raw', 'rankings', 
                                  'fp_draftrankings_2016.csv'))
  rank_2017 <- read.csv(file.path(getwd(), 'data', 'raw', 'rankings', 
                                  'fp_draftrankings_2017.csv'))
  rank_2018 <- read.csv(file.path(getwd(), 'data', 'raw', 'rankings', 
                                  'fp_draftrankings_2018.csv'))
  
  # 2016
  names(rank_2016) <- tolower(names(rank_2016))
  rank_2016 <- rank_2016 %>% 
    dplyr::rename(avg=ave,
                  std.dev=stdev,
                  rank=ecr,
                  player=name) %>%
    dplyr::mutate(pos = gsub(',', '/', pos),
                  pos = gsub('/', ' | ', pos),
                  year = 2016,
                  x = NULL,
                  adp = as.numeric(as.character(adp)),
                  player = as.character(player),
                  team = as.character(team)) %>%
    tibble::as.tibble()
  rank_2016$adp[is.na(rank_2016$adp)] <- ceiling(rank_2016$avg[is.na(rank_2016$adp)])
  
  # 2017
  names(rank_2017) <- tolower(names(rank_2017))
  x1 <- str_locate(rank_2017$name, '[(]')[,1]
  name <- substr(rank_2017$name, 1, x1-2)
  team <- substr(rank_2017$name, x1+1, x1+3)
  team <- gsub(',', '', team)
  team <- gsub(')', '', team)
  pos <- substr(rank_2017$name, x1+5, 100)
  x2 <- str_locate(pos, '[)]')[,1]
  pos <- substr(pos, 1, x2-1)
  pos <- gsub(',', ' | ', pos)
  rank_2017$year <- 2017
  rank_2017$name <- NULL
  rank_2017$player <- name
  rank_2017$team <- team
  rank_2017$pos <- substr(pos, 2, 100)
  rank_2017 <- rank_2017 %>% 
    dplyr::rename(rank=ranking,
                  std.dev=stdev,
                  avg=average) %>%
    tibble::as.tibble()
  rank_2017$adp[is.na(rank_2017$adp)] <- ceiling(rank_2017$avg[is.na(rank_2017$adp)])
  
  # 2018
  names(rank_2018) <- tolower(names(rank_2018))
  rank_2018 <- rank_2018 %>% 
    dplyr::rename(pos = positions) %>%
    dplyr::mutate(pos = gsub(',', '/', pos),
                  pos = gsub('/', ' | ', pos),
                  year = 2018,
                  vs..adp = NULL,
                  team = as.character(team),
                  player = as.character(player)) %>%
    tibble::as.tibble()
  rank_2018$adp[is.na(rank_2018$adp)] <- ceiling(rank_2018$avg[is.na(rank_2018$adp)])
  
  # Combine All
  rankings_df <- rbind(rank_2016, rank_2017, rank_2018) %>%
    tibble::as.tibble() %>%
    dplyr::select(player, team, pos, year, rank, tidyselect::everything())
    
  usethis::use_data(rankings_df, overwrite=TRUE) 
  
### Seasons ------------------------------------------------------------------------------
  
  ## Read in Data  
  season2018_df <- read.csv(file.path(getwd(), 'data', 'raw', 'seasons', 'season2018.csv'))
  season2017_df <- read.csv(file.path(getwd(), 'data', 'raw', 'seasons', 'season2017.csv'))
  season2016_df <- read.csv(file.path(getwd(), 'data', 'raw', 'seasons', 'season2016.csv'))
  season2015_df <- read.csv(file.path(getwd(), 'data', 'raw', 'seasons', 'season2015.csv'))
  
  # 2015
  season2015_df <- season2015_df %>%
    dplyr::mutate(Team = c(2, 3, 1, 5, 6, 4)) %>%
    dplyr::select(team = Team, rank = Rank, r = R, hr = HR, rbi = RBI, so = K, hbp = HBP,
                  sbn = SBN, obp = OBP, slg = SLG, e = E, qs = QS, k = K.1, oba = OBA,
                  whip = WHIP, ptw = PTW, svhd = SVHD, moves = Moves)
  
  # 2016
  season2016_df <- season2016_df %>%
    dplyr::mutate(Team = c(7, 1, 2, 4, 5, 8)) %>%
    dplyr::select(team = Team, rank = Rank, r = R, hr = HR, rbi = RBI, so = K, sbn = SBN, 
                  obp = OBP, slg = SLG, gidp = GIDP, ppa = PPA, fpct = FPCT, ip = IP, 
                  qs = QS, k = K.1, hb = HB, sv = SV, hd = HD, whip = WHIP, moves = Moves)
  
  # 2017
  season2017_df <- season2017_df %>%
    dplyr::mutate(Team = c(2, 9, 8, 3, 11, 1, 10)) %>%
    dplyr::select(team = Team, rank = Rank, r = R, hr = HR, rbi = RBI, so = K, sbn = SBN, 
                  obp = OBP, slg = SLG, gidp = GIDP, e = E, ip = IP, qs = QS, k = K.1, 
                  sv = SV, hd = HD, whip = WHIP, moves = Moves, starts = Starts)
  
  # 2018
  season2018_df <- season2018_df %>%
    dplyr::mutate(Team = c(8, 1, 12, 3, 4, 10, 2, 9)) %>%
    dplyr::select(team = Team, rank = Rank, r = R, hr = HR, rbi = RBI, so = K, sb = SB, 
                  obp = OBP, slg = SLG, gidp = GIDP, e = E, ip = IP, qs = QS, k = K.1, 
                  sv = SV, hd = HD, whip = WHIP, moves = Moves, starts = Starts)
  
  seasons_ <- list(`2015` = season2015_df,
                   `2016` = season2016_df,
                   `2017` = season2017_df,
                   `2018` = season2018_df)
  
  usethis::use_data(seasons_, overwrite=TRUE) 
  