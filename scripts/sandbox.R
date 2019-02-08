
  library(FBB30x30)
  library(tidyverse)

  configs <- setConfigs(nbr_owners = 8,
                        season_year = 2018,
                        rankings = c('rrv', rep('fp', 7)))

  ms_ <- multiFBBSimulations(sims = 50,
                             season_year = 2019,
                             rankings = c('rrv', 'fp'),
                             strategies = c('ba'))
  
  lm(points ~ ranking + strategy + as.factor(team), data = ms_$summary$summ)%>% summary()
  
  lm(points ~ ranking + as.factor(team), data = ms_$summary$summ)%>% summary()
  