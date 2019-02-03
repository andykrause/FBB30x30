
  library(FBB30x30)
  library(tidyverse)

  configs <- setConfigs(nbr_owners = 8,
                        season_year = 2018,
                        rankings = c('rrv', rep('fp', 7)))

  ms_ <- multiFBBSimulations(sims = 100,
                             rankings = c('rrv', 'rrvt', 'fp'),
                             strategies = c('ba', 'ba5'))
  
  lm(points ~ ranking + strategy + as.factor(team), data = ms_$summary$summ)%>% summary()