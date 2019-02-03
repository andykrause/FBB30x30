
  library(FBB30x30)
  library(tidyverse)

  configs <- setConfigs(nbr_owners = 8,
                        season_year = 2018,
                        rankings = c('rrv', rep('fp', 7)))

  ms_ <- multiFBBSimulations(sims = 15,
                             rankings = c('rrv', 'fp'))
  
  ## Add configs to simulations
  

  
  summary(lm(points ~ ds + as.factor(team), data = sst))
