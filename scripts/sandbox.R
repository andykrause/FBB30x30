
  library(FBB30x30)
  library(tidyverse)

  configs <- setConfigs(nbr_owners = 8,
                        season_year = 2019)

  rank_rrv <- customRankings('rrv', configs)
  rank_fp <- customRankings('fp', configs)
  
  
  



#####
  
  draft_info <- draftSetup(configs,
                           rankings_list = list(fp_2019, rrv_2019, rrv_2019, rrv_2019, 
                                                rrv_2019, rrv_2019, rrv_2019, rrv_2019))
  
  draft_obj <- executeDraft(draft_info, verbose = 2)
  
