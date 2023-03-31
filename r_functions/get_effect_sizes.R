

get_effect_sizes <- function(tidied_simdata){
  
  effect_sizes <- NULL
  
  for(i in unique(tidied_simdata$method)){
    for(j in unique(tidied_simdata$method)){
      for(s in unique(tidied_simdata$sample_size)){
        for(t in unique(tidied_simdata$trait)){
          for(site in unique(tidied_simdata$site)){
            for(m in unique(tidied_simdata$moment)){
              
              
              data_i <- tidied_simdata %>%
                filter(method == i,
                       sample_size == s,
                       trait == t,
                       moment == m,
                       site == site)
              
              data_j <- tidied_simdata %>%
                filter(method == j,
                       sample_size == s,
                       trait == t,
                       moment == m,
                       site == site)
              
              cohen_d <- cohen.d(data_i$deviation, data_j$deviation)
              
              output <- data.frame(method_1 = i,
                                   method_2 = j,
                                   trait = t,
                                   site = site,
                                   sample_size = s,
                                   moment = m,
                                   cohen_d = cohen_d$estimate,
                                   cohen_d_magnitude = cohen_d$magnitude,
                                   cohen_d_lower_95_ci <- cohen_d$conf.int[1],
                                   cohen_d_upper_95_ci <- cohen_d$conf.int[2])
              
              print(output)
              
              effect_sizes <- effect_sizes %>%
                bind_rows(output)
              
              rm(output)
              
              
            }# m
          }
        } #t
      }#s loop
    } #j loop
  } #i loop
  
 
  return(effect_sizes)
   
}
