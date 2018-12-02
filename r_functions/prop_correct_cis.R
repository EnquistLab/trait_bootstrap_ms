#function to summarize proportion of CIs that contain true mean

proportion_ci_correct<-function(simulation_output){
  
  
output<-unique(simulation_output[c('trait','site','moment','sample_size','total_samples')])  
output$proportion_ci95_correct<-NA  
output$proportion_ci100_correct<-NA
  
  
for(i in 1:nrow(output)){

output$proportion_ci95_correct[i]<-
        sum(simulation_output$in_95ci[which(simulation_output$sample_size==output$sample_size[i] &
        simulation_output$trait==output$trait[i] &
        simulation_output$site==output$site[i] &
        simulation_output$moment==output$moment[i])])/length(which(simulation_output$sample_size==output$sample_size[i] &
                                                                     simulation_output$trait==output$trait[i] &
                                                                     simulation_output$site==output$site[i] &
                                                                     simulation_output$moment==output$moment[i]))    
  
output$proportion_ci100_correct[i] <- 
  sum(simulation_output$in_100ci[which(simulation_output$sample_size==output$sample_size[i] &
                                                                      simulation_output$trait==output$trait[i] &
                                                                      simulation_output$site==output$site[i] &
                                                                      simulation_output$moment==output$moment[i])])/
  length(which(simulation_output$sample_size==output$sample_size[i] & 
                 simulation_output$trait==output$trait[i] &
                 simulation_output$site==output$site[i] &
                simulation_output$moment==output$moment[i]))    

  
}  
  
  
output$method<-"nonparametric_bootstrapping"  


output_p<-unique(simulation_output[c('trait','site','moment','sample_size','total_samples')])  
output_p$proportion_ci95_correct<-NA  
output_p$proportion_ci100_correct<-NA


for(i in 1:nrow(output)){
  
  output_p$proportion_ci95_correct[i]<-
    sum(simulation_output$in_95ci_para[which(simulation_output$sample_size==output$sample_size[i] &
                                          simulation_output$trait==output$trait[i] &
                                          simulation_output$site==output$site[i] &
                                          simulation_output$moment==output$moment[i])])/length(which(simulation_output$sample_size==output$sample_size[i] &
                                                                                                       simulation_output$trait==output$trait[i] &
                                                                                                       simulation_output$site==output$site[i] &
                                                                                                       simulation_output$moment==output$moment[i]))    
  
  output_p$proportion_ci100_correct[i] <- 
    sum(simulation_output$in_100_ci_para[which(simulation_output$sample_size==output$sample_size[i] &
                                           simulation_output$trait==output$trait[i] &
                                           simulation_output$site==output$site[i] &
                                           simulation_output$moment==output$moment[i])])/
    length(which(simulation_output$sample_size==output$sample_size[i] & 
                   simulation_output$trait==output$trait[i] &
                   simulation_output$site==output$site[i] &
                   simulation_output$moment==output$moment[i]))    
  
  
}  


output_p$method<-"parametric_bootstrapping"  

output<-rbind(output,output_p)


return(output)  
  
  
}

###############################

#function to summarize proportion of CIs that contain true mean

proportion_ci_correct_proportional_sampling<-function(simulation_output){
  
  
  output<-unique(simulation_output[c('trait','site','moment','sample_proportion','total_samples')])  
  output$proportion_ci95_correct<-NA  
  output$proportion_ci100_correct<-NA
  
  
  for(i in 1:nrow(output)){
    
    output$proportion_ci95_correct[i]<-
      sum(simulation_output$in_95ci[which(simulation_output$sample_proportion==output$sample_proportion[i] &
                                            simulation_output$trait==output$trait[i] &
                                            simulation_output$site==output$site[i] &
                                            simulation_output$moment==output$moment[i])])/length(which(simulation_output$sample_proportion==output$sample_proportion[i] &
                                                                                                         simulation_output$trait==output$trait[i] &
                                                                                                         simulation_output$site==output$site[i] &
                                                                                                         simulation_output$moment==output$moment[i]))    
    
    output$proportion_ci100_correct[i] <- 
      sum(simulation_output$in_100ci[which(simulation_output$sample_proportion==output$sample_proportion[i] &
                                             simulation_output$trait==output$trait[i] &
                                             simulation_output$site==output$site[i] &
                                             simulation_output$moment==output$moment[i])])/
      length(which(simulation_output$sample_proportion==output$sample_proportion[i] & 
                     simulation_output$trait==output$trait[i] &
                     simulation_output$site==output$site[i] &
                     simulation_output$moment==output$moment[i]))    
    
    
  }  
  
  
  
  return(output)  
  
  
}
