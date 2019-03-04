#tidy up output data

tidy_sim_output<-function(simulation_output){
  
output<-NULL  
  
  for(i in 1:nrow(simulation_output)){
  
  
  data_i<-simulation_output[i,]  
    
  
  out_i<-matrix(nrow = 3,ncol=11)
  out_i<-as.data.frame(out_i)
  colnames(out_i) <- c("original_id","sampling_method","number_or_prop","total_sample_size",
                       "trait","site","moment","estimation_method","estimate","pct_difference","ordinal_performance")
  out_i$original_id<- i 
  out_i$sampling_method<-data_i$sampling_method  
  out_i$number_or_prop<-data_i$sample_size_or_proportion
  out_i$total_sample_size <- data_i$total_samples    
  out_i$trait <- data_i$trait 
  out_i$site <- data_i$site
  out_i$moment <- data_i$moment
  
  
  #Calc relative performance
  out_i$estimation_method[1]<-"nonparametric"
  out_i$estimate[1] <- data_i$boot_mean
  out_i$pct_difference[1] <- ((data_i$boot_mean - data_i$actual_mean)/data_i$actual_mean)*100
  
  out_i$estimation_method[2]<-"parametric"
  out_i$estimate[2] <- data_i$parametric_mean
  out_i$pct_difference[2] <- ((data_i$parametric_mean - data_i$actual_mean)/data_i$actual_mean)*100
  
  out_i$estimation_method[3]<-"species_mean"
  out_i$estimate[3] <- data_i$cwm_mean
  out_i$pct_difference[3] <- ((data_i$cwm_mean - data_i$actual_mean)/data_i$actual_mean)*100
  
  
  #If all metrics perform the same, no winners, so assign NA
    if(out_i$pct_difference[1]==out_i$pct_difference[2]& out_i$pct_difference[2]==out_i$pct_difference[3]) {
      
      out_i$ordinal_performance<-NA
      
      }
  
  
  #If two metrics are equivalent, set winners and losers, no intermediate
  
  if((out_i$pct_difference[1]==out_i$pct_difference[2] | 
     out_i$pct_difference[2]==out_i$pct_difference[3] |
     out_i$pct_difference[1]==out_i$pct_difference[3] ) &
     !(out_i$pct_difference[1]==out_i$pct_difference[2]& out_i$pct_difference[2]==out_i$pct_difference[3])
     ) {
    
    
    
        out_i$ordinal_performance[which(abs(out_i$pct_difference)==min(abs(out_i$pct_difference)))]<-"best"
        out_i$ordinal_performance[which(abs(out_i$pct_difference)==max(abs(out_i$pct_difference)))]<-"worst"
    
      }
  
  
  #If all three are distinct values, assign them all ranks
  
  if( out_i$pct_difference[1]!=out_i$pct_difference[2] & 
      out_i$pct_difference[2]!=out_i$pct_difference[3] &
      out_i$pct_difference[1]!=out_i$pct_difference[3] ){
    
    
    
      out_i$ordinal_performance[which(abs(out_i$pct_difference)==min(abs(out_i$pct_difference)))]<-"best"
      out_i$ordinal_performance[which(abs(out_i$pct_difference)==max(abs(out_i$pct_difference)))]<-"worst"
      out_i$ordinal_performance[which(abs(out_i$pct_difference)!=max(abs(out_i$pct_difference)) &
                                  abs(out_i$pct_difference)!=min(abs(out_i$pct_difference)))]<-"intermediate"
      
      
      }
    
  
  
  output<-rbind(output,out_i)
  

  
  
  } #end i loop

    
  
return(output)  
  
  
  
  
}