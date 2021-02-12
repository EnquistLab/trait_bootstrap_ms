

#take in table of simulations results

summarize_simulations <- function(simulation_result){
  
  out <- unique(simulation_results[,which(!colnames(simulation_results)%in%c("estimate","ci_high","ci_low"))])

  out$fractional_difference <- NA
  out$pct_in_CI <- NA
  
  for(i in 1:nrow(out)){
    
    #print
    print(i)
    
    #Using merge to subset the data.
    data_i <- merge(x = out[i,],y = simulation_results)
    
    #fraction with true value in 95 CI? 
    out$pct_in_CI[i] <- length(which(data_i$ci_low<=data_i$true_value & data_i$ci_high>=data_i$true_value))/nrow(data_i)*100
    
    #mean fractional difference from true value
    out$fractional_difference[i] <- mean((data_i$estimate-data_i$true_value)/data_i$true_value)
    
    
    
    
    
    
  }# for loop
  
  
  return(out)
  
}


###################################


# I initially tried to use a for loop, but for some reason merge functions differently in the loop  
#    comparisons  <- apply(X = out,MARGIN = 1,FUN = function(x){
#        
#        #Using merge to subset the data.
#        data_x <- merge(x = x,y = simulation_results)
#        
#        #fraction with true value in 95 CI? 
#        pct_in_CI <- length(which(data_x$ci_low<=data_x$true_value & data_x$ci_high>=data_x$true_value))/nrow(data_x)*100
#        
#        #mean fractional difference from true value
#        fractional_difference <- mean((data_x$estimate-data_x$true_value)/data_x$true_value)
#        
#        #package it up to return
#        return(cbind(pct_in_CI,fractional_difference))
#        
#      }
#      )











