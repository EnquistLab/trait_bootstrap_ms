#code to randomly draw from all traits

draw_traits_random<-function(traits_df, sample_size){
  
traits_df <- na.omit(traits_df)


out_df<-NULL  
  
  
  for(i in 1:length(unique(traits_df$taxon))){
  
  
    taxon_i<-unlist(unique(traits_df$taxon))[i]
    #print(taxon_i)
    
    
    number_draws<-sample_size

    if(length(which(traits_df$taxon==taxon_i))<sample_size){number_draws<-length(which(traits_df$taxon==taxon_i))}

     
    out_df <- rbind(out_df,traits_df[resample(x = which(traits_df$taxon==taxon_i),size = number_draws),] )
  
  }#for i  
  
  return(out_df)  
  
}#end fx
####################################################

#code to randomly draw from all traits in proprtion to sample size

draw_traits_random_proportion<-function(traits_df, sample_proportion, community_df){
  
  traits_df <- na.omit(traits_df)
  
  
  out_df<-NULL  
  
  
  for(i in 1:length(unique(traits_df$taxon))){
    
    
    taxon_i<-unlist(unique(traits_df$taxon))[i]
    number_draws<-ceiling(sample_proportion*community_df$abundance[which(community_df$taxon==taxon_i)])
    
    #print(taxon_i)
    
    if(length(which(traits_df$taxon==taxon_i))<number_draws){number_draws<-length(which(traits_df$taxon==taxon_i))}
    
    out_df <- rbind(out_df,traits_df[resample(x = which(traits_df$taxon==taxon_i),size = number_draws),] )
    
  }#for i  
  
  return(out_df)  
  
}#end fx


####################################################
#code to randomly draw traits and calculate a mean

draw_traits_random_means<-function(traits_df, sample_size){
  
  traits_df <- na.omit(traits_df)
  
  out_df<-NULL  
  
  
  for(i in 1:length(unique(traits_df$taxon))){
    
    
    taxon<-unique(traits_df$taxon)[i]
    
    number_draws<-sample_size
    
    if(length(which(traits_df$taxon==taxon))<sample_size){number_draws<-length(which(traits_df$taxon==taxon))}
    
    
    if(number_draws==1){
      
      out_df <- rbind(out_df,c(taxon,apply(X = traits_df[resample(x = which(traits_df$taxon==taxon),size = number_draws),2:ncol(traits_df)],
                                                        MARGIN = 2,FUN = function(x){as.numeric(as.character(x))})
      ))
      
      
      
                          
    }# one observation
    
    if(number_draws > 1){
      
      out_df <- rbind(out_df,c(taxon,colMeans(    apply(X = traits_df[resample(x = which(traits_df$taxon==taxon),size = number_draws),2:ncol(traits_df)],
                                                        MARGIN = 2,FUN = function(x){as.numeric(as.character(x))})
      )))
      
    }#
    
  }#for i  
  colnames(out_df)[1]<-"taxon"
  
  out_df<-as.data.frame(out_df)
  return(out_df)  
  
}#end fx
###################


#####################
resample <- function(x, ...) x[sample.int(length(x), ...)]

#####################