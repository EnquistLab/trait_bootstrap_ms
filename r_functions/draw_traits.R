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






####################
distribution_to_means<-function(trait_df){
  out_df<-matrix(nrow = length(unique(trait_df$taxon)),ncol = ncol(trait_df))
  out_df<-as.data.frame(out_df)
  colnames(out_df)<-colnames(trait_df)
  
  #out_df<-trait_df[0,]
  
  for(i in 1:length(unique(traits_df$taxon))){
    
    
    taxon<-unique(traits_df$taxon)[i]
    
    
    
    
    if(length(which(trait_df$taxon==taxon))==1){
      
      out_df[i,]<-trait_df[which(trait_df$taxon==taxon),]
      
    }# one observation
    
    if(length(which(trait_df$taxon==taxon))>1){
      
      temp<-c(taxon,colMeans(   trait_df[which(trait_df$taxon==taxon),2:ncol(trait_df)]))
      #temp<-unname(temp)
      
      
      #names(temp)<-names(out_df)
              
              
        out_df[i,1:ncol(out_df)]<-temp
      
      
      }#
    
  }#for i  
  
  #out_df<-as.data.frame(out_df)
  
  
  out_df[2:ncol(out_df)]<-apply(X = out_df[2:ncol(out_df)],MARGIN = 2,FUN = function(x){as.numeric(as.character(x))})
  
  
  
    
  return(out_df)  
  
  
  
  
}#end fx

#####################
resample <- function(x, ...) x[sample.int(length(x), ...)]

#####################