

#####################
resample <- function(x, ...) x[sample.int(length(x), ...)]

#####################

#'@param tidy_traits Trait data in tidy format
#'@param sample_size Numeric (one number only)
draw_traits_tidy <- function(tidy_traits, sample_size){
  
  tidy_traits$site <- as.character(tidy_traits$site)
  tidy_traits$taxon <- as.character(tidy_traits$taxon)
  
  #create a site x taxon object to apply over
  site_x_taxon <- unique(tidy_traits[c("site","taxon")])
  
  trait_draw <- apply(X = site_x_taxon, MARGIN = 1,FUN = function(x){
    
    #get relevant individual IDs
    IDS <- unique(tidy_traits$ID[which(tidy_traits$site == x[1] & tidy_traits$taxon == x[2])])
    
    #if number of IDS <= sample size, return them all
    if(length(IDS) <= sample_size){
        return(tidy_traits[which(tidy_traits$ID %in% IDS),])
      }                   
    
    #if number of IDs > sample size, sample randomly
    return( tidy_traits[which(tidy_traits$ID %in% resample(IDS,sample_size)),]  )
    
  }  )#apply fx

  out <- do.call(rbind,trait_draw)
  rownames(out) <- NULL
  return(out)

}

#######################


#Quick convenient function when converting to species mean data
#'@param tidy_traits Trait data in tidy format
#'@param level Level at which to calculate means.  Options are "taxon_by_site" or "taxon".
samples_to_means <- function(tidy_traits, level = "taxon_by_site"){
  
  #check that input makes sense  
  if(!level %in% c("taxon","taxon_by_site")){stop("Invalid level supplied")}  
  
  
  if(level=="taxon_by_site"){
    return(as.data.frame(tidy_traits %>% group_by(taxon,trait,site) %>% summarise(across(value,mean), .groups="drop")))
    }
  

  if(level=="taxon"){
    return(as.data.frame(tidy_traits %>% group_by(taxon,trait) %>% summarise(across(value,mean), .groups="drop")))
  }
  

}#end fx



#######################################



#'@param tidy_traits Trait data in tidy format
#'@param sample_size Numeric (one number only)
#'@param focal_trait Trait used to simulate biased sampling
#'@param prob Probability of sampling each leaf (passed to dnbinom)
#'@param large_biased If TRUE (default) large individuals are preferentially sampled, if FALSE, small individuals.
draw_traits_tidy_large <- function(tidy_traits,sample_size,focal_trait = "leaf_area_mm2",prob = 0.75,large_biased=TRUE){
  
  
  
  tidy_traits$site <- as.character(tidy_traits$site)
  tidy_traits$taxon <- as.character(tidy_traits$taxon)
  
  #create a site x taxon object to apply over
  site_x_taxon <- unique(tidy_traits[c("site","taxon")])
  
  
  # #temp
  # #output <- NULL
  # for( i in 1:nrow(site_x_taxon)){
  #   
  #   x <- site_x_taxon[i,]
  #   IDS <- unique(tidy_traits$ID[which(tidy_traits$site == x[1] & tidy_traits$taxon == x[2])])
  #   
  #   
  #   #I don't think this does anything...
  #   if(length(IDS) <= sample_size){
  #     output <- cbind(output,tidy_traits[which(tidy_traits$ID %in% IDS),])
  #   }
  # 
  #   
  #   
  #   #if number of IDs > sample size, take a biased sample
  #   sorted_IDS <- tidy_traits[which(tidy_traits$ID %in% IDS & tidy_traits$trait == focal_trait),"ID"][order(tidy_traits[which(tidy_traits$ID %in% IDS & tidy_traits$trait == focal_trait),]["value"],decreasing = large_biased)]
  #   
  #   
  #   return( tidy_traits[which(tidy_traits$ID %in% resample(x = sorted_IDS,
  #                                                          size = sample_size,
  #                                                          replace = F,
  #                                                          prob = dnbinom(x = 1:length(sorted_IDS),size = sample_size,prob = prob))),]  )
  #   
  # }
  # 
  # #end temp
  trait_draw <- apply(X = site_x_taxon, MARGIN = 1,FUN = function(x){
    print(x)
    #get relevant individual IDs
    IDS <- unique(tidy_traits$ID[which(tidy_traits$site == x[1] & tidy_traits$taxon == x[2])])
    
    #if number of IDS <= sample size, return them all
    if(length(IDS) <= sample_size){
      return(tidy_traits[which(tidy_traits$ID %in% IDS),])
    }                   
    
    #if number of IDs > sample size, take a biased sample
    sorted_IDS <- tidy_traits[which(tidy_traits$ID %in% IDS & tidy_traits$trait == focal_trait),"ID"][order(tidy_traits[which(tidy_traits$ID %in% IDS & tidy_traits$trait == focal_trait),]["value"],decreasing = large_biased)]
    
    
    return( tidy_traits[which(tidy_traits$ID %in% resample(x = sorted_IDS,
                                                           size = sample_size,
                                                           replace = F,
                                                           prob = dnbinom(x = 1:length(sorted_IDS),size = sample_size,prob = prob))),]  )
    
  }  )#apply fx
  
  out <- do.call(rbind,trait_draw)
  rownames(out) <- NULL
  return(out)
  
}


##################################################################


#'@param tidy_traits Trait data in tidy format
#'@param sample_size Numeric (one number only)
#'@param community community data used to extract abundances
#'@param abd_biased "common" prefer common species, "rare" prefer rare species, "random" sample at random
#'@note random sampling ignores weight exponent
draw_traits_tidy_abd_biased <- function(tidy_traits,
                                    sample_size,
                                    community,
                                    abd_bias = "common",
                                    weight_exponent = 1){
  
  if(!abd_bias %in% c("common","rare","random")){
    
    stop('abd_bias should be one of "common","rare","random"')
  }  
  
  
  # #temp
  # output <- NULL
  # for( i in 1:length(unique(community$site))){
  # 
  #   x <- unique(community$site)[i]
  # 
  # 
  #   community_i <- community[which(community$site==x),]
  # 
  # 
  #   if(abd_bias == "common" ){
  # 
  #     community_i$weight <- community_i$abundance
  # 
  #   }
  # 
  #   if(abd_bias == "rare"){
  # 
  #     community_i$weight <-abs(community_i$abundance-max(community_i$abundance)+1)
  #   }
  # 
  #   if(abd_bias == "random"){
  # 
  #     community_i$weight <-1
  #   }
  # 
  # 
  #   #Multiply weight by exponent
  #   community_i$weight <- (community_i$weight/sum(community_i$weight))^weight_exponent
  # 
  # 
  #   weights <- merge(x = community_i,
  #                    y = tidy_traits[c("site","taxon","ID")])
  # 
  #   #probably not needed, but just to be safe
  #   weights <- unique(weights)
  # 
  # 
  #   #if number of IDS <= sample size, return them all
  #   if(nrow(weights) <= sample_size){
  #     return(tidy_traits[which(tidy_traits$ID %in% weights$ID),])
  #   }
  # 
  # 
  # 
  #   return( tidy_traits[which(tidy_traits$ID %in% resample(x = weights$ID,
  #                                                          size = sample_size,
  #                                                          replace = F,
  #                                                          prob = weights$weight)),]  )
  # 
  # }
  # 
  # #end temp
  trait_draw <- apply(X = unique(community["site"]),
                      MARGIN = 1,
                      FUN = function(x){
    #print(x)
                        
                        community_i <- community[which(community$site==x),]
                        
                        
                        if(abd_bias == "common" ){
                          
                          community_i$weight <- community_i$abundance
                          
                        }
                        
                        if(abd_bias == "rare"){
                          
                          community_i$weight <-abs(community_i$abundance-max(community_i$abundance)+1)
                        }
                        
                        if(abd_bias == "random"){
                          
                          community_i$weight <-1
                        }
                        
                        
                        #Multiply weight by exponent
                        community_i$weight <- (community_i$weight/sum(community_i$weight))^weight_exponent
                        
                        
                        weights <- merge(x = community_i,
                                         y = tidy_traits[c("site","taxon","ID")])
                        
                        #probably not needed, but just to be safe
                        weights <- unique(weights)
                        
                        #if number of IDS <= sample size, return them all
                        if(nrow(weights) <= sample_size){
                          return(tidy_traits[which(tidy_traits$ID %in% weights$ID),])
                        }
                        
                        return( tidy_traits[which(tidy_traits$ID %in% resample(x = weights$ID,
                                                                               size = sample_size,
                                                                               replace = F,
                                                                               prob = weights$weight)),]  )
  }  )#apply fx
  
  out <- do.call(rbind,trait_draw)
  rownames(out) <- NULL
  return(out)
  
}

