

#####################
resample <- function(x, ...) x[sample.int(length(x), ...)]

#####################

#'@param tidy_traits Trait data in tidy format
#'@param sample_size Numeric (one number only)
draw_traits_tidy <- function(tidy_traits,sample_size){
  
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


#Quick convenient function when convertin to species mean data
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
