
get_consensus_family <- function(species_list,taxon="taxon"){
  
  species_list <- 
    species_list %>% 
    group_by(taxon) %>% 
    mutate(genus = strsplit(as.character(.data[[taxon]]),split = " ")[[1]][1],
           consensus_family = NA)
  
  species_from_bien <- 
    BIEN::BIEN_taxonomy_genus(unique(species_list$genus))
  
  for(i in 1:length(unique(species_list$genus))) {
    
    #Print progress, but not all the time
    if((i/10)%%1==0){message(paste(round(i/length(unique(species_list$genus))*100),"percent done"))}
    genus_i <- unique(species_list$genus)[i]
    fams_i <- species_from_bien$scrubbed_family[which(species_from_bien$scrubbed_genus == genus_i)]
    
    if(length(fams_i) == 0){
      next #just skip it so that it maintains an NA and we can ignore it later  
      
    }
    
    if(length(unique(fams_i)) == 1) {
      
      species_list$consensus_family[which(species_list$genus==genus_i)] <- unique(fams_i)
      next
    }
    
    if(length(unique(fams_i)) > 1){
      fam_table_i <- as.data.frame(table(fams_i))
      max_freq <- max(fam_table_i$Freq)  
      
      #if multiple options match the match leave NA
      
      if(length(which(fam_table_i$Freq == max_freq)) > 1){next}
      
      
      #if one is higher than the rest, use it
      if(length(which(fam_table_i$Freq == max_freq)) == 1){
        species_list$consensus_family[which(species_list$genus == genus_i)] <- 
          as.character(fam_table_i$fams_i[which(fam_table_i$Freq == max_freq)])
        next
      }
      
      
      #If nothing has happened throw an error because it should have    
      stop("something weird happened, write more code")  
      
    }#if multiple options
    
  }#for loop
  
  
  return(species_list)
  
  
}#end fx
