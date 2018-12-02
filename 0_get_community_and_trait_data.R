#Script to assess performance of difference methods for calculating trait distributions given different sampling schemes
#Brian Maitner 18-10-2018 -> ?

####################################################################


#Notes





####################################################################

#load packages, data

library(BIEN)
library(moments)

traits<-read.csv("C:/Users/Brian/Desktop/current_projects/RMBL_traits/Trait_Data/rmbl_trait_data_master.csv")
atraits<-traits[which(traits$source=="Amanda"),]
rm(traits)

#Remove columns that are empty, no need to bother with them
atraits<-atraits[,which(!is.na(atraits[1,]))]
atraits$taxon<-gsub(pattern = "Artemesia", replacement = "Artemisia",x = atraits$taxon)#correct name error


#Species mean traits
species_list <- as.character(unique(atraits$taxon))
#sort(species_list)
atraits$SLA<-as.numeric(as.character(atraits$SLA)) 
atraits<-atraits[c("site","taxon","leaf_area_mm2","dry_mass_mg","SLA_m2_kg","height","biomass_per_ind" )]
saveRDS(object = atraits,file = "data/all_traits_unscaled_RMBL.rds")



#Scale traits
atraits$leaf_area_mm2<-scale(atraits$leaf_area_mm2)  
atraits$dry_mass_mg<-scale(atraits$dry_mass_mg)  
atraits$SLA_m2_kg<-scale(atraits$SLA_m2_kg)  
atraits$biomass_per_ind<-scale(atraits$biomass_per_ind)    
atraits$height <- scale(atraits$height)



data_out<-NULL
for(i in 1:length(species_list)){
  taxon <- as.character( species_list[i] )
  
  area_mean<-mean(atraits$leaf_area_mm2[which(atraits$taxon==taxon)],na.rm = T)  
  dry_mass_mean<-mean(atraits$dry_mass_mg[which(atraits$taxon==taxon)],na.rm = T)  
  sla_mean<-mean(atraits$SLA_m2_kg[which(atraits$taxon==taxon)],na.rm = T)  
  biomass_mean<-mean(atraits$biomass_per_ind[which(atraits$taxon==taxon)],na.rm = T)    
  height_mean <-mean(atraits$height[which(atraits$taxon==taxon)],na.rm = T)
  
  out_i<-cbind(taxon,area_mean,dry_mass_mean,sla_mean,biomass_mean,height_mean)
  data_out <- rbind(data_out,out_i)
  
  
}

data_out<-as.data.frame(data_out)
rm(i,out_i,area_mean,biomass_mean,dry_mass_mean,sla_mean,taxon,height_mean)

species_level_traits<-data_out
rm(data_out,species_list)

#colnames(atraits)

atraits<-atraits[c("site","taxon","leaf_area_mm2","dry_mass_mg","SLA_m2_kg","height","biomass_per_ind" )]

#community data

species_presence_tidy<-unique(atraits[c('taxon',"site")])
species_presence_tidy$abundance<-NA

for(i in 1:nrow(species_presence_tidy)){
  taxon<-species_presence_tidy$taxon[i]
  site<-species_presence_tidy$site[i]
  species_presence_tidy$abundance[i]<-length(which(atraits$taxon==taxon & atraits$site==site) ) 
}
rm(i,taxon,site)



saveRDS(object = atraits,file = "data/all_traits_RMBL.rds")
saveRDS(object = species_level_traits,file = "data/species_mean_traits_RMBL.rds")
saveRDS(object = species_presence_tidy,file = "data/species_presence_RMBL.rds")



##########################################
##########################################
