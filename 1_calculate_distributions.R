#Calculate actual and inferred distributions

source("r_functions/trait_distribution_fx.R")


atraits<-readRDS("data/all_traits_RMBL.rds")
mean_traits<-readRDS("data/species_mean_traits_RMBL.rds")
community<-readRDS("data/species_presence_RMBL.rds")

###########################################################

#record actual distributions

for(i in 1:length(unique(atraits$site))){
site_i<-as.character(unique(atraits$site)[i]  )
data_i<-atraits[which(atraits$site==site_i),]  
write.csv(x = data_i$leaf_area_mm2,file = paste("output_distributions/RMBL_observed/RMBL",site_i,"leaf_area_mm2","observed.csv",sep = ".")  )
write.csv(x = data_i$dry_mass_mg,file = paste("output_distributions/RMBL_observed/RMBL",site_i,"dry_mass_mg","observed.csv",sep = ".")  )
write.csv(x = data_i$SLA_m2_kg,file = paste("output_distributions/RMBL_observed/RMBL",site_i,"SLA_m2_kg","observed.csv",sep = ".")  )
write.csv(x = data_i$height,file = paste("output_distributions/RMBL_observed/RMBL",site_i,"height","observed.csv",sep = ".")  )  
write.csv(x = data_i$biomass_per_ind,file = paste("output_distributions/RMBL_observed/RMBL",site_i,"biomass_per_ind","observed.csv",sep = ".")  )  
}

