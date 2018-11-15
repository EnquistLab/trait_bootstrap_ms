#Calculate actual and inferred distributions

source("r_functions/trait_distribution_fx.R")
source("r_functions/draw_traits.R")


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
rm(data_i,site_i,i)
}


###########################################################

#Non-parametric bootstrapped distributions


n_to_sample<-c(1,2,3,4,5,6,7,8,9,10,50,100,200,500)
n_reps_trait<-10
n_reps_boot <- 200

#assuming random sampling
#draw=draw of traits
  #boot=boostrap within that draw of traits

set.seed(seed = 1)
for(s in 1:length(unique(community$site))){
site<-unique(community$site)[s]


for(i in n_to_sample){
  
  for(t in 1:n_reps_trait){
    
    traits_df<-draw_traits_random(traits_df = atraits[which(atraits$site==site),which(colnames(atraits)!="site")],sample_size = i)  
    out_dist_n<-trait_distributions(number_replicates = n_reps_boot, abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,trait_data = traits_df)
    
    for( l in 1: length(out_dist_n)){
    
    #write output  
    write.csv(x = out_dist_n[[l]],
              file = paste("output_distributions/RMBL_random_sampling/RMBL",site,names(out_dist_n)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
              row.names = F  )

    }#l loop  
  }#t n reps trait
}#i n to sample
}#s site
rm(s,i,l,t,out_dist_n)


###################################

#Mean traits

n_to_sample<-c(1,2,3,4,5,6,7,8,9,10,50,100,200,500)
n_reps_trait<-10
n_reps_boot <- 200

#assuming random sampling
#draw=draw of traits
#boot=boostrap within that draw of traits

for(s in 1:length(unique(community$site))){
  site<-unique(community$site)[s]
  
  
  for(i in n_to_sample){
    
    for(t in 1:n_reps_trait){
      
      traits_df<-draw_traits_random_means(traits_df = atraits[which(atraits$site==site),which(colnames(atraits)!="site")],sample_size = i)  
      
      
      
      out_dist_n <- trait_distributions(number_replicates = n_reps_boot, 
                                        abundance_data = community[c("taxon","abundance")][which(community$site==site),]  , 
                                        trait_data = traits_df)
  
      
          
      for( l in 1: length(out_dist_n)){
        
        #write output  
        
        
        
        write.csv(x = out_dist_n[[l]],
                  file = paste("output_distributions/RMBL_random_sampling_mean_values/RMBL",site,names(out_dist_n)[l],paste("n_",i,sep = ""),"draw",t,sep = ".")  ,
                  row.names = F)
        
        
        
      }#l loop  
      
      
      
    }#t n reps trait
    
    
    
  }#i n to sample
  
  
}#s site

rm(s,i,l,t,out_dist_n)








