#Load packages and data


###########################################
library(BIEN)
library(moments)
library(traitstrap)
library(devtools)
library(tidyr)

source("C:/Users/Brian/Desktop/current_projects/traitstrap/R/parametric_bs.R")
source("r_functions/draw_traits_tidy.R")


#Code commented out to keep track of origin of trait data
#traits<-read.csv("C:/Users/Brian/Desktop/current_projects/RMBL_traits/Trait_Data/rmbl_trait_data_master.csv")
#atraits<-traits[which(traits$source=="Amanda"),]
#rm(traits)
#Remove columns that are empty, no need to bother with them
#atraits <- atraits[,which(!is.na(atraits[1,]))]
#atraits$taxon<-gsub(pattern = "Artemesia", replacement = "Artemisia",x = atraits$taxon)#correct name error
#Prune trait data to needed columns
#atraits$SLA<-as.numeric(as.character(atraits$SLA)) 
#atraits<-atraits[c("site","taxon","leaf_area_mm2","dry_mass_mg","SLA_m2_kg","height","biomass_per_ind" )]
#saveRDS(object = atraits,file = "data/all_traits_unscaled_RMBL.rds")

atraits <- readRDS(file = "data/all_traits_unscaled_RMBL.rds")

#Removed 7% of records with any missing data
message(paste(round(nrow(na.omit(atraits))/nrow(atraits)*100),"percent complete data. Pruning incomplete rows."))
atraits <- na.omit(atraits)

#Add individual ID column and covert to long form
atraits$ID <- 1:nrow(atraits)


#Convert to tidy/skinny/long form
atraits <- gather(data = atraits,key = "trait","value",3:7)

#Get community data
atraits %>% group_by(taxon,site) %>% summarise(across(ID,~(length(unique(.x))),.names = "abundance"),.groups="drop") -> community



###########################################################################

# Set simulation parameters

n_to_sample <- c(1,2,3,4,5,6,7,8,9,10,25,40,50,100,200,500) #sample sizes (for each species) to draw from the full set of trait data
n_reps_trait <- 10 #controls the number of replicated draws for each  sample size
n_reps_boot <- 200 #number of bootstrap replicates to use
set.seed(2005) #set seed for reproducability.  2005 = Year Transformers: The Movie (cartoon version) is set.

  for( n in n_to_sample){
    for(t in 1:n_reps_trait){  
      
      
      #First simulate a draw of the relevant sample size
      traits_nt <- draw_traits_tidy(tidy_traits = atraits,sample_size =  n)
      
      #Get species mean traits
      species_means_nt <- samples_to_means(tidy_traits = traits_nt,level = "taxon")
      
      #Get species x site mean traits
      species_site_means_nt <- samples_to_means(tidy_traits = traits_nt,level = "taxon_by_site")
      
      #Trait imputation for distributions
      imputed_full <- 
       trait_impute(comm = community,
                   traits = traits_nt,
                   scale_hierarchy = "site",
                   global = T,
                   taxon_col = "taxon",
                   trait_col = "trait",
                   value_col = "value",
                   abundance_col = "abundance")
      
      #Trait imputation for species mean
      imputed_species_mean <- 
        trait_impute(comm = community,
                     traits = species_means_nt,
                     scale_hierarchy = NULL,
                     global = T,
                     taxon_col = "taxon",
                     trait_col = "trait",
                     value_col = "value",
                     abundance_col = "abundance")
      
      #Trait imputation for species x site mean
      imputed_species_x_site_mean <- 
        trait_impute(comm = community,
                     traits = species_site_means_nt,
                     scale_hierarchy = "site",
                     global = T,
                     taxon_col = "taxon",
                     trait_col = "trait",
                     value_col = "value",
                     abundance_col = "abundance")
      
      
      
      
      
    } #t trait rep  
  }# n sample size loop



###############################################################################


# VV Old code VV

#Simulate differing sample sizes and compare inferences


n_to_sample <- c(1,2,3,4,5,6,7,8,9,10,25,40,50,100,200,500) #sample sizes (for each species) to draw from the full set of trait data
n_reps_trait<-10 #controls the number of replicated draws for each  sample size
n_reps_boot <- 200 #number of bootstrap replicates to use

#assuming random sampling
#draw=draw of traits
#boot=boostrap within that draw of traits

set.seed(1)
for(s in 1:length(unique(community$site))){
  site<-as.character(unique(community$site))[s]
  
  
  for(i in n_to_sample){
    
    for(t in 1:n_reps_trait){
      
      traits_df <- draw_traits_tidy(tidy_traits =  atraits[which(atraits$site==site),which(colnames(atraits)!="site")],sample_size = i)  
      
      
      trait_means <- distribution_to_means(traits_df)
      traits_df_log <- traits_df
      traits_df_log[,2:ncol(traits_df_log)]  <- log(traits_df_log[,2:ncol(traits_df_log)])
      log_trait_means<-distribution_to_means(traits_df_log)
      
      out_dist_t_boot<-NULL
      out_dist_t_boot<-trait_distributions(number_replicates = n_reps_boot, 
                                           abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,
                                           trait_data = traits_df)
      out_dist_t_param <- NULL
      out_dist_t_param<-trait_distributions_parametric(number_replicates = n_reps_boot, 
                                                       abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,
                                                       trait_data = traits_df)
      out_dist_t_mean<-NULL
      out_dist_t_mean<-trait_distributions(number_replicates = n_reps_boot, 
                                           abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,
                                           trait_data = trait_means)
      
      
      #Log  
      out_dist_t_boot_log <- NULL
      out_dist_t_boot_log<-trait_distributions(number_replicates = n_reps_boot, 
                                               abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,
                                               trait_data = traits_df_log)
      out_dist_t_param_log<-NULL
      out_dist_t_param_log<-trait_distributions_parametric_manual(number_replicates = n_reps_boot, 
                                                                  abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,
                                                                  trait_data = traits_df_log,
                                                                  dist_type = "normal")
      out_dist_t_mean_log<-NULL
      out_dist_t_mean_log<-trait_distributions(number_replicates = n_reps_boot, 
                                               abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,
                                               trait_data = log_trait_means)
      
      
      
      for( l in 1: length(out_dist_t_boot)){
        
        #write output  
        write.csv(x = out_dist_t_boot[[l]],
                  file = paste("output_distributions/RMBL_random_sampling/RMBL",site,names(out_dist_t_boot)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
                  row.names = F  )
        
        
        write.csv(x = out_dist_t_mean[[l]],
                  file = paste("output_distributions/RMBL_random_sampling_mean_values/RMBL",site,names(out_dist_t_mean)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
                  row.names = F  )
        
        write.csv(x = out_dist_t_param[[l]],
                  file = paste("output_distributions/RMBL_random_sampling_parametric_bs/RMBL",site,names(out_dist_t_mean)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
                  row.names = F  )
        
        
        
        write.csv(x = out_dist_t_boot_log[[l]],
                  file = paste("output_distributions/RMBL_random_sampling_log/RMBL",site,names(out_dist_t_boot)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
                  row.names = F  )
        
        
        write.csv(x = out_dist_t_mean_log[[l]],
                  file = paste("output_distributions/RMBL_random_sampling_mean_log_values/RMBL",site,names(out_dist_t_mean)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
                  row.names = F  )
        
        write.csv(x = out_dist_t_param_log[[l]],
                  file = paste("output_distributions/RMBL_random_sampling_parametric_bs_log/RMBL",site,names(out_dist_t_mean)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
                  row.names = F  )
        
        
        
        #Is np mean within the CI?
        mean_in_95ci <- if(
          calc_ci(rowMeans(out_dist_t_boot[[l]]))$ci_min<=  mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(rowMeans(out_dist_t_boot[[l]]))$ci_max>=  mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        mean_in_100ci <- if(
          min(rowMeans(out_dist_t_boot[[l]]))<=  mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(rowMeans(out_dist_t_boot[[l]])) >=  mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        
        var_in_95ci <- if(
          calc_ci(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var))$ci_min<=  var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var))$ci_max>=  var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        var_in_100ci <- if(
          min(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var))<=  var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var)) >=  var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        skew_in_95ci <- if(
          calc_ci(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness))$ci_min<=  skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness))$ci_max>=  skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        skew_in_100ci <- if(
          min(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness))<=  skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness)) >=  skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        kurt_in_95ci <- if(
          calc_ci(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis))$ci_min<=  kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis))$ci_max>=  kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        kurt_in_100ci <- if(
          min(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis))<=  kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis)) >=  kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        
        #########################
        #Parametric CI
        
        
        
        mean_in_95ci_param <- if(
          calc_ci(rowMeans(out_dist_t_param[[l]]))$ci_min<=  mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(rowMeans(out_dist_t_param[[l]]))$ci_max>=  mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        mean_in_100ci_param <- if(
          min(rowMeans(out_dist_t_param[[l]]))<=  mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(rowMeans(out_dist_t_param[[l]])) >=  mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        
        var_in_95ci_param <- if(
          calc_ci(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = var))$ci_min<=  var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = var))$ci_max>=  var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        var_in_100ci_param <- if(
          min(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = var))<=  var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = var)) >=  var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        skew_in_95ci_param <- if(
          calc_ci(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = skewness))$ci_min<=  skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = skewness))$ci_max>=  skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        skew_in_100ci_param <- if(
          min(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = skewness))<=  skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = skewness)) >=  skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        kurt_in_95ci_param <- if(
          calc_ci(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = kurtosis))$ci_min<=  kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = kurtosis))$ci_max>=  kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        kurt_in_100ci_param <- if(
          min(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = kurtosis))<=  kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = kurtosis)) >=  kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        
        #####Log CIS
        
        #Is np mean within the CI?
        mean_in_95ci_log <- if(
          calc_ci(rowMeans(out_dist_t_boot_log[[l]]))$ci_min<=  mean(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(rowMeans(out_dist_t_boot_log[[l]]))$ci_max>=  mean(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        mean_in_100ci_log <- if(
          min(rowMeans(out_dist_t_boot_log[[l]]))<=  mean(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(rowMeans(out_dist_t_boot_log[[l]])) >=  mean(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        
        var_in_95ci_log <- if(
          calc_ci(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = var))$ci_min<=  var(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = var))$ci_max>=  var(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        var_in_100ci_log <- if(
          min(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = var))<=  var(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = var)) >=  var(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        skew_in_95ci_log <- if(
          calc_ci(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = skewness))$ci_min<=  skewness(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = skewness))$ci_max>=  skewness(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        skew_in_100ci_log <- if(
          min(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = skewness))<=  skewness(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = skewness)) >=  skewness(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        kurt_in_95ci_log <- if(
          calc_ci(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = kurtosis))$ci_min<=  kurtosis(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = kurtosis))$ci_max>=  kurtosis(atraits[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        kurt_in_100ci_log <- if(
          min(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = kurtosis))<=  kurtosis(atraits[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = kurtosis)) >=  kurtosis(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        
        #########################
        #Parametric CI
        
        
        
        mean_in_95ci_param_log <- if(
          calc_ci(rowMeans(out_dist_t_param_log[[l]]))$ci_min<=  mean(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(rowMeans(out_dist_t_param_log[[l]]))$ci_max>=  mean(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        mean_in_100ci_param_log <- if(
          min(rowMeans(out_dist_t_param_log[[l]]))<=  mean(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(rowMeans(out_dist_t_param_log[[l]])) >=  mean(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        
        var_in_95ci_param_log <- if(
          calc_ci(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = var))$ci_min<=  var(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = var))$ci_max>=  var(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        var_in_100ci_param_log <- if(
          min(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = var))<=  var(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = var)) >=  var(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        skew_in_95ci_param_log <- if(
          calc_ci(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = skewness))$ci_min<=  skewness(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = skewness))$ci_max>=  skewness(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        skew_in_100ci_param_log <- if(
          min(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = skewness))<=  skewness(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = skewness)) >=  skewness(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        kurt_in_95ci_param_log <- if(
          calc_ci(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = kurtosis))$ci_min<=  kurtosis(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & calc_ci(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = kurtosis))$ci_max>=  kurtosis(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        kurt_in_100ci_param_log <- if(
          min(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = kurtosis))<=  kurtosis(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T)
          & max(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = kurtosis)) >=  kurtosis(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean)[l])],na.rm = T) 
        ){1}else(0)
        
        
        
        
        
        
        output_l_mean<-cbind(i,#sample size
                             t,#rep number
                             nrow(traits_df),#number sampled
                             names(out_dist_t_mean)[l], #trait
                             as.character(site), #site
                             "mean", #moment
                             mean(rowMeans(out_dist_t_boot[[l]])),#boot mean
                             mean(rowMeans(out_dist_t_mean[[l]]) ), #cwm mean
                             mean(rowMeans(out_dist_t_param[[l]]) ), #paraboot mean
                             mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T), #actual mean
                             (mean(rowMeans(out_dist_t_boot[[l]]))-mean(rowMeans(out_dist_t_mean[[l]]) ))/sd(rowMeans(out_dist_t_boot[[l]])),#SESmean
                             (mean(rowMeans(out_dist_t_boot[[l]]))-mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T))/sd(rowMeans(out_dist_t_boot[[l]])),#SESactual
                             mean_in_95ci,
                             mean_in_100ci,
                             mean_in_95ci_param,
                             mean_in_100ci_param,
                             
                             mean(rowMeans(out_dist_t_boot_log[[l]])),#boot mean
                             mean(rowMeans(out_dist_t_mean_log[[l]]) ), #cwm mean
                             mean(rowMeans(out_dist_t_param_log[[l]]) ), #paraboot mean
                             mean(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean_log)[l])],na.rm = T), #actual mean
                             (mean(rowMeans(out_dist_t_boot_log[[l]]))-mean(rowMeans(out_dist_t_mean_log[[l]]) ))/sd(rowMeans(out_dist_t_boot_log[[l]])),#SESmean
                             (mean(rowMeans(out_dist_t_boot_log[[l]]))-mean(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean_log)[l])],na.rm = T))/sd(rowMeans(out_dist_t_boot_log[[l]])),#SESactual
                             mean_in_95ci_log,
                             mean_in_100ci_log,
                             mean_in_95ci_param_log,
                             mean_in_100ci_param_log
                             
                             #included?
        )#mean output
        
        output_l_var<-cbind(i,#sample size
                            t,#rep number 
                            nrow(traits_df),#number sampled
                            names(out_dist_t_mean)[l], #trait
                            as.character(site), #site
                            "variance", #moment
                            mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var)),#boot mean
                            mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = var)), #cwm mean
                            mean(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = var)), #param mean
                            var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T), #actual mean
                            (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var))-mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = var)))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var)),#SESmean
                            (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var))-var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var)),#SESactual
                            var_in_95ci,
                            var_in_100ci,
                            var_in_95ci_param,
                            var_in_100ci_param,
                            
                            mean(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = var)),#boot mean
                            mean(apply(X = out_dist_t_mean_log[[l]], MARGIN = 1,FUN = var)), #cwm mean
                            mean(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = var)), #param mean
                            var(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean_log)[l])],na.rm = T), #actual mean
                            (mean(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = var))-mean(apply(X = out_dist_t_mean_log[[l]], MARGIN = 1,FUN = var)))/sd(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = var)),#SESmean
                            (mean(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = var))-var(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean_log)[l])],na.rm = T))/sd(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = var)),#SESactual
                            var_in_95ci_log,
                            var_in_100ci_log,
                            var_in_95ci_param_log,
                            var_in_100ci_param_log
                            
        )#var output
        
        
        output_l_skew<-cbind(i,#sample size
                             t,#rep number
                             nrow(traits_df),#number sampled
                             names(out_dist_t_mean)[l], #trait
                             as.character(site), #site
                             "skewness", #moment
                             mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness)),#boot mean
                             mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = skewness)), #cwm mean
                             mean(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = skewness)), #param mean
                             skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T), #actual mean
                             (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness))-mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = skewness)))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness)),#SESmean
                             (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness))-skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness)),#SESactual
                             skew_in_95ci,
                             skew_in_100ci,
                             skew_in_95ci_param,
                             skew_in_100ci_param,
                             
                             mean(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = skewness)),#boot mean
                             mean(apply(X = out_dist_t_mean_log[[l]], MARGIN = 1,FUN = skewness)), #cwm mean
                             mean(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = skewness)), #param mean
                             skewness(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean_log)[l])],na.rm = T), #actual mean
                             (mean(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = skewness))-mean(apply(X = out_dist_t_mean_log[[l]], MARGIN = 1,FUN = skewness)))/sd(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = skewness)),#SESmean
                             (mean(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = skewness))-skewness(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean_log)[l])],na.rm = T))/sd(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = skewness)),#SESactual
                             skew_in_95ci_log,
                             skew_in_100ci_log,
                             skew_in_95ci_param_log,
                             skew_in_100ci_param_log
                             
        )#var output
        
        output_l_kurt<-cbind(i,#sample size
                             t,#rep number
                             nrow(traits_df),#number sampled
                             names(out_dist_t_mean)[l], #trait
                             as.character(site), #site
                             "kurtosis", #moment
                             mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis)),#boot mean
                             mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = kurtosis)), #cwm mean
                             mean(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = kurtosis)), #parametric mean
                             kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T), #actual mean
                             (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis))-mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = kurtosis)))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis)),#SESmean
                             (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis))-kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis)),#SESactual
                             kurt_in_95ci,
                             kurt_in_100ci,
                             kurt_in_95ci_param,
                             kurt_in_100ci_param,
                             
                             mean(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = kurtosis)),#boot mean
                             mean(apply(X = out_dist_t_mean_log[[l]], MARGIN = 1,FUN = kurtosis)), #cwm mean
                             mean(apply(X = out_dist_t_param_log[[l]], MARGIN = 1,FUN = kurtosis)), #parametric mean
                             kurtosis(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean_log)[l])],na.rm = T), #actual mean
                             (mean(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = kurtosis))-mean(apply(X = out_dist_t_mean_log[[l]], MARGIN = 1,FUN = kurtosis)))/sd(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = kurtosis)),#SESmean
                             (mean(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = kurtosis))-kurtosis(atraits_log[which(atraits_log$site==site),which(colnames(atraits_log)==names(out_dist_t_mean_log)[l])],na.rm = T))/sd(apply(X = out_dist_t_boot_log[[l]], MARGIN = 1,FUN = kurtosis)),#SESactual
                             kurt_in_95ci_log,
                             kurt_in_100ci_log,
                             kurt_in_95ci_param_log,
                             kurt_in_100ci_param_log
                             
        )#var output
        
        simulation_output<-rbind(simulation_output,output_l_mean,output_l_var,output_l_skew,output_l_kurt)
        rm(output_l_kurt,output_l_mean,output_l_skew,output_l_var)
        
        
      }#l loop  
    }#t n reps trait
  }#i n to sample
}#s site
rm(i,l,s,t,site,out_dist_t_boot,out_dist_t_mean,trait_means,traits_df)
simulation_output<-as.data.frame(simulation_output)
colnames(simulation_output)<-c("sample_size","replicate","total_samples","trait","site","moment",
                               "boot_mean","cwm_mean","parametric_mean","actual_mean","SES_boot_v_cwm","SES_boot_v_actual","in_95ci","in_100ci","in_95ci_para","in_100_ci_para",
                               "boot_mean_log","cwm_mean_log","parametric_mean_log","actual_mean_log","SES_boot_v_cwm_log","SES_boot_v_actual_log","in_95ci_log","in_100ci_log","in_95ci_para_log","in_100_ci_para_log")








