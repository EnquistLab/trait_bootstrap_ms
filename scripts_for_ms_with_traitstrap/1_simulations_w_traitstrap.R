#Load packages and data


###########################################
library(BIEN)
library(moments)
library(traitstrap)
library(devtools)
library(tidyr)
library(dplyr)
library(ggplot2)

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

#correct unit error
atraits$dry_mass_mg[which(atraits$dry_mass_mg==7000)] <- 7

#Add LMA and remove SLA
atraits$LMA_mg_mm2 <- atraits$dry_mass_mg/atraits$leaf_area_mm2
atraits <- atraits[grep(pattern = "SLA",x = colnames(atraits),invert = T)]

#Add individual ID column and covert to long form
atraits$ID <- 1:nrow(atraits)


#Convert to tidy/skinny/long form
atraits <- gather(data = atraits,key = "trait","value",3:7)

#Get community data
atraits %>% group_by(taxon,site) %>% summarise(across(ID,~(length(unique(.x))),.names = "abundance"),.groups="drop") -> community

#log transform
ggplot(data = atraits,mapping = aes(x=value))+geom_histogram()+facet_wrap(trait~site,scales = "free")
atraits$value <- log10(atraits$value)
ggplot(data = atraits,mapping = aes(x=value))+geom_histogram()+facet_wrap(trait~site,scales = "free")






###########################################################################

# Set simulation parameters


n_to_sample <- (1:22)^2 #sample sizes
n_reps_trait <- 10 #controls the number of replicated draws for each  sample size
n_reps_boot <- 200 #number of bootstrap replicates to use
set.seed(2005) #set seed for reproducibility.  2005 = Year Transformers: The Movie (cartoon version) is set.
output<-NULL

  for( n in n_to_sample){
    for(t in 1:n_reps_trait){  
      
      
      #First simulate a draw of the relevant sample size
      traits_nt <- draw_traits_tidy(tidy_traits = atraits,sample_size =  n)
      
      #Get species mean traits
      species_means_nt <- samples_to_means(tidy_traits = traits_nt,level = "taxon")
      species_means_nt$site <- "ALL" #Note: adding a "dummy" site here so traitstrap will use global data but still return site-level moments
      
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
                     scale_hierarchy = "site",
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
      
      #Get Non-parametric moments
      np_results_nt <- 
      trait_np_bootstrap(imputed_traits = imputed_full,
                         nrep = n_reps_boot,
                         sample_size = 200)
      np_results_nt <-
        trait_summarise_boot_moments(BootstrapMoments = np_results_nt)
      
      #Get CWM species
      cwm_species_results_nt <- 
        trait_np_bootstrap(imputed_traits = imputed_species_mean,
                           nrep = n_reps_boot,
                           sample_size = 200)
      
      cwm_species_results_nt <-
        trait_summarise_boot_moments(BootstrapMoments = cwm_species_results_nt)
      
      #Get CWM species x site
      cwm_species_x_site_results_nt <- 
        trait_np_bootstrap(imputed_traits = imputed_species_x_site_mean,
                           nrep = n_reps_boot,
                           sample_size = 200)
      
      cwm_species_x_site_results_nt <-
        trait_summarise_boot_moments(BootstrapMoments = cwm_species_x_site_results_nt)
      
      #Get parametric moments
      pbs_results_nt <-
        trait_parametric_bootstrap(imputed_traits = imputed_full,
                                   distribution_type = "normal",
                                   nrep = n_reps_boot,
                                   samples_per_abundance = 10) #note that this sampling is a bit different
        
      
      pbs_results_nt <-
        trait_summarise_boot_moments(BootstrapMoments = pbs_results_nt)
      
      output <- rbind(output,rbind(cbind(method = "nonparametric bs", sample_size = n, np_results_nt),
            cbind(method = "parametric bs", sample_size = n, pbs_results_nt),
            cbind(method = "global cwm", sample_size = n, cwm_species_results_nt),
            cbind(method = "site-specic CWM", sample_size = n, cwm_species_x_site_results_nt))
            )
      
    
    } #t trait rep  
  }# n sample size loop


#cleanup
rm(cwm_species_results_nt,cwm_species_x_site_results_nt,imputed_full,imputed_species_mean,imputed_species_x_site_mean,
   np_results_nt,pbs_results_nt,species_means_nt,species_site_means_nt,traits_nt,n,t)

#Append true moments to data

#First, calculate true moments for each site x trait

atraits %>% group_by(site,trait) %>% summarise(true_mean=mean(value),
                                               true_variance=var(value),
                                               true_skewness = skewness(value),
                                               true_kurtosis = kurtosis(value)) -> true_moments

#Next, append true moments to output data for convenience
output <- merge(x = output,y = true_moments,by = c("site","trait"))

#cleanup
rm(true_moments)

#save output
saveRDS(object = output,file = "output_data/simulation_results.RDS")



###############################################################################

#Global vs local should be a separate simulation


#Get global data
traits <- unique(atraits$trait)
taxa <- unique(atraits$taxon)

BIEN_trait_list()

#needed:  
#"leaf_area_mm2"   
#"dry_mass_mg"     
#"height"          
#"biomass_per_ind" 
#"LMA_mg_mm2"     

BIEN_trait_means <- lapply(X = c("leaf area","leaf dry mass","whole plant height","leaf area per leaf dry mass"),FUN = function(x){BIEN_trait_mean(species = taxa,trait = x)})
BIEN_trait_means <- do.call(rbind,BIEN_trait_means) 
table(BIEN_trait_means$level_used)#Very little data at species level
BIEN_trait_means$mean_value <- as.numeric(as.character(BIEN_trait_means$mean_value))

hist(log10(1/BIEN_trait_means$mean_value[which(BIEN_trait_means$trait=="leaf area per leaf dry mass")]))
hist(log10(atraits$value[which(atraits$trait=="LMA_mg_mm2")]))

unique(atraits$trait)
