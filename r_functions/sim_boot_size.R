
########################################################################

library(e1071)
library(tidyverse)
source("r_functions/parametric_bs.R")

sim_boot_size <- function(tidy_traits,
                            community,
                            n_to_sample = (1:22)^2, #sample sizes
                            n_reps_trait = 10, #controls the number of replicated draws for each  sample size
                            n_reps_boot = 200, #number of bootstrap replicates to use
                            boot_sample_size = c(200, 400, 800, 1600, 3200), #number of individuals to draw from distributions
                            seed = 2005, #set seed for reproducibility.  2005 = Year Transformers: The Movie (cartoon version) is set.
                            prob = NULL #Probability for biased sampling.  If NULL, sampling will be random.
){
  
  
  
  set.seed(seed = seed)  
  output<-NULL
  
  for( n in 1:length(boot_sample_size)){
    for(z in 1:length(n_to_sample)){
    for(t in 1:n_reps_trait){  
    message("starting sample size ",n, " of ",length(boot_sample_size), ": trait draw ", t, " of ", n_reps_trait)  
      
      #First simulate a draw of the relevant sample size
      if (is.null(prob)){
        
        traits_nt <- draw_traits_tidy(tidy_traits = tidy_traits,
                                      sample_size =  n_to_sample[z])  
        
      }else{
        
        traits_nt <- draw_traits_tidy_large(tidy_traits = tidy_traits,
                                            sample_size =  n_to_sample[z],
                                            focal_trait = "leaf_area_mm2",
                                            prob = prob)
        
      }
      
      
      
      #Get species mean traits
      species_means_nt <- samples_to_means(tidy_traits = traits_nt,
                                           level = "taxon")
      
      species_means_nt$site <- "ALL" #Note: adding a "dummy" site here so traitstrap will use global data but still return site-level moments
      
      #Get species x site mean traits
      species_site_means_nt <- samples_to_means(tidy_traits = traits_nt,
                                                level = "taxon_by_site")
      
      #Trait imputation for distributions
      imputed_full <- 
        trait_impute(comm = community,
                     traits = traits_nt,
                     scale_hierarchy = "site",
                     global = T,
                     taxon_col = "taxon",
                     trait_col = "trait",
                     value_col = "value",
                     abundance_col = "abundance",
                     min_n_leaves = 1)
      
      #Trait imputation for species mean
      imputed_species_mean <- 
        trait_impute(comm = community,
                     traits = species_means_nt,
                     scale_hierarchy = "site",
                     global = T,
                     taxon_col = "taxon",
                     trait_col = "trait",
                     value_col = "value",
                     abundance_col = "abundance",
                     min_n_leaves = 1)
      
      #Trait imputation for species x site mean
      imputed_species_x_site_mean <- 
        trait_impute(comm = community,
                     traits = species_site_means_nt,
                     scale_hierarchy = "site",
                     global = T,
                     taxon_col = "taxon",
                     trait_col = "trait",
                     value_col = "value",
                     abundance_col = "abundance",
                     min_n_leaves = 1)
      
      #Get Non-parametric moments
      np_results_nt <- 
        trait_np_bootstrap(imputed_traits = imputed_full,
                           nrep = n_reps_boot,
                           sample_size = boot_sample_size[n])
      np_results_nt <-
        trait_summarise_boot_moments(bootstrap_moments = np_results_nt,
                                     parametric = F)
      
      #Get CWM species
      cwm_species_results_nt <- 
        trait_np_bootstrap(imputed_traits = imputed_species_mean,
                           nrep = n_reps_boot,
                           sample_size = boot_sample_size[n])
      
      cwm_species_results_nt <-
        trait_summarise_boot_moments(bootstrap_moments = cwm_species_results_nt,
                                     parametric = F)
      
      #Get CWM species x site
      cwm_species_x_site_results_nt <- 
        trait_np_bootstrap(imputed_traits = imputed_species_x_site_mean,
                           nrep = n_reps_boot,
                           sample_size = boot_sample_size[n])
      
      cwm_species_x_site_results_nt <-
        trait_summarise_boot_moments(bootstrap_moments = cwm_species_x_site_results_nt,
                                     parametric = F)
      
      #Get parametric moments
      pbs_results_nt <-
        trait_fit_distributions(imputed_traits = imputed_full,
                                distribution_type = "normal")
      
      pbs_results_nt <-
        trait_parametric_bootstrap(fitted_distributions = pbs_results_nt,
                                   nrep = n_reps_boot,
                                   sample_size = boot_sample_size[n])  
      
      pbs_results_nt <-
        trait_summarise_boot_moments(bootstrap_moments = pbs_results_nt,
                                     parametric = F)
      
      
      #oldschool parametric moments
      
      samples_per_abundance <- 1
      
      og_pbs_results_nt <-
      trait_parametric_bootstrap_individuals(imputed_traits = imputed_full,
                                             distribution_type = "normal",
                                             nrep = n_reps_boot,
                                             samples_per_abundance = samples_per_abundance)
    
      
      og_pbs_results_nt <-
        trait_summarise_boot_moments(bootstrap_moments = og_pbs_results_nt,
                                     parametric = F)
      

      output <- rbind(output,rbind(cbind(method = "old parametric bs", trait_sample_size = n_to_sample[z], boot_sample_size = (sum(community$abundance)*samples_per_abundance), og_pbs_results_nt),
                                   cbind(method = "nonparametric bs", trait_sample_size = n_to_sample[z], boot_sample_size = boot_sample_size[n], np_results_nt),
                                   cbind(method = "parametric bs", trait_sample_size = n_to_sample[z], boot_sample_size = boot_sample_size[n], pbs_results_nt),
                                   cbind(method = "global cwm", trait_sample_size = n_to_sample[z], boot_sample_size = boot_sample_size[n], cwm_species_results_nt),
                                   cbind(method = "site-specic CWM", trait_sample_size = n_to_sample[z], boot_sample_size = boot_sample_size[n], cwm_species_x_site_results_nt))
      )
      
      
    } #t trait rep  
    } #z sample size loop
  }# n sample size loop
  
  
  #cleanup
  rm(cwm_species_results_nt,cwm_species_x_site_results_nt,imputed_full,imputed_species_mean,imputed_species_x_site_mean,
     np_results_nt,pbs_results_nt,species_means_nt,species_site_means_nt,traits_nt,n,t)
  
  #Append true moments to data
  
  #First, calculate true moments for each site x trait
  
  tidy_traits %>% group_by(site,trait) %>% summarise(true_mean=mean(value),
                                                     true_variance=var(value),
                                                     true_skewness = skewness(value),
                                                     true_kurtosis = kurtosis(value)) -> true_moments
  
  #Next, append true moments to output data for convenience
  output <- merge(x = output,y = true_moments,by = c("site","trait"))
  
  #cleanup
  rm(true_moments)
  
  return(output)
} #end fx
