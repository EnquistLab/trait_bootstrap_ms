library(e1071)
library(tidyverse)

# Note that this applies sample sizes at the species x site level.
# This function assumes that one ID is one sample.
# So assigning IDs to individuals could return multiple samples per individual.
# Assigning IDs to traits taken from the same organ returns 
# multiple traits form the same organ (e.g. leaf)


sim_sample_size_abundance_biased <- function(tidy_traits,
                                              community,
                                              n_to_sample = (1:22)^2, #sample sizes
                                              n_reps_trait = 10, #controls the number of replicated draws for each  sample size
                                              n_reps_boot = 200, #number of bootstrap replicates to use
                                              seed = 2005, #set seed for reproducibility.  2005 = Year Transformers: The Movie (cartoon version) is set.
                                              distribution_type = "normal", #distribution for fitting the parametric
                                              min_n_in_sample = 1,
                                              abd_bias = "common", #one of "common","rare",or"random"
                                              weight_exponent = 2 # weights are scaled to this exponent
){
  
  
  set.seed(seed = seed)  
  output<-NULL
  
  for( n in n_to_sample){
    for(t in 1:n_reps_trait){  
      
      
      #First simulate a draw of the relevant sample size

        traits_nt <- draw_traits_tidy_abd_biased(tidy_traits = tidy_traits,
                                                 sample_size = n,
                                                 community = community,
                                                 abd_bias = abd_bias,
                                                 weight_exponent = weight_exponent)
        

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
                     min_n_in_sample = min_n_in_sample)
      
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
                     min_n_in_sample = 1)
      
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
                     min_n_in_sample = 1)
      
      #Get Non-parametric moments
      np_results_nt <- 
        trait_np_bootstrap(imputed_traits = imputed_full,
                           nrep = n_reps_boot,
                           sample_size = 200)
      
      np_results_nt <-
        trait_summarise_boot_moments(bootstrap_moments = np_results_nt,
                                     parametric = F)
      
      #Get CWM species
      cwm_species_results_nt <- 
        trait_np_bootstrap(imputed_traits = imputed_species_mean,
                           nrep = n_reps_boot,
                           sample_size = 200)
      
          cwm_species_results_nt <-
            trait_summarise_boot_moments(bootstrap_moments = cwm_species_results_nt,
                                         parametric = F)
          
      #Get CWM species x site
      cwm_species_x_site_results_nt <- 
        trait_np_bootstrap(imputed_traits = imputed_species_x_site_mean,
                           nrep = n_reps_boot,
                           sample_size = 200)
          cwm_species_x_site_results_nt <-
            trait_summarise_boot_moments(bootstrap_moments = cwm_species_x_site_results_nt,
                                         parametric = F)
      
      #Get parametric moments
      pbs_results_nt <-
        trait_fit_distributions(imputed_traits = imputed_full,
                                distribution_type = distribution_type)
      
      pbs_results_nt <-
        trait_parametric_bootstrap(fitted_distributions = pbs_results_nt,
                                   nrep = n_reps_boot,
                                   sample_size = 200)  
      
      pbs_results_nt <-
        trait_summarise_boot_moments(bootstrap_moments = pbs_results_nt,
                                     parametric = F)
      
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
  
  tidy_traits %>%
    group_by(site,trait) %>%
    summarise(true_mean = mean(value),
              true_variance = var(value),
              true_skewness = skewness(value),
              true_kurtosis = e1071::kurtosis(value)) -> true_moments
  
  #Next, append true moments to output data for convenience
  output <- merge(x = output,y = true_moments,by = c("site","trait"))
  
  #cleanup
  rm(true_moments)
  
  return(output)
} #end fx
