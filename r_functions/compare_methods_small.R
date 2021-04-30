###function
#input:
#dataset
#cwm, site cwm, parametric, nonparametric

compare_methods <- function(tidy_traits,
                            community,
                            distribution_type = "normal"){
  
  traits_nt <- tidy_traits
  
  #Get species mean traits
  species_means_nt <- samples_to_means(tidy_traits = traits_nt,
                                       level = "taxon")
  
  species_means_nt$site <- "ALL" #Note: adding a "dummy" site here so traitstrap will use global data but still return site-level moments
  
  #Get species x site mean traits
  species_site_means_nt <- samples_to_means(tidy_traits = traits_nt,
                                            level = "taxon_by_site")
  
  #if family and genus info has been provided, use it, otherwise, just family
  if(is.null(traits_nt$family)){
    
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
                   min_n_in_sample = 1)
    
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
    
    
  }else{
    
    
    #add taxonomic info to mean values
    taxonomy <- unique(traits_nt[c("taxon","genus","family")])
    taxonomy <- na.omit(taxonomy)
    
    species_means_nt <- merge(x = species_means_nt,
                              y = taxonomy)
    
    species_site_means_nt <- 
      merge(x = species_site_means_nt,
            y = taxonomy)
    
    
    
    #Trait imputation for distributions
    imputed_full <- 
      trait_impute(comm = community,
                   traits = traits_nt,
                   scale_hierarchy = "site",
                   global = T,
                   taxon_col = c("taxon","genus","family"),
                   trait_col = "trait",
                   value_col = "value",
                   abundance_col = "abundance",
                   min_n_in_sample = 1)
    
    #Trait imputation for species mean
    imputed_species_mean <- 
      trait_impute(comm = community,
                   traits = species_means_nt,
                   scale_hierarchy = "site",
                   global = T,
                   taxon_col = c("taxon","genus","family"),
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
                   taxon_col = c("taxon","genus","family"),
                   trait_col = "trait",
                   value_col = "value",
                   abundance_col = "abundance",
                   min_n_in_sample = 1)
    
    
  }
  
  
  
  #Get Non-parametric moments
  np_results_nt <- 
    trait_np_bootstrap(imputed_traits = imputed_full,
                       nrep = 200,
                       sample_size = 200)
  np_results_nt <-
    trait_summarise_boot_moments(bootstrap_moments = np_results_nt,
                                 parametric = F)
  
  #Get CWM species
  cwm_species_results_nt <- 
    trait_np_bootstrap(imputed_traits = imputed_species_mean,
                       nrep = 200,
                       sample_size = 200)
  
  cwm_species_results_nt <-
    trait_summarise_boot_moments(bootstrap_moments = cwm_species_results_nt,
                                 parametric = F)
  
  #Get CWM species x site
  cwm_species_x_site_results_nt <- 
    trait_np_bootstrap(imputed_traits = imputed_species_x_site_mean,
                       nrep = 200,
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
                               nrep = 200,
                               sample_size = 200)  
  
  pbs_results_nt <-
    trait_summarise_boot_moments(bootstrap_moments = pbs_results_nt,
                                 parametric = F)
  
  output <- rbind(cbind(method = "nonparametric bs", np_results_nt),
                  cbind(method = "parametric bs", pbs_results_nt),
                  cbind(method = "global cwm", cwm_species_results_nt),
                  cbind(method = "site-specic CWM", cwm_species_x_site_results_nt))
  
  
  return(output)
  
  
  
}#end compare_methods fx















