

###############################################################
#'@param nreps Number of replicated draws of traits.
#'@param nsamples Number of trait samples per species per plot.
#'@param n_reps_boot is the number of bootstrap replicates to use
#'@note This simple little function isn't very general and currently assumes normal distributions.
sim_percent_sampling <- function(traits, community, nreps=10, nsamples = 10, n_reps_boot=200){
  
  output <- NULL
  
  for(i in 1:nreps){
    
  # Get initial sample of all species
    traits_nt <- draw_traits_tidy(tidy_traits = traits, sample_size = nsamples)
    community_nt <- community
    
  while(nrow(community_nt) > 0){    
  #iterate through and sequentially remove the least abundant species in each site 
    
    
  if(nrow(community_nt) < 1){next}
    
  # calculate values and record
  #Get species mean traits
  species_means_nt <- samples_to_means(tidy_traits = traits_nt,level = "taxon")
  species_means_nt$site <- "ALL" #Note: adding a "dummy" site here so traitstrap will use global data but still return site-level moments
  
  #Get species x site mean traits
  species_site_means_nt <- samples_to_means(tidy_traits = traits_nt,level = "taxon_by_site")
  
  #Trait imputation for distributions
  imputed_full <- 
    trait_impute(comm = community_nt,
                 traits = traits_nt,
                 scale_hierarchy = "site",
                 global = T,
                 taxon_col = "taxon",
                 trait_col = "trait",
                 value_col = "value",
                 abundance_col = "abundance")
  
  #Trait imputation for species mean
  imputed_species_mean <- 
    trait_impute(comm = community_nt,
                 traits = species_means_nt,
                 scale_hierarchy = "site",
                 global = T,
                 taxon_col = "taxon",
                 trait_col = "trait",
                 value_col = "value",
                 abundance_col = "abundance")
  
  #Trait imputation for species x site mean
  imputed_species_x_site_mean <- 
    trait_impute(comm = community_nt,
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
    trait_summarise_boot_moments(bootstrap_moments =  np_results_nt)
  
  #Get CWM species
  cwm_species_results_nt <-
    trait_np_bootstrap(imputed_traits = imputed_species_mean,
                       nrep = n_reps_boot,
                       sample_size = 200)
  
  cwm_species_results_nt <-
    trait_summarise_boot_moments(bootstrap_moments =  cwm_species_results_nt)
  
  #Get CWM species x site
  cwm_species_x_site_results_nt <-
    trait_np_bootstrap(imputed_traits = imputed_species_x_site_mean,
                       nrep = n_reps_boot,
                       sample_size = 200)
  
  cwm_species_x_site_results_nt <-
    trait_summarise_boot_moments(bootstrap_moments = cwm_species_x_site_results_nt)
  
  #Get parametric moments
  fitted_nt <- trait_fit_distributions(imputed_traits = imputed_full,
                          distribution_type = "normal")
  
  pbs_results_nt <-
    trait_parametric_bootstrap(fitted_distributions = fitted_nt,
                               nrep = n_reps_boot, sample_size = 200)


  pbs_results_nt <-
    trait_summarise_boot_moments(bootstrap_moments = pbs_results_nt)


  sample_sizes_t <- community_nt %>% group_by(site) %>% summarise(sum_abd = sum(abundance), ntaxa= length(abundance))


  output_t <- rbind(cbind(method = "nonparametric bs", sample_size = nsamples, np_results_nt),
                    cbind(method = "parametric bs", sample_size = nsamples, pbs_results_nt),
                    cbind(method = "global cwm", sample_size = nsamples, cwm_species_results_nt),
                    cbind(method = "site-specic CWM", sample_size = nsamples, cwm_species_x_site_results_nt))
  output_t <- merge(output_t, sample_sizes_t)
  output_t$replicate <- i
  
  
  #########################
  
  #NA checks
    if(any(is.na(output_t$mean) | is.na(output_t$var) ) ){stop("NAs found in mean or variance")}
    
    #should only see NAs for skew and kurt where nspecies = 1
    if(any(output_t$ntaxa[which(is.na(output_t$skew))]!=1)){stop("NAs in skewness with multiple taxa")}
    if(any(output_t$ntaxa[which(is.na(output_t$kurt))]!=1)){stop("NAs in kurtosis with multiple taxa")}
    
  
  ###############################  
  
  output <- rbind(output,output_t)
  
  
  #print(nrow(community_nt))
  
  
  #Now update the community_nt and traits objects by removing the least common species
  community_nt  %>% group_by(site) %>%summarise(taxon=taxon[rand.min(abundance)]) -> to_toss
  
  #remove the "to toss" species from the community_nt and trait data
  
  community_nt %>% anti_join(to_toss) -> community_nt
  
  traits_nt %>% anti_join(to_toss) -> traits_nt
  
  
  
  
  }#while loop
    
    
    
    
    
    
    
    
    
    
    
    
      
    
  }# i loop  

  
  
  
  #Append percent community sampled to output
  output %>% group_by(site) %>% mutate(pct_abd_sampled = sum_abd/max(sum_abd)*100)-> output 

  
  
  #Calculate true moment
  traits %>% group_by(site,trait) %>% summarise(true_mean=mean(value),
                                                 true_variance=var(value),
                                                 true_skewness = skewness(value),
                                                 true_kurtosis = kurtosis(value)) -> true_moments
  
  #Next, append true moments to output data for convenience
  output <- merge(x = output,y = true_moments,by = c("site","trait"))
  
  
  #tidy up results
  #could probably do this with pipes, but I'll go oldschool and cut down the googling
  
  #colnames(output)
  
  sim_mean <- output[c("site","trait","method","sample_size","n",
                       "sum_abd","ntaxa","replicate","pct_abd_sampled",
                        "mean","ci_low_mean","ci_high_mean","true_mean")]
  sim_mean$moment <- "mean"
  colnames(sim_mean)[grep(pattern = "ci_low",x = colnames(sim_mean))]<-"ci_low"
  colnames(sim_mean)[grep(pattern = "ci_high",x = colnames(sim_mean))]<-"ci_high"
  colnames(sim_mean)[grep(pattern = "true",x = colnames(sim_mean))]<-"true_value"
  colnames(sim_mean)[grep(pattern = "mean",x = colnames(sim_mean))]<-"estimate"
  
  sim_var <- output[c("site","trait","method","sample_size","n",
                       "sum_abd","ntaxa","replicate","pct_abd_sampled",
                       "var","ci_low_var","ci_high_var","true_variance")]
  sim_var$moment <- "variance"
  colnames(sim_var)[grep(pattern = "ci_low",x = colnames(sim_var))]<-"ci_low"
  colnames(sim_var)[grep(pattern = "ci_high",x = colnames(sim_var))]<-"ci_high"
  colnames(sim_var)[grep(pattern = "true",x = colnames(sim_var))]<-"true_value"
  colnames(sim_var)[grep(pattern = "var",x = colnames(sim_var))]<-"estimate"
  
  
  sim_skew <- output[c("site","trait","method","sample_size","n",
                        "sum_abd","ntaxa","replicate","pct_abd_sampled",
                        "skew","ci_low_skew","ci_high_skew","true_skewness")]
  sim_skew$moment <- "skewness"
  colnames(sim_skew)[grep(pattern = "ci_low",x = colnames(sim_skew))]<-"ci_low"
  colnames(sim_skew)[grep(pattern = "ci_high",x = colnames(sim_skew))]<-"ci_high"
  colnames(sim_skew)[grep(pattern = "true",x = colnames(sim_skew))]<-"true_value"
  colnames(sim_skew)[grep(pattern = "skew",x = colnames(sim_skew))]<-"estimate"
  
  sim_kurt <- output[c("site","trait","method","sample_size","n",
                        "sum_abd","ntaxa","replicate","pct_abd_sampled",
                        "kurt","ci_low_kurt","ci_high_kurt","true_kurtosis")]
  sim_kurt$moment <- "kurtosis"
  colnames(sim_kurt)[grep(pattern = "ci_low",x = colnames(sim_kurt))]<-"ci_low"
  colnames(sim_kurt)[grep(pattern = "ci_high",x = colnames(sim_kurt))]<-"ci_high"
  colnames(sim_kurt)[grep(pattern = "true",x = colnames(sim_kurt))]<-"true_value"
  colnames(sim_kurt)[grep(pattern = "kurt",x = colnames(sim_kurt))]<-"estimate"
  
  output <- rbind(sim_mean,sim_var,sim_skew,sim_kurt)
  rm(sim_mean,sim_var,sim_skew,sim_kurt)
  
  #Rename methods for plotting
  #unique(output$method)
  output$method[which(output$method=="global cwm")] <- "Cross-Site CWM"
  output$method[which(output$method=="site-specic CWM")] <- "Site-Specific CWM"
  output$method[which(output$method=="nonparametric bs")] <- "Non-Parametric BS"
  output$method[which(output$method=="parametric bs")] <- "Parametric BS"
  
  output$method <- ordered(output$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametric BS","Non-Parametric BS"))
  
  
  return(output)
  
  
  
} #fx



##################################

#resample works better than sample
resample <- function(x, ...) x[sample.int(length(x), ...)]


#accessory function to handle random assignment of mins
rand.min<-function(x){resample(which(x==min(x)),1)}


