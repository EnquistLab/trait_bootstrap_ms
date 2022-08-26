library(e1071)
library(tidyverse)
library(FD)
library(funrar)
#library(mFD)

# Note that this applies sample sizes at the species x site level.
# This function assumes that one ID is one sample.
# So assigning IDs to individuals could return multiple samples per individual.
# Assigning IDs to traits taken from the same organ returns 
# multiple traits form the same organ (e.g. leaf)

sim_sample_size_multidimensional_rarity <- function(tidy_traits,
                                             community,
                                             n_to_sample = (1:22)^2, #sample sizes
                                             n_reps_trait = 10, #controls the number of replicated draws for each  sample size
                                             n_reps_boot = 200, #number of bootstrap replicates to use
                                             seed = 2005, #set seed for reproducibility.  2005 = Year Transformers: The Movie (cartoon version) is set.
                                             prob = NULL, ##Probability for biased sampling.  If NULL, sampling will be random.
                                             min_n_in_sample = 1,
                                             focal_trait = "leaf_area_mm2", #trait used for biased sampling
                                             large_biased = TRUE, #should sampling be biased towards large individuals?,
                                             tempoutRDS = NULL #temporary RDS to log output periodically
){
  
  
  set.seed(seed = seed)  
  output<-NULL
  
  for( n in n_to_sample){
    for(t in 1:n_reps_trait){  
      
      message(paste("sample size",n,", rep",t))
      
      #First simulate a draw of the relevant sample size
      if (is.null(prob)){
        
        #note that this draws traits for species and sites combos by their ID.
        traits_nt <- draw_traits_tidy(tidy_traits = tidy_traits,
                                      sample_size =  n)  
        
      }else{
        
        traits_nt <- draw_traits_tidy_large(tidy_traits = tidy_traits,
                                            sample_size =  n,
                                            focal_trait = focal_trait,
                                            prob = prob,
                                            large_biased = large_biased)
        
      }
      
      
      #Get species mean traits
      species_means_nt <- samples_to_means(tidy_traits = traits_nt,
                                           level = "taxon")
      
      #Note: adding a "dummy" site here so traitstrap will use global data but still return site-level moments
      #Note :adding ID for multivariate code
      
      species_means_nt %>%
        mutate(site="ALL") %>%
        group_by(taxon, site)%>%
        mutate(ID=cur_group_id())%>%
        ungroup()-> species_means_nt
      
      #Get species x site mean traits
      
      species_site_means_nt <- samples_to_means(tidy_traits = traits_nt,
                                                level = "taxon_by_site")
      
      species_site_means_nt %>%
        group_by(taxon, site) %>%
        mutate(ID=cur_group_id()) %>%
        ungroup()-> species_site_means_nt
      
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
      
      #dbfd is rather slow, can I do something faster?
      #feve
      #fdis
      #Frid
      #fdiv
      
      
      
      
      np_results_nt <- 
        trait_multivariate_bootstrap(imputed_traits = imputed_full,
                                     nrep = n_reps_boot,
                                     sample_size = 200,
                                     fun = function(x){
                                       temp <-
                                         funrar(pres_matrix = matrix(data = 1,ncol = nrow(x),nrow = 1,
                                                                     dimnames = list(1,1:nrow(x))),
                                                dist_matrix = as.matrix(dist(x = x,method = "euclidean",upper = TRUE,diag = TRUE)),
                                                rel_abund = TRUE)
                                       return(data.frame(mean_uniqueness = mean(temp$Ui$Ui),
                                                         mean_distinctiveness = mean(temp$Di)))
                                       
                                       
                                     },
                                     id = "ID")
      
      np_results_nt$mean_uniqueness <-
        lapply(np_results_nt$result,FUN = function(x){x$mean_uniqueness}) %>% unlist()
      
      np_results_nt$mean_distinctiveness <-
        lapply(np_results_nt$result,FUN = function(x){x$mean_distinctiveness}) %>% unlist()
      
      
      # reformat
      
      np_results_nt %>%
        dplyr::select(-result) -> np_results_nt
      
      # summarize non-parametric moments
      
      np_results_nt %>%
        group_by(site) %>%
        summarise(ci_low_avg_uniqueness = traitstrap:::get_ci(data = mean_uniqueness, which = "low", parametric = FALSE),
                  ci_high_avg_uniqueness = traitstrap:::get_ci(data = mean_uniqueness, which = "high", parametric = FALSE),
                  avg_uniqueness = mean(mean_uniqueness),
                  ci_low_avg_distinctiveness = traitstrap:::get_ci(data = mean_distinctiveness, which = "low", parametric = FALSE),
                  ci_high_avg_distinctiveness = traitstrap:::get_ci(data = mean_distinctiveness, which = "high", parametric = FALSE),
                  avg_distinctiveness = mean(mean_distinctiveness)) -> np_results_nt
      
      
      
      # Get CWM species
      cwm_species_results_nt <- 
        trait_multivariate_bootstrap(imputed_traits = imputed_species_mean,
                                     nrep = n_reps_boot,
                                     sample_size = 200,
                                     fun = function(x){
                                       temp <-
                                         funrar(pres_matrix = matrix(data = 1,ncol = nrow(x),nrow = 1,
                                                                     dimnames = list(1,1:nrow(x))),
                                                dist_matrix = as.matrix(dist(x = x,method = "euclidean",upper = TRUE,diag = TRUE)),
                                                rel_abund = TRUE)
                                       return(data.frame(mean_uniqueness = mean(temp$Ui$Ui),
                                                         mean_distinctiveness = mean(temp$Di)))
                                       
                                       
                                     },
                                     id = "ID")
      
      cwm_species_results_nt$mean_uniqueness <-
        lapply(cwm_species_results_nt$result,FUN = function(x){x$mean_uniqueness}) %>% unlist()
      
      cwm_species_results_nt$mean_distinctiveness <-
        lapply(cwm_species_results_nt$result,FUN = function(x){x$mean_distinctiveness}) %>% unlist()
      
      
      # reformat
      
      cwm_species_results_nt %>%
        dplyr::select(-result) -> cwm_species_results_nt
      
      # summarize cwm moments
      
      cwm_species_results_nt %>%
        group_by(site) %>%
        summarise(ci_low_avg_uniqueness = traitstrap:::get_ci(data = mean_uniqueness, which = "low", parametric = FALSE),
                  ci_high_avg_uniqueness = traitstrap:::get_ci(data = mean_uniqueness, which = "high", parametric = FALSE),
                  avg_uniqueness = mean(mean_uniqueness),
                  ci_low_avg_distinctiveness = traitstrap:::get_ci(data = mean_distinctiveness, which = "low", parametric = FALSE),
                  ci_high_avg_distinctiveness = traitstrap:::get_ci(data = mean_distinctiveness, which = "high", parametric = FALSE),
                  avg_distinctiveness = mean(mean_distinctiveness)) -> cwm_species_results_nt
      
      
      # Get CWM species x site cwm_species_x_site
      
      cwm_species_x_site_results_nt <- 
        trait_multivariate_bootstrap(imputed_traits = imputed_species_x_site_mean,
                                     nrep = n_reps_boot,
                                     sample_size = 200,
                                     fun = function(x){
                                       temp <-
                                         funrar(pres_matrix = matrix(data = 1,ncol = nrow(x),nrow = 1,
                                                                     dimnames = list(1,1:nrow(x))),
                                                dist_matrix = as.matrix(dist(x = x,method = "euclidean",upper = TRUE,diag = TRUE)),
                                                rel_abund = TRUE)
                                       return(data.frame(mean_uniqueness = mean(temp$Ui$Ui),
                                                         mean_distinctiveness = mean(temp$Di)))
                                       
                                       
                                     },
                                     id = "ID")
      
      cwm_species_x_site_results_nt$mean_uniqueness <-
        lapply(cwm_species_x_site_results_nt$result,FUN = function(x){x$mean_uniqueness}) %>% unlist()
      
      cwm_species_x_site_results_nt$mean_distinctiveness <-
        lapply(cwm_species_x_site_results_nt$result,FUN = function(x){x$mean_distinctiveness}) %>% unlist()
      
      
      # reformat
      
      cwm_species_x_site_results_nt %>%
        dplyr::select(-result) -> cwm_species_x_site_results_nt
      
      # summarize cwm moments
      
      cwm_species_x_site_results_nt %>%
        group_by(site) %>%
        summarise(ci_low_avg_uniqueness = traitstrap:::get_ci(data = mean_uniqueness, which = "low", parametric = FALSE),
                  ci_high_avg_uniqueness = traitstrap:::get_ci(data = mean_uniqueness, which = "high", parametric = FALSE),
                  avg_uniqueness = mean(mean_uniqueness),
                  ci_low_avg_distinctiveness = traitstrap:::get_ci(data = mean_distinctiveness, which = "low", parametric = FALSE),
                  ci_high_avg_distinctiveness = traitstrap:::get_ci(data = mean_distinctiveness, which = "high", parametric = FALSE),
                  avg_distinctiveness = mean(mean_distinctiveness)) -> cwm_species_x_site_results_nt
      
      
      
      #can't think of a good parametric distribution for this, so skipping that one
      
      output <- rbind(output, rbind(cbind(method = "nonparametric bs", sample_size = n, np_results_nt),
                                    cbind(method = "global cwm", sample_size = n, cwm_species_results_nt),
                                    cbind(method = "site-specic CWM", sample_size = n, cwm_species_x_site_results_nt))
      )
      
      
      #log output if requested
      
      if(!is.null(tempoutRDS)){
        
        saveRDS(object = output,
                file = tempoutRDS)
        
      }
      
      
    } #t trait rep  
  }# n sample size loop
  
  
  #cleanup
  rm(cwm_species_results_nt,
     cwm_species_x_site_results_nt,
     imputed_full,
     imputed_species_mean,
     imputed_species_x_site_mean,
     np_results_nt,
     species_means_nt,
     species_site_means_nt,
     traits_nt,
     n,
     t)
  
  #Append true moments to data
  
  #First, calculate true values for each site x trait
  
  tidy_traits %>%
    pivot_wider(id_cols = c(ID,site,taxon),
                names_from = trait,
                values_from = value) -> wide_traits
  
  
  temp_fx <- function(x){
      temp <-
        funrar(pres_matrix = matrix(data = 1,ncol = nrow(x),nrow = 1,
                                    dimnames = list(1,1:nrow(x))),
               dist_matrix = as.matrix(dist(x = data.frame(x$leaf_area_mm2,
                                                           x$dry_mass_mg,
                                                           x$height,
                                                           x$biomass_per_ind,
                                                           x$LMA_mg_mm2),method = "euclidean",upper = TRUE,diag = TRUE)),
               rel_abund = TRUE)
      
      out <- data.frame(mean_uniqueness = mean(temp$Ui$Ui),
                        mean_distinctiveness = mean(temp$Di))
      
      return(as_tibble(out))
      
      
    }    

  
  true_values <-
    wide_traits %>%
    ungroup %>%
    group_by(site) %>%
      group_modify(~temp_fx(.))
    
  
  #Next, append true moments to output data for convenience
  
  true_values <-
  true_values %>%
    rename(true_mean_uniqueness = mean_uniqueness,
           true_mean_distinctiveness =mean_distinctiveness)
  
  true_values %>%
    inner_join(y = output,
               by = c("site" = "site")) -> output
  
  #log output if requested
  
  if(!is.null(tempoutRDS)){
    
    saveRDS(object = output,
            file = tempoutRDS)
    
  }
  
  
  #cleanup
  rm(true_values)
  
  return(output)
} #end fx

#############################################


# 
# output%>%
#   mutate(feve_diff = FEve - true_FEve)%>%
#   ggplot(mapping = aes(x = sample_size,
#                        y = feve_diff))+
#   geom_point()
# 
# 
# output%>%
#   mutate(fdis_diff = FDis - true_FDis)%>%
#   ggplot(mapping = aes(x = sample_size,
#                        y = fdis_diff))+
#   geom_point()
# 
# 
# 
