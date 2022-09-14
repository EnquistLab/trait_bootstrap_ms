library(e1071)
library(tidyverse)
library(FD)
library(traitstrap)
library(funrar)
#library(mFD)

# Note that this applies sample sizes at the species x site level.
# This function assumes that one ID is one sample.
  # So assigning IDs to individuals could return multiple samples per individual.
  # Assigning IDs to traits taken from the same organ returns 
  # multiple traits form the same organ (e.g. leaf)

sim_sample_size_multidimensional <- function(tidy_traits,
                            community,
                            n_to_sample = (1:22)^2, #sample sizes
                            n_reps_trait = 10, #controls the number of replicated draws for each  sample size
                            n_reps_boot = 200, #number of bootstrap replicates to use
                            seed = 2005, #set seed for reproducibility.  2005 = Year Transformers: The Movie (cartoon version) is set.
                            prob = NULL, ##Probability for biased sampling.  If NULL, sampling will be random.
                            min_n_in_sample = 1,
                            focal_trait = "leaf_area_mm2", #trait used for biased sampling
                            large_biased = TRUE, #should sampling be biased towards large individuals?,
                            FRic = FALSE,
                            FDiv = FALSE,
                            tempoutRDS = NULL, #temporary RDS to log output periodically
                            continue=FALSE
){

  #if the code is set to continue, AND there is an output file, load the output and pick up from there.  Else, start fresh

  if(continue & !is.null(tempoutRDS)){

        if(file.exists(tempoutRDS)){
          output <- readRDS(tempoutRDS)
        }else{output<-NULL}

  }else{output <- NULL}
  
  

for( n in n_to_sample){
  for(t in 1:n_reps_trait){

    set.seed(seed = seed*n*t)  
    
    message(paste("sample size",n,", rep",t))
    
    
    #check if replicate t for n traits has already been done
      
      if(any(output$sample_size == n &
              output$rep == t)){next}  
    
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
    
  np_results_nt <- 
        trait_multivariate_bootstrap(imputed_traits = imputed_full,
                                     nrep = n_reps_boot,
                                     sample_size = 200,
                                     fun = function(x){dbFD(x = x,
                                                            calc.FRic = FRic,
                                                            calc.FDiv = FDiv,
                                                            calc.CWM = FALSE,
                                                            stand.x = FALSE,
                                                            scale.RaoQ = FALSE)},
                                     id = "ID")
      
      
      # reformat
      
        np_results_nt %>%
          rowwise()%>%
          mutate(FEve = .data$result$FEve,
                 FDis = .data$result$FDis,
                 RaoQ = .data$result$RaoQ,
                 FRic = .data$result$FRic,
                 FDiv = .data$result$FDiv
        ) %>%
          dplyr::select(-result) -> np_results_nt

      # summarize non-parametric moments
      
      np_results_nt %>%
        group_by(site) %>%
        summarise(ci_low_FEve = traitstrap:::get_ci(data = FEve, which = "low", parametric = FALSE),
                  ci_high_FEve = traitstrap:::get_ci(data = FEve, which = "high", parametric = FALSE),
                  FEve = mean(FEve),
                  ci_low_FDis = traitstrap:::get_ci(data = FDis, which = "low", parametric = FALSE),
                  ci_high_FDis = traitstrap:::get_ci(data = FDis, which = "high", parametric = FALSE),
                  FDis = mean(FDis),
                  ci_low_RaoQ = traitstrap:::get_ci(data = RaoQ, which = "low", parametric = FALSE),
                  ci_high_RaoQ = traitstrap:::get_ci(data = RaoQ, which = "high", parametric = FALSE),
                  RaoQ = mean(RaoQ),
                  ci_low_FRic = traitstrap:::get_ci(data = FRic, which = "low", parametric = FALSE),
                  ci_high_FRic = traitstrap:::get_ci(data = FRic, which = "high", parametric = FALSE),
                  FRic = mean(FRic),
                  ci_low_FDiv = traitstrap:::get_ci(data = FDiv, which = "low", parametric = FALSE),
                  ci_high_FDiv = traitstrap:::get_ci(data = FDiv, which = "high", parametric = FALSE),
                  FDiv = mean(FDiv)) -> np_results_nt

            
  
    # Get CWM species
    cwm_species_results_nt <- 
      trait_np_bootstrap(imputed_traits = imputed_species_mean,
                         nrep = n_reps_boot,
                         sample_size = 200)
    
    cwm_species_results_nt <- 
    trait_multivariate_bootstrap(imputed_traits = imputed_species_mean,
                                 nrep = n_reps_boot,
                                 sample_size = 200,
                                 fun = function(x){dbFD(x = x,
                                                        calc.FRic = FRic,
                                                        calc.FDiv = FDiv,
                                                        calc.CWM = FALSE,
                                                        stand.x = FALSE,
                                                        scale.RaoQ = FALSE)},
                                 id = "ID")
    
    
      # reformat
      
      cwm_species_results_nt %>%
        rowwise()%>%
        mutate(FEve = .data$result$FEve,
               FDis = .data$result$FDis,
               RaoQ = .data$result$RaoQ,
               FRic = .data$result$FRic,
               FDiv = .data$result$FDiv
        ) %>%
        dplyr::select(-result) -> cwm_species_results_nt
      
      # summarize cwm moments
      
      cwm_species_results_nt %>%
        group_by(site) %>%
        summarise(ci_low_FEve = traitstrap:::get_ci(data = FEve, which = "low", parametric = FALSE),
                  ci_high_FEve = traitstrap:::get_ci(data = FEve, which = "high", parametric = FALSE),
                  FEve = mean(FEve),
                  ci_low_FDis = traitstrap:::get_ci(data = FDis, which = "low", parametric = FALSE),
                  ci_high_FDis = traitstrap:::get_ci(data = FDis, which = "high", parametric = FALSE),
                  FDis = mean(FDis),
                  ci_low_RaoQ = traitstrap:::get_ci(data = RaoQ, which = "low", parametric = FALSE),
                  ci_high_RaoQ = traitstrap:::get_ci(data = RaoQ, which = "high", parametric = FALSE),
                  RaoQ = mean(RaoQ),
                  ci_low_FRic = traitstrap:::get_ci(data = FRic, which = "low", parametric = FALSE),
                  ci_high_FRic = traitstrap:::get_ci(data = FRic, which = "high", parametric = FALSE),
                  FRic = mean(FRic),
                  ci_low_FDiv = traitstrap:::get_ci(data = FDiv, which = "low", parametric = FALSE),
                  ci_high_FDiv = traitstrap:::get_ci(data = FDiv, which = "high", parametric = FALSE),
                  FDiv = mean(FDiv)) -> cwm_species_results_nt
      
    
      # Get CWM species x site
      
      cwm_species_x_site_results_nt <- 
        trait_np_bootstrap(imputed_traits = imputed_species_x_site_mean,
                           nrep = n_reps_boot,
                           sample_size = 200)
      
      cwm_species_x_site_results_nt <- 
        trait_multivariate_bootstrap(imputed_traits = imputed_species_mean,
                                     nrep = n_reps_boot,
                                     sample_size = 200,
                                     fun = function(x){dbFD(x = x,
                                                            calc.FRic = FRic,
                                                            calc.FDiv = FDiv,
                                                            calc.CWM = FALSE,
                                                            stand.x = FALSE,
                                                            scale.RaoQ = FALSE)},
                                     id = "ID")
      
      
      # reformat
        
        cwm_species_x_site_results_nt %>%
          rowwise() %>%
          mutate(FEve = .data$result$FEve,
                 FDis = .data$result$FDis,
                 RaoQ = .data$result$RaoQ,
                 FRic = .data$result$FRic,
                 FDiv = .data$result$FDiv
          ) %>%
          dplyr::select(-result) -> cwm_species_x_site_results_nt
      
      # summarize cwm species x site moments
      
      cwm_species_x_site_results_nt %>%
        group_by(site) %>%
        summarise(ci_low_FEve = traitstrap:::get_ci(data = FEve, which = "low", parametric = FALSE),
                  ci_high_FEve = traitstrap:::get_ci(data = FEve, which = "high", parametric = FALSE),
                  FEve = mean(FEve),
                  ci_low_FDis = traitstrap:::get_ci(data = FDis, which = "low", parametric = FALSE),
                  ci_high_FDis = traitstrap:::get_ci(data = FDis, which = "high", parametric = FALSE),
                  FDis = mean(FDis),
                  ci_low_RaoQ = traitstrap:::get_ci(data = RaoQ, which = "low", parametric = FALSE),
                  ci_high_RaoQ = traitstrap:::get_ci(data = RaoQ, which = "high", parametric = FALSE),
                  RaoQ = mean(RaoQ),
                  ci_low_FRic = traitstrap:::get_ci(data = FRic, which = "low", parametric = FALSE),
                  ci_high_FRic = traitstrap:::get_ci(data = FRic, which = "high", parametric = FALSE),
                  FRic = mean(FRic),
                  ci_low_FDiv = traitstrap:::get_ci(data = FDiv, which = "low", parametric = FALSE),
                  ci_high_FDiv = traitstrap:::get_ci(data = FDiv, which = "high", parametric = FALSE),
                  FDiv = mean(FDiv)) -> cwm_species_x_site_results_nt
      
    # calculate metrics the old school way (no intraspecific)
        
        imputed_species_mean %>%
          ungroup%>%
          select(taxon,trait,value)%>%
          unique()%>%
          pivot_wider(names_from = trait,
                      values_from = value) %>%
          column_to_rownames("taxon") -> mean_wide
        
          community %>%
            pivot_wider(names_from = site,
                        values_from = abundance,
                        values_fill = 0)%>%
          column_to_rownames("taxon")%>%t() -> community_wide
          
          if(!all(colnames(community_wide)==rownames(mean_wide))){
            stop("error in species order")
            }
          
          
          cwm_species_results_og_weighting <- 
              dbFD(x = mean_wide,
                   a = community_wide,
                   calc.FRic = FRic,
                   calc.FDiv = FDiv,
                   calc.CWM = FALSE,
                   stand.x = FALSE,
                   scale.RaoQ = FALSE)
        
          # reformat
          
          bind_cols(method = "original",
                    sample_size = n,
                    rep = t,
                    site = names(cwm_species_results_og_weighting$FEve),
                    FEve = cwm_species_results_og_weighting$FEve,
                    FDis = cwm_species_results_og_weighting$FDis,
                    RaoQ = cwm_species_results_og_weighting$RaoQ,
                    FRic = cwm_species_results_og_weighting$FRic,
                    FDiv = cwm_species_results_og_weighting$FDiv) ->
            cwm_species_results_og_weighting

  
          
            
    #can't think of a good parametric distribution for this, so skipping that one

          output_nt <- rbind(cbind(method = "nonparametric bs", sample_size = n, rep = t, np_results_nt),
                             cbind(method = "global cwm", sample_size = n, rep = t, cwm_species_results_nt),
                             cbind(method = "site-specic CWM", sample_size = n, rep = t, cwm_species_x_site_results_nt))
                
          output_nt <- bind_rows(output_nt,cwm_species_results_og_weighting)
              
          
    output <- rbind(output, output_nt)
    
    
  
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
  
  true_values <- 
  wide_traits %>%
    ungroup %>%
    group_by(site) %>%
    summarise(fds = dbFD(x = data.frame(leaf_area_mm2,
                                        dry_mass_mg,
                                        height,
                                        biomass_per_ind,
                                        LMA_mg_mm2),
                         calc.FRic = FRic,
                         calc.FDiv = FDiv,
                         calc.CWM = FALSE,
                         stand.x = FALSE,
                         scale.RaoQ = FALSE)
              ) %>%
      mutate(metric = names(fds),
             values = unlist(fds))%>%
      dplyr::select(-fds)%>%
      filter(metric %in% c("FEve","FDis","RaoQ","FRic","FDiv"))

  #Next, append true moments to output data for convenience
  
  true_values %>%
    pivot_wider(names_from = metric,
                names_glue = "true_{metric}",
                values_from = values) %>%
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



