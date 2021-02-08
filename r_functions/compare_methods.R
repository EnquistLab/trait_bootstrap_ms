n_to_sample <- c(1,2,3,4,5,6,7,8,9,10,25,40,50,100,200,500) #sample sizes (for each species) to draw from the full set of trait data
n_reps_trait<-10 #controls the number of replicated draws for each  sample size
n_reps_boot <- 200 #number of bootstrap replicates to use
sampling_method <- "random"
trait_data <- atraits
community_data <- community
set.seed(1)
###################################
source("r_functions/draw_traits_tidy.R")
source("r_functions/draw_traits.R")
source("r_functions")

#'@param sampling_method Character. How to draw traits from the dataset.  "random" or "biased_large"
#'@param sample_sizes Numeric.  A single sample size or a vector of sample sizes (per species).
#'@param trait_reps Numeric. THe number of replicates for each sample_sizes
#'@param trait_data Data.frame of species trait data.  Columns are site, taxon, ID, trait, value
#'@param community_data Data.frame of community data.
compare_methods <- function(sampling_method = "random", sample_sizes,trait_reps,trait_data,community_data){
  
  
  
    for(i in n_to_sample){
    
      for(t in 1:n_reps_trait){
        
        traits_df <- draw_traits_tidy(tidy_traits =  trait_data, sample_size = i)  
        
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
        
        
  
      }#trait reps loop
    }#sample size loop

  
  
  
  
}#end fx