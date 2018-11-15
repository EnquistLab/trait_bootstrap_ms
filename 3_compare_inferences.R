#Comparing trait inferences on the same simulated data
#Code not complete...or even close

source("r_functions/trait_distribution_fx.R")
source("r_functions/draw_traits.R")
library(moments)

atraits<-readRDS("data/all_traits_RMBL.rds")
mean_traits<-readRDS("data/species_mean_traits_RMBL.rds")
community<-readRDS("data/species_presence_RMBL.rds")

simulation_output<-NULL

n_to_sample<-c(1,2,3,4,5,6,7,8,9,10,50,100,200,500)
n_reps_trait<-10
n_reps_boot <- 200

#assuming random sampling
#draw=draw of traits
#boot=boostrap within that draw of traits

set.seed(1)
for(s in 1:length(unique(community$site))){
  site<-unique(community$site)[s]
  
  
  for(i in n_to_sample){
    
    for(t in 1:n_reps_trait){
      
      traits_df<-draw_traits_random(traits_df = atraits[which(atraits$site==site),which(colnames(atraits)!="site")],sample_size = i)  
      trait_means<-distribution_to_means(traits_df)
      
      trait_means$leaf_area_mm2
      
      
      out_dist_t_boot<-trait_distributions(number_replicates = n_reps_boot, abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,trait_data = traits_df)
      out_dist_t_mean<-trait_distributions(number_replicates = n_reps_boot, abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,trait_data = trait_means)
      

      for( l in 1: length(out_dist_t_boot)){
        
        #write output  
        write.csv(x = out_dist_t_boot[[l]],
                  file = paste("output_distributions/RMBL_random_sampling/RMBL",site,names(out_dist_t_boot)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
                  row.names = F  )
        
        
        write.csv(x = out_dist_t_mean[[l]],
                  file = paste("output_distributions/RMBL_random_sampling_mean_values/RMBL",site,names(out_dist_t_mean)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
                  row.names = F  )
        
        
        output_l_mean<-cbind(i,#sample size
                t,#rep number 
                names(out_dist_t_mean)[l], #trait
                as.character(site), #site
                "mean", #moment
                mean(rowMeans(out_dist_t_boot[[l]])),#boot mean
                mean(rowMeans(out_dist_t_mean[[l]]) ), #cwm mean
                mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T), #actual mean
                (mean(rowMeans(out_dist_t_boot[[l]]))-mean(rowMeans(out_dist_t_mean[[l]]) ))/sd(rowMeans(out_dist_t_boot[[l]])),#SESmean
                (mean(rowMeans(out_dist_t_boot[[l]]))-mean(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T))/sd(rowMeans(out_dist_t_boot[[l]]))#SESactual
                
                )#mean output
        
        output_l_var<-cbind(i,#sample size
                             t,#rep number 
                             names(out_dist_t_mean)[l], #trait
                             as.character(site), #site
                             "variance", #moment
                             mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var)),#boot mean
                             mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = var)), #cwm mean
                             var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T), #actual mean
                             (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var))-mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = var)))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var)),#SESmean
                             (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var))-var(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = var))#SESactual
                             
        )#var output
        
        
        output_l_skew<-cbind(i,#sample size
                            t,#rep number 
                            names(out_dist_t_mean)[l], #trait
                            as.character(site), #site
                            "skewness", #moment
                            mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness)),#boot mean
                            mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = skewness)), #cwm mean
                            skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T), #actual mean
                            (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness))-mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = skewness)))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness)),#SESmean
                            (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness))-skewness(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = skewness))#SESactual
                            
        )#var output
        
        output_l_kurt<-cbind(i,#sample size
                             t,#rep number 
                             names(out_dist_t_mean)[l], #trait
                             as.character(site), #site
                             "kurtosis", #moment
                             mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis)),#boot mean
                             mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = kurtosis)), #cwm mean
                             kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T), #actual mean
                             (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis))-mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = kurtosis)))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis)),#SESmean
                             (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis))-kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis))#SESactual
                             
        )#var output
          
        simulation_output<-rbind(simulation_output,output_l_mean,output_l_var,output_l_skew,output_l_kurt)
      
            
        
      }#l loop  
    }#t n reps trait
  }#i n to sample
}#s site


#################################
