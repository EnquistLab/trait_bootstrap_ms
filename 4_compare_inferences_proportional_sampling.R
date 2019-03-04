#compare inferences sampling proportional to abundance


source("r_functions/trait_distribution_fx.R")
source("r_functions/draw_traits.R")
source("r_functions/calc_ci.R")
library(moments)

atraits<-readRDS("data/all_traits_unscaled_RMBL.rds")
atraits$site<-as.character(atraits$site)
#mean_traits<-readRDS("data/species_mean_traits_RMBL.rds")
community<-readRDS("data/species_presence_RMBL.rds")



simulation_output<-NULL

prop_to_sample<-c(.01,.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)
n_reps_trait<-10
n_reps_boot <- 200

#assuming random sampling
#draw=draw of traits
#boot=boostrap within that draw of traits

set.seed(1)
for(s in 1:length(unique(community$site))){
  site<-unique(community$site)[s]
  community_s<-community[which(community$site==site),]
  
  for(i in prop_to_sample){
    
    for(t in 1:n_reps_trait){
      
      traits_df<-draw_traits_random_proportion(traits_df = atraits[which(atraits$site==site),which(colnames(atraits)!="site")],
                                               sample_proportion = i,
                                               community_df = community_s)  
      
      trait_means<-distribution_to_means(traits_df)
      
      out_dist_t_boot<-trait_distributions(number_replicates = n_reps_boot, abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,trait_data = traits_df)
      out_dist_t_param<-trait_distributions_parametric(number_replicates = n_reps_boot, abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,trait_data = traits_df)
      out_dist_t_mean<-trait_distributions(number_replicates = n_reps_boot, abundance_data = community[c("taxon","abundance")][which(community$site==site),]  ,trait_data = trait_means)
      
      
      for( l in 1: length(out_dist_t_boot)){
        
        #write output  
        write.csv(x = out_dist_t_boot[[l]],
                  file = paste("output_distributions/RMBL_proportional_sampling/RMBL",site,names(out_dist_t_boot)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
                  row.names = F  )
        
        
        write.csv(x = out_dist_t_mean[[l]],
                  file = paste("output_distributions/RMBL_proportional_sampling_mean_values/RMBL",site,names(out_dist_t_mean)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
                  row.names = F  )
      
        
        write.csv(x = out_dist_t_param[[l]],
                  file = paste("output_distributions/RMBL_proportional_sampling_parametric_bs/RMBL",site,names(out_dist_t_mean)[l],paste("n_",i,sep = ""),"draw",t,sep = "."),
                  row.names = F  )
        
        
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
                             mean_in_100ci_param
                             
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
                            var_in_100ci_param
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
                             skew_in_100ci_param
        )#var output
        
        output_l_kurt<-cbind(i,#sample size
                             t,#rep number
                             nrow(traits_df),#number sampled
                             names(out_dist_t_mean)[l], #trait
                             as.character(site), #site
                             "kurtosis", #moment
                             mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis)),#boot mean
                             mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = kurtosis)), #cwm mean
                             mean(apply(X = out_dist_t_param[[l]], MARGIN = 1,FUN = kurtosis)), #cwm mean
                             kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T), #actual mean
                             (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis))-mean(apply(X = out_dist_t_mean[[l]], MARGIN = 1,FUN = kurtosis)))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis)),#SESmean
                             (mean(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis))-kurtosis(atraits[which(atraits$site==site),which(colnames(atraits)==names(out_dist_t_mean)[l])],na.rm = T))/sd(apply(X = out_dist_t_boot[[l]], MARGIN = 1,FUN = kurtosis)),#SESactual
                             kurt_in_95ci,
                             kurt_in_100ci,
                             kurt_in_95ci_param,
                             kurt_in_100ci_param
        )#var output
        
        simulation_output<-rbind(simulation_output,output_l_mean,output_l_var,output_l_skew,output_l_kurt)
        rm(output_l_kurt,output_l_mean,output_l_skew,output_l_var)
        
        
      }#l loop  
    }#t n reps trait
  }#i n to sample
}#s site
rm(i,l,s,t,site,out_dist_t_boot,out_dist_t_mean,trait_means,traits_df)
simulation_output<-as.data.frame(simulation_output)
colnames(simulation_output)<-c("sample_proportion","replicate","total_samples","trait","site","moment","boot_mean","cwm_mean","parametric_mean","actual_mean","SES_boot_v_cwm","SES_boot_v_actual","in_95ci","in_100ci","in_95ci_para","in_100ci_para")

saveRDS(object = simulation_output,file = "output_distributions/simulation_output_proportional.rds")
simulation_output_proportional<-readRDS(file = "output_distributions/simulation_output_proportional.rds")
#################################


simulation_output_proportional$moment<-as.character(simulation_output_proportional$moment)
simulation_output_proportional$boot_mean<-as.numeric(as.character(simulation_output_proportional$boot_mean))
simulation_output_proportional$sample_proportion<-as.numeric(as.character(simulation_output_proportional$sample_proportion))
simulation_output_proportional$SES_boot_v_cwm<-as.numeric(as.character(simulation_output_proportional$SES_boot_v_cwm))
simulation_output_proportional$SES_boot_v_actual<-as.numeric(as.character(simulation_output_proportional$SES_boot_v_actual))
simulation_output_proportional$cwm_mean<-as.numeric(as.character(simulation_output_proportional$cwm_mean))
simulation_output_proportional$actual_mean<-as.numeric(as.character(simulation_output_proportional$actual_mean))
simulation_output_proportional$in_95ci<-as.numeric(as.character(simulation_output_proportional$in_95ci))
simulation_output_proportional$in_100ci<-as.numeric(as.character(simulation_output_proportional$in_100ci))
simulation_output_proportional$parametric_mean<-as.numeric(as.character(simulation_output_proportional$parametric_mean))


#Generate output data showing how percent in ci changes with sample size
source("r_functions/prop_correct_cis.R")
ci_output_proportional<-proportion_ci_correct_proportional_sampling(simulation_output = simulation_output_proportional)







##################################

#Plots

library(ggplot2)




ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="mean"),], aes(x= sample_proportion, y=cwm_mean))+
  geom_point(position=position_dodge(width = .01),color="magenta",alpha=.5,shape=1)+geom_abline(aes(slope = 0,intercept = actual_mean))+
  geom_point(aes(x= sample_proportion, y=boot_mean),color="blue",alpha=0.5,shape=4)+
  geom_point(aes(x= sample_proportion, y=parametric_mean),color="green",alpha=0.25,shape=4)+
  facet_wrap(~trait+site,scales = "free_y")+ggtitle("mean")

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="variance"),], aes(x= sample_proportion, y=cwm_mean))+
  geom_point(position=position_dodge(width = .01),color="magenta",alpha=.5,shape=1)+geom_abline(aes(slope = 0,intercept = actual_mean))+
  geom_point(aes(x= sample_proportion, y=boot_mean),color="blue",alpha=0.5,shape=4)+
  geom_point(aes(x= sample_proportion, y=parametric_mean),color="green",alpha=0.25,shape=4)+
  facet_wrap(~trait+site,scales = "free_y")+ggtitle("variance")

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="skewness"),], aes(x= sample_proportion, y=cwm_mean))+
  geom_point(position=position_dodge(width = .01),color="magenta",alpha=.5,shape=1)+geom_abline(aes(slope = 0,intercept = actual_mean))+
  geom_point(aes(x= sample_proportion, y=boot_mean),color="blue",alpha=0.5,shape=4)+
  geom_point(aes(x= sample_proportion, y=parametric_mean),color="green",alpha=0.25,shape=4)+
  geom_point(aes(x= sample_proportion, y=actual_mean),color="green",alpha=0,shape=4)+ #added this to make sure true mean was included in the plot
  facet_wrap(~trait+site,scales = "free_y")+ggtitle("skewness")

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="kurtosis"),], aes(x= sample_proportion, y=cwm_mean))+
  geom_point(position=position_dodge(width = .01),color="magenta",alpha=.5,shape=1)+geom_abline(aes(slope = 0,intercept = actual_mean))+
  geom_point(aes(x= sample_proportion, y=boot_mean),color="blue",alpha=0.5,shape=4)+
  geom_point(aes(x= sample_proportion, y=parametric_mean),color="green",alpha=0.25,shape=4)+
  geom_point(aes(x= sample_proportion, y=actual_mean),color="green",alpha=0,shape=4)+ #added this to make sure true mean was included in the plot
  facet_wrap(~trait+site,scales = "free_y")+ggtitle("kurtosis")
















ggplot(data = ci_output_proportional[which(ci_output_proportional$moment=="mean"),], aes(x= sample_proportion, y=proportion_ci95_correct))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("mean")

ggplot(data = ci_output[which(ci_output$moment=="variance"),], aes(x= sample_proportion, y=proportion_ci95_correct))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("variance")

ggplot(data = ci_output[which(ci_output$moment=="skewness"),], aes(x= sample_proportion, y=proportion_ci95_correct))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("skewness")

ggplot(data = ci_output[which(ci_output$moment=="kurtosis"),], aes(x= sample_proportion, y=proportion_ci95_correct))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("kurtosis")

ggplot(data = ci_output[which(ci_output$moment=="mean"),], aes(x= sample_proportion, y=proportion_ci100_correct))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("mean")

ggplot(data = ci_output[which(ci_output$moment=="variance"),], aes(x= sample_proportion, y=proportion_ci100_correct))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("variance")

ggplot(data = ci_output[which(ci_output$moment=="skewness"),], aes(x= sample_proportion, y=proportion_ci100_correct))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("skewness")

ggplot(data = ci_output[which(ci_output$moment=="kurtosis"),], aes(x= sample_proportion, y=proportion_ci100_correct))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("kurtosis")







ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="mean"),], aes(x= sample_proportion, y=boot_mean))+
  geom_point(position=position_dodge(width = .1))+
  facet_wrap(~trait+site)

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="mean"),], aes(x= sample_proportion, y=cwm_mean))+
  geom_point(position=position_dodge(width = .01),color="magenta",alpha=.5)+geom_abline(aes(slope = 0,intercept = actual_mean))+
  geom_point(aes(x= sample_proportion, y=boot_mean),color="blue",alpha=0.5)+
  facet_wrap(~trait+site)+ggtitle("mean")

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="variance"),], aes(x= sample_proportion, y=cwm_mean))+
  geom_point(position=position_dodge(width = .01),color="magenta",alpha=.5)+geom_abline(aes(slope = 0,intercept = actual_mean))+
  geom_point(aes(x= sample_proportion, y=boot_mean),color="blue",alpha=0.5)+
  facet_wrap(~trait+site)+ggtitle("variance")

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="skewness"),], aes(x= sample_proportion, y=cwm_mean))+
  geom_point(position=position_dodge(width = .01),color="magenta",alpha=.5)+geom_abline(aes(slope = 0,intercept = actual_mean))+
  geom_point(aes(x= sample_proportion, y=boot_mean),color="blue",alpha=0.5)+
  facet_wrap(~trait+site)+ggtitle("skewness")

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="kurtosis"),], aes(x= sample_proportion, y=cwm_mean))+
  geom_point(position=position_dodge(width = .01),color="magenta",alpha=.5)+geom_abline(aes(slope = 0,intercept = actual_mean))+
  geom_point(aes(x= sample_proportion, y=boot_mean),color="blue",alpha=0.5)+
  facet_wrap(~trait+site)+ggtitle("kurtosis")


ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="mean"),], aes(x= sample_proportion, y=boot_mean))+
  geom_point(position=position_dodge(width = .1),color="blue")+
  facet_wrap(~trait+site)

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="mean"),], aes(x= sample_proportion, y=SES_boot_v_cwm))+
  geom_point(position=position_dodge(width = .1),color="blue")+
  facet_wrap(~trait+site)

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="mean"),], aes(x= sample_proportion, y=SES_boot_v_actual))+
  geom_point(position=position_dodge(width = .1),color="blue")+
  facet_wrap(~trait+site)

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="variance"),], aes(x= sample_proportion, y=SES_boot_v_actual))+
  geom_point(position=position_dodge(width = .1),color="blue")+
  facet_wrap(~trait+site)+ggtitle("Variance")

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="skewness"),], aes(x= sample_proportion, y=SES_boot_v_actual))+
  geom_point(position=position_dodge(width = .1),color="blue")+
  facet_wrap(~trait+site)+ggtitle("skewnesss")

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="kurtosis"),], aes(x= sample_proportion, y=SES_boot_v_actual))+
  geom_point(position=position_dodge(width = .1),color="blue")+
  facet_wrap(~trait+site)+ggtitle("kurtosis")



simulation_output_proportional$SES_boot_v_cwm




ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="mean"),], aes(x= sample_proportion, y=in_95ci))+
  geom_point(position=position_dodge(width = .1),alpha=0.1)+
  facet_wrap(~trait+site)

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="mean"),], aes(x= sample_proportion, y=in_100ci))+
  geom_point(position=position_dodge(width = .1),alpha=0.1)+
  facet_wrap(~trait+site)+ggtitle("mean")

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="variance"),], aes(x= sample_proportion, y=in_100ci))+
  geom_point(position=position_dodge(width = .1),alpha=0.1)+
  facet_wrap(~trait+site)+ggtitle("variance")

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="skewness"),], aes(x= sample_proportion, y=in_100ci))+
  geom_point(position=position_dodge(width = .1),alpha=0.1)+
  facet_wrap(~trait+site)+ggtitle("skewness")

ggplot(data = simulation_output_proportional[which(simulation_output_proportional$moment=="kurtosis"),], aes(x= sample_proportion, y=in_100ci))+
  geom_point(position=position_dodge(width = .1),alpha=0.1)+
  facet_wrap(~trait+site)+ggtitle("kurtosis")





