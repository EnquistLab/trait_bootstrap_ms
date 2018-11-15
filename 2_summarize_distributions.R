#Summarize output from distribution simulations



#trait
#site
#method
#n individuals
#min
#mean
#max

library(moments)

file_key_rmbl_random<-matrix(nrow = length(list.files("output_distributions/RMBL_random_sampling/")),ncol = 5)
file_key_rmbl_random<-as.data.frame(file_key_rmbl_random)
colnames(file_key_rmbl_random)<-c("trait","site","method","indiv_per_spp","file")

#get a guide to the files
for(i in 1:length(list.files("output_distributions/RMBL_random_sampling/",full.names = T))){
  
file_name_i<-list.files("output_distributions/RMBL_random_sampling/",full.names = T)[i]
file_name_i<-unlist(strsplit(file_name_i,split = "/"))[3]
file_key_rmbl_random$site[i]<-strsplit(file_name_i,'.',fixed = T)[[1]][2]
file_key_rmbl_random$trait[i]<-strsplit(file_name_i,'.',fixed = T)[[1]][3]
file_key_rmbl_random$indiv_per_spp[i]<-strsplit(file_name_i,'.',fixed = T)[[1]][4]
file_key_rmbl_random$indiv_per_spp[i]<-strsplit(file_key_rmbl_random$indiv_per_spp[i],split = "_")[[1]][2]
file_key_rmbl_random$file[i]<-list.files("output_distributions/RMBL_random_sampling/",full.names = T)[i]
file_key_rmbl_random$method[i]<-"boostrap_with_random_sampling"
print(paste(round(i/length(list.files("output_distributions/RMBL_random_sampling/",full.names = T))*100,digits = 2),"percent done"))
rm(i,file_name_i)

}

#summarize files


simulation_summary<-matrix(nrow = nrow(unique(file_key_rmbl_random[,1:4])),ncol = 16)
simulation_summary<-as.data.frame(simulation_summary)
colnames(simulation_summary)<-c("trait","site","method","indiv_per_spp","min_mean","mean_mean","max_mean",
                                "min_var","mean_var","max_var",
                                "min_skew","mean_skew","max_skew",
                                "min_kurt","mean_kurt","max_kurt")

for(i in 1:nrow(unique(file_key_rmbl_random[,1:4]))){
  
  set_i<-unique(file_key_rmbl_random[,1:4])[i,]
  files_i<-file_key_rmbl_random$file[which(file_key_rmbl_random$trait==set_i$trait &
                   file_key_rmbl_random$site==set_i$site &
                   file_key_rmbl_random$indiv_per_spp==set_i$indiv_per_spp)]
  
  f_out<-matrix(nrow=length(files_i),ncol = 4)
  f_out<-as.data.frame(f_out)
  colnames(f_out)<-c("mean","var","skew","kurt")
  
  for(f in 1:length(files_i)){
  #print(f)  
  data_f<-read.csv(file = files_i[f])  
  
  #print(mean(rowMeans(data_f)))
  
  f_out$mean[f] <- mean(rowMeans(data_f))
  f_out$var[f] <- mean(apply(X = data_f,MARGIN = 1,FUN = var))
  f_out$skew[f] <- mean(apply(X = data_f,MARGIN = 1,FUN = skewness) )
  f_out$kurt[f] <- mean(apply(X = data_f,MARGIN = 1,FUN = kurtosis))
  
    
  }#f
  
simulation_summary$trait[i]<-set_i$trait
simulation_summary$site[i]<-set_i$site
simulation_summary$method[i]<-set_i$method
simulation_summary$indiv_per_spp[i]<-set_i$indiv_per_spp

simulation_summary$min_mean[i]<-min(f_out$mean)
simulation_summary$mean_mean[i]<-mean(f_out$mean)
simulation_summary$max_mean[i]<-max(f_out$mean)

simulation_summary$min_var[i]<-min(f_out$var)
simulation_summary$mean_var[i]<-mean(f_out$var)
simulation_summary$max_var[i]<-max(f_out$var)

simulation_summary$min_skew[i]<-min(f_out$skew)
simulation_summary$mean_skew[i]<-mean(f_out$skew)
simulation_summary$max_skew[i]<-max(f_out$skew)

simulation_summary$min_kurt[i]<-min(f_out$kurt)
simulation_summary$mean_kurt[i]<-mean(f_out$kurt)
simulation_summary$max_kurt[i]<-max(f_out$kurt)


print(paste(round(i/nrow(unique(file_key_rmbl_random[,1:4]))*100,digits = 2),"percent done"))

    
}



file_key_rmbl_random_species_mean<-matrix(nrow = length(list.files("output_distributions/RMBL_random_sampling_mean_values/")),ncol = 5)
file_key_rmbl_random_species_mean<-as.data.frame(file_key_rmbl_random)
colnames(file_key_rmbl_random_species_mean)<-c("trait","site","method","indiv_per_spp","file")

#get a guide to the files
for(i in 1:length(list.files("output_distributions/RMBL_random_sampling_mean_values/",full.names = T))){
  
  file_name_i<-list.files("output_distributions/RMBL_random_sampling_mean_values/",full.names = T)[i]
  file_name_i<-unlist(strsplit(file_name_i,split = "/"))[3]
  file_key_rmbl_random_species_mean$site[i]<-strsplit(file_name_i,'.',fixed = T)[[1]][2]
  file_key_rmbl_random_species_mean$trait[i]<-strsplit(file_name_i,'.',fixed = T)[[1]][3]
  file_key_rmbl_random_species_mean$indiv_per_spp[i]<-strsplit(file_name_i,'.',fixed = T)[[1]][4]
  file_key_rmbl_random_species_mean$indiv_per_spp[i]<-strsplit(file_key_rmbl_random_species_mean$indiv_per_spp[i],split = "_")[[1]][2]
  file_key_rmbl_random_species_mean$file[i]<-list.files("output_distributions/RMBL_random_sampling_mean_values/",full.names = T)[i]
  file_key_rmbl_random_species_mean$method[i]<-"species_mean_with_random_sampling"
  print(paste(round(i/length(list.files("output_distributions/RMBL_random_sampling_mean_values/",full.names = T))*100,digits = 2),"percent done"))
  rm(i,file_name_i)
  
}


simulation_summary2<-matrix(nrow = nrow(unique(file_key_rmbl_random_species_mean[,1:4])),ncol = 16)
simulation_summary2<-as.data.frame(simulation_summary)
colnames(simulation_summary2)<-c("trait","site","method","indiv_per_spp","min_mean","mean_mean","max_mean",
                                "min_var","mean_var","max_var",
                                "min_skew","mean_skew","max_skew",
                                "min_kurt","mean_kurt","max_kurt")


for(i in 1:nrow(unique(file_key_rmbl_random_species_mean[,1:4]))){
  
  set_i<-unique(file_key_rmbl_random_species_mean[,1:4])[i,]
  files_i<-file_key_rmbl_random_species_mean$file[which(file_key_rmbl_random_species_mean$trait==set_i$trait &
                                             file_key_rmbl_random_species_mean$site==set_i$site &
                                             file_key_rmbl_random_species_mean$indiv_per_spp==set_i$indiv_per_spp)]
  
  f_out<-matrix(nrow=length(files_i),ncol = 4)
  f_out<-as.data.frame(f_out)
  colnames(f_out)<-c("mean","var","skew","kurt")
  
  for(f in 1:length(files_i)){
    #print(f)  
    data_f<-read.csv(file = files_i[f])  
    
    #print(mean(rowMeans(data_f)))
    
    f_out$mean[f] <- mean(rowMeans(data_f))
    f_out$var[f] <- mean(apply(X = data_f,MARGIN = 1,FUN = var))
    f_out$skew[f] <- mean(apply(X = data_f,MARGIN = 1,FUN = skewness) )
    f_out$kurt[f] <- mean(apply(X = data_f,MARGIN = 1,FUN = kurtosis))
    
    
  }#f
  
  simulation_summary2$trait[i]<-set_i$trait
  simulation_summary2$site[i]<-set_i$site
  simulation_summary2$method[i]<-set_i$method
  simulation_summary2$indiv_per_spp[i]<-set_i$indiv_per_spp
  
  simulation_summary2$min_mean[i]<-min(f_out$mean)
  simulation_summary2$mean_mean[i]<-mean(f_out$mean)
  simulation_summary2$max_mean[i]<-max(f_out$mean)
  
  simulation_summary2$min_var[i]<-min(f_out$var)
  simulation_summary2$mean_var[i]<-mean(f_out$var)
  simulation_summary2$max_var[i]<-max(f_out$var)
  
  simulation_summary2$min_skew[i]<-min(f_out$skew)
  simulation_summary2$mean_skew[i]<-mean(f_out$skew)
  simulation_summary2$max_skew[i]<-max(f_out$skew)
  
  simulation_summary2$min_kurt[i]<-min(f_out$kurt)
  simulation_summary2$mean_kurt[i]<-mean(f_out$kurt)
  simulation_summary2$max_kurt[i]<-max(f_out$kurt)
  
  
  print(paste(round(i/nrow(unique(file_key_rmbl_random_species_mean[,1:4]))*100,digits = 2),"percent done"))
  
  
}

simulation_summary<-rbind(simulation_summary,simulation_summary2)

####################

###################

library(ggplot2)

simulation_summary$indiv_per_spp<-as.numeric(simulation_summary$indiv_per_spp)

ggplot(data = simulation_summary, aes(x= log(indiv_per_spp), y=mean_mean, color=method))+
  geom_point(position=position_dodge(width = .1))+
  geom_errorbar(aes(ymin = min_mean, ymax=max_mean),position=position_dodge(width = .1))+
  facet_wrap(~trait+site)

ggplot(data = simulation_summary, aes(x= log(indiv_per_spp), y=mean_var, color=method))+
  geom_point(position=position_dodge(width = .1))+
  geom_errorbar(aes(ymin = min_var, ymax=max_var),position=position_dodge(width = .1))+
  facet_wrap(~trait+site)
  

ggplot(data = simulation_summary, aes(x= log(indiv_per_spp), y=mean_skew, color=method))+
  geom_point(position=position_dodge(width = .1))+
  geom_errorbar(aes(ymin = min_skew, ymax=max_skew),position=position_dodge(width = .1))+
  facet_wrap(~trait+site)

ggplot(data = simulation_summary, aes(x= log(indiv_per_spp), y=mean_kurt, color=method))+
  geom_point(position=position_dodge(width = .1))+
  geom_errorbar(aes(ymin = min_kurt, ymax=max_kurt),position=position_dodge(width = .1))+
  facet_wrap(~trait+site)
  


##########
#add true values tp simulation outputs

simulation_summary$obs_mean<-NA
simulation_summary$obs_var<-NA
simulation_summary$obs_skew<-NA
simulation_summary$obs_kurt<-NA
traits<-readRDS("data/all_traits_RMBL.rds")

for(i in 1:nrow(unique(simulation_summary[c('site','trait')]))){
  site<-unique(simulation_summary[c('site','trait')])[i,1]
  trait<-unique(simulation_summary[c('site','trait')])[i,2]
  
  
  
  
  simulation_summary$obs_mean[which(simulation_summary$trait==trait & 
                                      simulation_summary$site==site)] <- mean(unlist(traits[which(traits$site==site),][trait]),na.rm = T)    
  
  simulation_summary$obs_var[which(simulation_summary$trait==trait & 
                                     simulation_summary$site==site)] <- var(unlist(traits[which(traits$site==site),][trait]),na.rm = T)    
  
  simulation_summary$obs_skew[which(simulation_summary$trait==trait & 
                                      simulation_summary$site==site)] <- skewness(unlist(traits[which(traits$site==site),][trait]),na.rm = T)    
  simulation_summary$obs_kurt[which(simulation_summary$trait==trait & 
                                      simulation_summary$site==site)] <- kurtosis(unlist(traits[which(traits$site==site),][trait]),na.rm = T)    
  
  
  
}

#############
#values relative to actual

ggplot(data = simulation_summary, aes(x= log10(indiv_per_spp), y=mean_mean-obs_mean, color=method))+
  geom_point(position=position_dodge(width = .1))+
  geom_errorbar(aes(ymin = min_mean - obs_mean, ymax=max_mean - obs_mean),position=position_dodge(width = .1))+
  facet_wrap(~trait+site)+geom_hline(yintercept = 0,alpha=0.5)


ggplot(data = simulation_summary, aes(x= log10(indiv_per_spp), y=mean_var-obs_var, color=method))+
  geom_point(position=position_dodge(width = .1))+
  geom_errorbar(aes(ymin = min_var-obs_var, ymax=max_var-obs_var),position=position_dodge(width = .1))+
  facet_wrap(~trait+site)+geom_hline(yintercept = 0,alpha=0.5)


ggplot(data = simulation_summary, aes(x= log10(indiv_per_spp), y=mean_skew-obs_skew, color=method))+
  geom_point(position=position_dodge(width = .1))+
  geom_errorbar(aes(ymin = min_skew-obs_skew, ymax=max_skew-obs_skew),position=position_dodge(width = .1))+
  facet_wrap(~trait+site)+geom_hline(yintercept = 0,alpha=0.5)

ggplot(data = simulation_summary, aes(x= log10(indiv_per_spp), y=mean_kurt-obs_kurt, color=method))+
  geom_point(position=position_dodge(width = .1))+
  geom_errorbar(aes(ymin = min_kurt-obs_kurt, ymax=max_kurt-obs_kurt),position=position_dodge(width = .1))+
  facet_wrap(~trait+site)+geom_hline(yintercept = 0,alpha=0.5)











##########
#maybe new analyses that calculate whether true mean falls within bootstrapped mean 95 CI?



