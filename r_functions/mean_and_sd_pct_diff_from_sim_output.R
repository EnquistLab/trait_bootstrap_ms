mean_and_sd_pct_diff_from_sim_output <- function(simulation_output){
  
stsm <- unique(simulation_output[c("sample_size","trait","site","moment")])
output<-NULL
  
for(i in 1:nrow(stsm)){
  
sample_size <- stsm$sample_size[i]  
trait <- as.character(stsm$trait[i])
site <- as.character(stsm$site[i])  
moment <- stsm$moment[i]


data_i<- simulation_output[which(simulation_output$sample_size==sample_size &
                          simulation_output$trait==trait & 
                          simulation_output$site == site &
                          simulation_output$moment == moment),]


mean_pct_diff_nonpara<- mean(data_i$pct_diff_nonpara)
sd_pct_diff_nonpara<- sd(data_i$pct_diff_nonpara)
min_pct_diff_nonpara<- min(data_i$pct_diff_nonpara)
max_pct_diff_nonpara<- max(data_i$pct_diff_nonpara)

nonpara_out_i<-cbind(sample_size,trait,site,moment,"nonparametric",
                mean_pct_diff_nonpara,sd_pct_diff_nonpara,min_pct_diff_nonpara,max_pct_diff_nonpara)


mean_pct_diff_para<- mean(data_i$pct_diff_para)
sd_pct_diff_para<- sd(data_i$pct_diff_para)
min_pct_diff_para<- min(data_i$pct_diff_para)
max_pct_diff_para<- max(data_i$pct_diff_para)

para_out_i<-cbind(sample_size,trait,site,moment,"parametric",
                mean_pct_diff_para,sd_pct_diff_para,min_pct_diff_para,max_pct_diff_para)



mean_pct_diff_cwm<- mean(data_i$pct_diff_cwm)
sd_pct_diff_cwm<- sd(data_i$pct_diff_cwm)
min_pct_diff_cwm<- min(data_i$pct_diff_cwm)
max_pct_diff_cwm<- max(data_i$pct_diff_cwm)

mean_out_i<-cbind(sample_size,trait,site,moment,"mean",
                     mean_pct_diff_cwm,sd_pct_diff_cwm,min_pct_diff_cwm,max_pct_diff_cwm)



output_i <- rbind(nonpara_out_i,para_out_i,mean_out_i)

#output_i <- cbind(sample_size,trait,site,moment,
#                  mean_pct_diff_nonpara,sd_pct_diff_nonpara,min_pct_diff_nonpara,max_pct_diff_nonpara,
#                  mean_pct_diff_para,sd_pct_diff_para,min_pct_diff_para,max_pct_diff_para,
#                  mean_pct_diff_cwm,sd_pct_diff_cwm,min_pct_diff_cwm,max_pct_diff_cwm)

output <- rbind(output,output_i)
  
  } # 
  
  
colnames(output)<-  c("sample_size","trait","site","moment","estimation_method","mean_pct_diff","sd_pct_diff","min_pct_diff","max_pct_diff" )
  
return(output)  
  
  
  
  
  
  
}#