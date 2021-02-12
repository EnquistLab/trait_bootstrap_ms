#analyze and plot simulation results


##############################################


#read in data
simdata <- readRDS("output_data/simulation_results.RDS")

#need to further tidy up data (add a column for moment)

  #could probably do this with pipes, but I'll go oldschool and cut down the googling

sim_mean <- simdata[c("site","trait","method","sample_size","n",
                       "mean","ci_low_mean","ci_high_mean","true_mean")]
sim_mean$moment <- "mean"
colnames(sim_mean)[grep(pattern = "ci_low",x = colnames(sim_mean))]<-"ci_low"
colnames(sim_mean)[grep(pattern = "ci_high",x = colnames(sim_mean))]<-"ci_high"
colnames(sim_mean)[grep(pattern = "true",x = colnames(sim_mean))]<-"true_value"
colnames(sim_mean)[grep(pattern = "mean",x = colnames(sim_mean))]<-"estimate"

sim_var <- simdata[c("site","trait","method","sample_size","n",
                      "var","ci_low_var","ci_high_var","true_variance")]
sim_var$moment <- "variance"
colnames(sim_var)[grep(pattern = "ci_low",x = colnames(sim_var))]<-"ci_low"
colnames(sim_var)[grep(pattern = "ci_high",x = colnames(sim_var))]<-"ci_high"
colnames(sim_var)[grep(pattern = "true",x = colnames(sim_var))]<-"true_value"
colnames(sim_var)[grep(pattern = "var",x = colnames(sim_var))]<-"estimate"


sim_skew <- simdata[c("site","trait","method","sample_size","n",
                      "skew","ci_low_skew","ci_high_skew","true_skewness")]
sim_skew$moment <- "skewness"
colnames(sim_skew)[grep(pattern = "ci_low",x = colnames(sim_skew))]<-"ci_low"
colnames(sim_skew)[grep(pattern = "ci_high",x = colnames(sim_skew))]<-"ci_high"
colnames(sim_skew)[grep(pattern = "true",x = colnames(sim_skew))]<-"true_value"
colnames(sim_skew)[grep(pattern = "skew",x = colnames(sim_skew))]<-"estimate"



sim_kurt <- simdata[c("site","trait","method","sample_size","n",
                      "kurt","ci_low_kurt","ci_high_Kurt","true_kurtosis")]
sim_kurt$moment <- "kurtosis"
colnames(sim_kurt)[grep(pattern = "ci_low",x = colnames(sim_kurt))]<-"ci_low"
colnames(sim_kurt)[grep(pattern = "ci_high",x = colnames(sim_kurt))]<-"ci_high"
colnames(sim_kurt)[grep(pattern = "true",x = colnames(sim_kurt))]<-"true_value"
colnames(sim_kurt)[grep(pattern = "kurt",x = colnames(sim_kurt))]<-"estimate"

simdata <- rbind(sim_mean,sim_var,sim_skew,sim_kurt)
rm(sim_mean,sim_var,sim_skew,sim_kurt)


library(ggplot2)


##############################################################
#Accuracy vs sample size

ggplot(data = simdata[which(simdata$moment=="mean"),], mapping = aes(y=(estimate-true_value)/true_value,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Mean")


ggplot(data = simdata[which(simdata$moment=="variance"),], mapping = aes(y=(estimate-true_value)/true_value,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Variance")

ggplot(data = simdata[which(simdata$moment=="skewness"),], mapping = aes(y=(estimate-true_value)/true_value,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Skewness")

ggplot(data = simdata[which(simdata$moment=="kurtosis"),], mapping = aes(y=(estimate-true_value)/true_value,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Kurtosis")


############################################################

source("r_functions/sumarize_sims.R")
sim_summary <- summarize_simulations(simulation_result = simulation_result)
colnames(sim_summary)

ggplot(data = sim_summary[which(sim_summary$moment=="mean"),], mapping = aes(y=pct_in_CI,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Mean")


ggplot(data = sim_summary[which(sim_summary$moment=="variance"),], mapping = aes(y=pct_in_CI,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Variance")

ggplot(data = sim_summary[which(sim_summary$moment=="skewness"),], mapping = aes(y=pct_in_CI,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Skewness")

ggplot(data = sim_summary[which(sim_summary$moment=="kurtosis"),], mapping = aes(y=pct_in_CI,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Kurtosis")


######

ggplot(data = sim_summary[which(sim_summary$moment=="mean"),], 
       mapping = aes(y=fractional_difference,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Mean")


ggplot(data = sim_summary[which(sim_summary$moment=="variance"),], 
       mapping = aes(y=fractional_difference,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Variance")

ggplot(data = sim_summary[which(sim_summary$moment=="skewness"),], 
       mapping = aes(y=fractional_difference,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Skewness")

ggplot(data = sim_summary[which(sim_summary$moment=="kurtosis"),], 
       mapping = aes(y=fractional_difference,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Kurtosis")


