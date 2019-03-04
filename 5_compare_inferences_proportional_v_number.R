#Comparing sampling via proportion and absolute number of samples


#################################


simulation_output<-readRDS("output_distributions/simulation_output.rds")
simulation_output$moment<-as.character(simulation_output$moment)
simulation_output$boot_mean<-as.numeric(as.character(simulation_output$boot_mean))
simulation_output$sample_size<-as.numeric(as.character(simulation_output$sample_size))
simulation_output$SES_boot_v_cwm<-as.numeric(as.character(simulation_output$SES_boot_v_cwm))
simulation_output$SES_boot_v_actual<-as.numeric(as.character(simulation_output$SES_boot_v_actual))
simulation_output$cwm_mean<-as.numeric(as.character(simulation_output$cwm_mean))
simulation_output$actual_mean<-as.numeric(as.character(simulation_output$actual_mean))
simulation_output$in_95ci<-as.numeric(as.character(simulation_output$in_95ci))
simulation_output$in_100ci<-as.numeric(as.character(simulation_output$in_100ci))
simulation_output$parametric_mean<-as.numeric(as.character(simulation_output$parametric_mean))
simulation_output$in_95ci_para<-as.numeric(as.character(simulation_output$in_95ci_para))
simulation_output$in_100_ci_para<-as.numeric(as.character(simulation_output$in_100_ci_para))


#Generate output data showing how percent in ci changes with sample size
source("r_functions/prop_correct_cis.R")
ci_output<-proportion_ci_correct(simulation_output = simulation_output)

#################################

simulation_output_proportional<-readRDS(file = "output_distributions/simulation_output_proportional.rds")
simulation_output_proportional$moment<-as.character(simulation_output_proportional$moment)
simulation_output_proportional$boot_mean<-as.numeric(as.character(simulation_output_proportional$boot_mean))
simulation_output_proportional$sample_proportion<-as.numeric(as.character(simulation_output_proportional$sample_proportion))
simulation_output_proportional$SES_boot_v_cwm<-as.numeric(as.character(simulation_output_proportional$SES_boot_v_cwm))
simulation_output_proportional$SES_boot_v_actual<-as.numeric(as.character(simulation_output_proportional$SES_boot_v_actual))
simulation_output_proportional$cwm_mean<-as.numeric(as.character(simulation_output_proportional$cwm_mean))
simulation_output_proportional$actual_mean<-as.numeric(as.character(simulation_output_proportional$actual_mean))
simulation_output_proportional$in_95ci<-as.numeric(as.character(simulation_output_proportional$in_95ci))
simulation_output_proportional$in_100ci<-as.numeric(as.character(simulation_output_proportional$in_100ci))

#Generate output data showing how percent in ci changes with sample size
source("r_functions/prop_correct_cis.R")
ci_output_proportional<-proportion_ci_correct_proportional_sampling(simulation_output = simulation_output_proportional)

##################################


ci_output$sampling_method<-"number"
ci_output_proportional$sampling_method<-"proportion"
colnames(ci_output)[4]<-"sample_size_or_proportion"
colnames(ci_output_proportional)[4]<-"sample_size_or_proportion"
ci_output_combind<-rbind(ci_output,ci_output_proportional)
ci_output_combind$total_samples<-as.numeric(as.character(ci_output_combind$total_samples))

library(ggplot2)


ggplot(data = ci_output_combind[which(ci_output_combind$moment=="mean"),], aes(x= total_samples, y=proportion_ci95_correct,color = sampling_method,shape=method))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("mean")



ggplot(data = ci_output_combind[which(ci_output_combind$moment=="mean"),], aes(x= total_samples, y=proportion_ci95_correct_para,color = sampling_method,shape=method))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("mean")



ggplot(data = ci_output_combind[which(ci_output_combind$moment=="variance"),], aes(x= total_samples, y=proportion_ci95_correct,color = sampling_method,shape=method))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("variance")

ggplot(data = ci_output_combind[which(ci_output_combind$moment=="skewness"),], aes(x= total_samples, y=proportion_ci95_correct,color = sampling_method,shape=method))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("skewness")

ggplot(data = ci_output_combind[which(ci_output_combind$moment=="kurtosis"),], aes(x= total_samples, y=proportion_ci95_correct,color = sampling_method,shape=method))+
  geom_point(position=position_dodge(width = .1))+ylim(c(0,1))+
  facet_wrap(~trait+site)+ggtitle("kurtosis")



