#Stats
###########################################################################################
###########################################################################################
#Load data

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
simulation_output$parametric_mean<-as.numeric(as.character(simulation_output$parametric_mean))

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
simulation_output_proportional$parametric_mean<-as.numeric(as.character(simulation_output_proportional$parametric_mean))



#Convert to skinny format

simulation_output$sampling_method <- "number"
simulation_output_proportional$sampling_method <- "proportion"
colnames(simulation_output)[1]<-"sample_size_or_proportion"
colnames(simulation_output_proportional)[1]<-"sample_size_or_proportion"
colnames(simulation_output_proportional)<-colnames(simulation_output)
combined_sim_out<-rbind(simulation_output,simulation_output_proportional)
source("r_functions/tidy_up_output.R")
tidy_output<-tidy_sim_output(simulation_output = combined_sim_out)
rm(simulation_output,simulation_output_proportional,tidy_sim_output)

tidy_output$ordinal_performance

tidy_output$ordinal_performance <- factor(tidy_output$ordinal_performance,levels=c("worst","intermediate","best"),ordered=TRUE)


###########################################################################
###########################################################################

#

#Ordered logistic regression (polr() fx in mass package)
#Best_method~sample_size+trait+moment+site
#Conveniently, this doesn’t require and variable standardization


library(MASS)
?polr

polr(formula = ordinal_performance ~ trait + site + moment + estimation_method + total_sample_size,
     data = tidy_output)


colnames(tidy_output)
