#effect sizes

  #Cohen's d is defined as the difference between two means divided by the pooled standard deviation for the data

source("r_functions/tidy_simdata.R")
source("r_functions/get_effect_sizes.R")
library(tidyverse)
library(ggplot2)
library(effsize)

### Colorado herbs

  simdata =
    tidy_simdata(readRDS("output_data/simulation_results.RDS"))

  colorado_effect_sizes <- get_effect_sizes(simdata)
  
  saveRDS(object = colorado_effect_sizes,
          file = "output_data/effect_sizes_colorado.rds")

#########################################################

### Panama trees
  
  simdata_panama <-
    tidy_simdata(readRDS("output_data/panama_simulation_results.RDS"))
  
  panama_effect_sizes <- get_effect_sizes(tidied_simdata = simdata_panama)
  
  saveRDS(object = panama_effect_sizes,
          file = "output_data/effect_sizes_panama.rds")
  
#########################################################

  ### Portal rodents
  
  simdata_rats <-
    tidy_simdata(readRDS("output_data/simulation_results_rodents.RDS"))

  rodent_effect_sizes <- get_effect_sizes(simdata_rats)
  
  saveRDS(object = rodent_effect_sizes,
          file = "output_data/effect_sizes_rodents.rds")
  

#########################################################
  
  ### Experimental tadpoles

  simdata_frogs <-
    tidy_simdata(readRDS("output_data/simulation_results_treefrogs.RDS"))

  frog_effect_sizes <- get_effect_sizes(simdata_frogs)
  
  saveRDS(object = frog_effect_sizes,
          file = "output_data/effect_sizes_frogs.rds")
  
#########################################################

  ### Timeseries phytoplankton
  
  simdata_plankton <- 
    tidy_simdata(readRDS("output_data/simulation_results_phyto_subset_scaled.RDS"))

  plankton_effect_sizes <- get_effect_sizes(simdata_plankton)
  
  saveRDS(object = plankton_effect_sizes,
          file = "output_data/effect_sizes_plankton.rds")
  
  
  
#########################################################


colorado_effect_sizes %>%
  ggplot(mapping = aes(x=sample_size,y=cohen_d,color = moment))+
  geom_point()+
  facet_wrap(method_1~method_2)

colorado_effect_sizes %>%
  filter(moment == "mean") %>%
  ggplot(mapping = aes(x=sample_size,
                       y=cohen_d,
                       color = trait))+
  geom_point()+
  facet_grid(method_1~method_2)+
  ggtitle("Mean")
  
colorado_effect_sizes %>%
  filter(moment == "variance") %>%
  ggplot(mapping = aes(x=sample_size,
                       y=cohen_d,
                       color = trait))+
  geom_point()+
  facet_grid(method_1~method_2)



colorado_effect_sizes %>%
  filter(moment == "skewness") %>%
  ggplot(mapping = aes(x=sample_size,
                       y=cohen_d,
                       color = trait))+
  geom_point()+
  facet_grid(method_1~method_2)

colorado_effect_sizes %>%
  filter(moment == "kurtosis") %>%
  ggplot(mapping = aes(x=sample_size,
                       y=cohen_d,
                       color = trait))+
  geom_point()+
  facet_grid(method_1~method_2)

colorado_effect_sizes %>%
  filter(sample_size == 9)%>%
  mutate(treatment = paste(method_1,method_2))%>%
  group_by(treatment) %>% 
  ggplot(aes(x=treatment,y=cohen_d))+
  geom_violin()+
  facet_wrap(~moment)

