source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

#### Read Data ####


#read in data
simdata =
  tidy_simdata(readRDS("output_data/simulation_results.RDS"))%>%
  mutate(dataset = "Colorado Herbs")

simdata_panama <-
  tidy_simdata(readRDS("output_data/panama_simulation_results.RDS"))%>%
  mutate(dataset = "Panama Trees")

simdata_rats <-
  tidy_simdata(readRDS("output_data/simulation_results_rodents.RDS"))%>%
  mutate(dataset = "Arizona Rodents")

simdata_frogs <-
  tidy_simdata(readRDS("output_data/simulation_results_treefrogs.RDS"))%>%
  mutate(dataset = "Texas Tadpoles")

simdata_plankton <- 
  tidy_simdata(readRDS("output_data/simulation_results_phyto_subset_scaled.RDS"))%>%
  mutate(dataset = "Switzerland Plankton")


  all_simdata <-
    bind_rows(simdata,simdata_panama,simdata_rats,
            simdata_frogs,simdata_plankton)
  
  all_simdata %>%
    mutate(percent_error = abs(((true_value - estimate)/true_value)*100)) %>%
    group_by(sample_size,dataset,moment,method) %>%
    summarise(avg_pct_error = mean(percent_error))%>%
    filter(sample_size <=25 & sample_size > 1) %>%
    pivot_wider(names_from = method,values_from = avg_pct_error) %>%
    mutate(pct_difference_sscw_vs_cscw = (`Site-Specific CW`-`Cross-Site CW`)/`Cross-Site CW`)%>%
    mutate(pct_difference_pbs_vs_cscw = (`Parametric BS`-`Cross-Site CW`)/`Cross-Site CW`)%>%
    mutate(pct_difference_npbs_vs_cscw = (`Nonparametric BS`-`Cross-Site CW`)/`Cross-Site CW`)%>%
    ungroup()%>%
    group_by(moment)->temp
  
  temp %>%
    group_by(sample_size,moment)%>%
    summarise(mean_pct_improvement_sscw_vs_cw = mean(pct_difference_sscw_vs_cscw)*100,
              mean_pct_improvement_pbs_vs_cw = mean(pct_difference_pbs_vs_cscw)*100,
              mean_pct_improvement_npbs_vs_cw = mean(pct_difference_npbs_vs_cscw)*100,
              )%>%
    mutate(mean_pct_improvement_sscw_vs_cw = round(mean_pct_improvement_sscw_vs_cw,digits = 2),
           mean_pct_improvement_pbs_vs_cw = round(mean_pct_improvement_pbs_vs_cw,digits = 2),
           mean_pct_improvement_npbs_vs_cw = round(mean_pct_improvement_npbs_vs_cw,digits = 2))%>%
    rename(`Sample size` = sample_size,
           `Moment` = moment,
           `Avg. % Imp. Site-Specific CW` = mean_pct_improvement_sscw_vs_cw,
           `Avg. % Imp. Parametric BS` = mean_pct_improvement_pbs_vs_cw,
           `Avg. % Imp. Nonparametric BS` = mean_pct_improvement_npbs_vs_cw
           ) -> pct_imp_table

  library(gt)  
  
  pct_imp_table %>%
    mutate(`Sample size` = paste("Sample Size = ", `Sample size`)) %>%
    gt()%>%
    data_color(
      columns = c("Avg. % Imp. Site-Specific CW"),
      colors = colors[2],
      )%>%
    data_color(
      columns = c("Avg. % Imp. Parametric BS"),
      colors = colors[3],
    )%>%
    data_color(
      columns = c("Avg. % Imp. Nonparametric BS"),
      colors = colors[4],
    )%>%
    gt::tab_options(column_labels.vlines.style = "solid",column_labels.vlines.width = 2)-> pct_imp_gt
    

  pct_imp_gt
  
  gtsave(data = pct_imp_gt,
         filename = "figures/pct_improvement_table.png",vwidth = 1500, vheight = 1000)
  
  gtsave(data = pct_imp_gt,
         filename = "figures/pct_improvement_table.html")
  

  #########################
  
