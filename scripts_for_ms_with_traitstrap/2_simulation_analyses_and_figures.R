#analyze and plot simulation results

source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

#### Read Data ####


#read in data
simdata =
  tidy_simdata(readRDS("output_data/simulation_results.RDS"))

simdata_biased = 
  tidy_simdata(readRDS("output_data/simulation_results_biased.RDS"))

simdata_panama <- 
  tidy_simdata(readRDS("output_data/panama_simulation_results.RDS"))

simdata_rats <- 
  tidy_simdata(readRDS("output_data/simulation_results_rodents.RDS"))

simdata_frogs <- 
  tidy_simdata(readRDS("output_data/simulation_results_treefrogs.RDS"))


### Accuracy (creative) ----

#All traits

simmeans = 
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(trait, moment, site) %>%
  mutate(true_val = mean(true_value)) %>%
  group_by(trait, moment, method, site, true_val) %>%
  summarise(estimate = mean(estimate)) %>%
  filter(site == 'Road') %>%
  mutate(facet_lab = paste0(moment,"_",trait))

simdata_lollipop =
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  filter(site == 'Road') %>%
  mutate(facet_lab = paste0(moment,"_",trait)) #%>%
# group_by(trait, moment, method, site, sample_size) %>%
# slice_sample(n = 20)

#re-order to match moment 'numbers'
simmeans$moment <- factor(simmeans$moment,
                          levels = c("mean",
                                     "variance",
                                     "skewness",
                                     "kurtosis"))


simdata_lollipop$moment <- factor(simdata_lollipop$moment,
                                  levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))


#TODO clean labelling

ggplot(simmeans) + 
  geom_vline(aes(xintercept = true_val), 
             color = "grey50",
             size = 1) +
  geom_jitter(data = simdata_lollipop,
              aes(x = estimate, 
                  y = method, 
                  fill = method), 
              color = "grey85", 
              width = 0, height = 0.2, alpha = 0.3, shape = 21) +
  geom_segment(data = simmeans,
               aes(x = true_val, 
                   xend = estimate, 
                   y = method, 
                   yend = method), 
               color = "grey50", 
               size = 0.5) +
  geom_point(data = simmeans,
             aes(x = estimate, 
                 y = method),
             color = "grey50", size = 3) + 
  geom_point(data = simmeans,
             aes(x = estimate, 
                 y = method,
                 color = method), 
             size = 2) +
  facet_wrap(~trait + moment,
             #rows = vars(trait),
             labeller = labeller(
               .default = capitalize,
               trait = traits_parsed,
               .multi_line = FALSE
             ),
             ncol = 4,
             scales = "free_x",
             strip.position = 'top') +
  scale_fill_manual(guide = guide_legend(title = "Method"),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method"),
                      values = pal_df$c,
                      breaks = pal_df$l) +
  scale_size(guide = guide_legend(title = "Sample Size"),
             range = c(0.5, 2)) +
  labs(
    x = "Estimated Value",
    y = NULL
  ) +
  guides(size = 'none') +
  figure_theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "#141438",
                                       colour = NA),
        legend.background = element_rect(fill = "#141438",
                                         colour = NA),
        panel.background = element_rect(fill = "#141438",
                                        colour = "grey69"),
        strip.text.y = element_blank(),
        strip.text.x.top = element_blank(),
        panel.grid.major.y = element_line(size = 0.05,
                                          colour = "grey65"),
        legend.key = element_blank(),
        legend.text = element_text(colour = "grey65"),
        axis.title = element_text(colour = "grey65"),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.title = element_text(colour = "grey65"),
        legend.position = 'bottom',
        plot.margin=unit(c(3,0.3,0,5), "cm"))

ggsave(here::here("figures/Lollipops_All.png"),
       height = 9, width = 13,
       units = "in", dpi = 300)



### Accuracy Across datasets (creative) ----

#All traits

simmeans = 
  rbind(simdata %>%
          mutate(dataset = rep("Colorado", nrow(.))),
        simdata_frogs %>%
          mutate(dataset = rep("Frogs", nrow(.))#,
                 # method = ifelse(method == "Site-Specific CWM",
                 #                 "Cross-Site CWM",
                 #                 as.character(method))
          ),
        simdata_panama %>%
          mutate(dataset = rep("Panama", nrow(.))),
        simdata_rats %>%
          mutate(dataset = rep("Rodents", nrow(.)))) %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  filter(site %in% c("PBM", "south_campus", "BCIBALA TRES", "1")) %>%
  filter(trait %in% c("dry_mass_mg", "body_length_mm", "LMA", "log10_weight")) %>%
  group_by(dataset, moment) %>%
  mutate(true_val = mean(true_value)) %>%
  group_by(dataset, moment, method, true_val) %>%
  summarise(estimate = mean(estimate)) %>%
  mutate(facet_lab = paste0(moment,"_",dataset))

simdata_lollipop =
  rbind(simdata %>%
          mutate(dataset = rep("Colorado", nrow(.))),
        simdata_frogs %>%
          mutate(dataset = rep("Frogs", nrow(.))#,
                 # method = ifelse(method == "Site-Specific CWM",
                 #                 "Cross-Site CWM",
                 #                 as.character(method))
          ),
        simdata_panama %>%
          mutate(dataset = rep("Panama", nrow(.))),
        simdata_rats %>%
          mutate(dataset = rep("Rodents", nrow(.)))) %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  filter(site %in% c("PBM", "south_campus", "BCIBALA TRES", "1")) %>%
  filter(trait %in% c("dry_mass_mg", "body_length_mm", "LMA", "log10_weight")) %>%
  mutate(facet_lab = paste0(moment,"_",dataset)) #%>%
# group_by(dataset, moment, method, sample_size) %>%
# slice_sample(n = 20)

simdata_lollipop %>%
  distinct(dataset, trait) %>%
  group_by(dataset) %>%
  slice_sample(n = 1)

#re-order to match moment 'numbers'
simmeans$moment <- factor(simmeans$moment,
                          levels = c("mean",
                                     "variance",
                                     "skewness",
                                     "kurtosis"))

simdata_lollipop$moment <- factor(simdata_lollipop$moment,
                                  levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))

#TODO clean labelling

ggplot(simmeans) + 
  geom_vline(aes(xintercept = true_val), 
             color = "grey50",
             size = 1) +
  geom_jitter(data = simdata_lollipop,
              aes(x = estimate, 
                  y = method, 
                  fill = method), 
              color = "grey85", 
              width = 0, height = 0.2, alpha = 0.3, shape = 21) +
  geom_segment(data = simmeans,
               aes(x = true_val, 
                   xend = estimate, 
                   y = method, 
                   yend = method), 
               color = "grey50", 
               size = 0.5) +
  geom_point(data = simmeans,
             aes(x = estimate, 
                 y = method),
             color = "grey50", size = 3) + 
  geom_point(data = simmeans,
             aes(x = estimate, 
                 y = method,
                 color = method), 
             size = 2) +
  facet_wrap(~dataset + moment,
             #rows = vars(trait),
             # labeller = labeller(
             #   .default = capitalize,
             #   trait = traits_parsed,
             #   .multi_line = FALSE
             # ),
             ncol = 4,
             scales = "free_x",
             strip.position = 'top') +
  scale_fill_manual(guide = guide_legend(title = "Method"),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method"),
                      values = pal_df$c,
                      breaks = pal_df$l) +
  scale_size(guide = guide_legend(title = "Sample Size"),
             range = c(0.5, 2)) +
  labs(
    x = "Estimated Value",
    y = NULL
  ) +
  guides(size = 'none') +
  figure_theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.background = element_rect(fill = "#141438",
                                       colour = NA),
        legend.background = element_rect(fill = "#141438",
                                         colour = NA),
        panel.background = element_rect(fill = "#141438",
                                        colour = NA),
        strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                    size = 14, face = "bold",
                                    colour = "grey65"),
        strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                        size = 12, face = "bold",
                                        colour = "grey65"),
        panel.grid.major.y = element_line(size = 0.05,
                                          colour = "grey65"),
        legend.key = element_blank(),
        legend.text = element_text(colour = "grey65"),
        axis.title = element_text(colour = "grey65"),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.title = element_text(colour = "grey65"),
        legend.position = 'bottom')

ggsave(here::here("figures/Lollipops_Datsets.png"),
       height = 8, width = 11,
       units = "in", dpi = 300)



############################################################

source("r_functions/sumarize_sims.R")
sim_summary <- summarize_simulations(simulation_result = simdata)
sim_summary$moment <- ordered(sim_summary$moment,levels=c("mean","variance","skewness","kurtosis"))

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


#####################

ggplot(data = sim_summary[which(sim_summary$site=="Almont"),],
       mapping = aes(y=fractional_difference,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = moment~trait,scales = "free")


### Moon plots - accuracy of moments - 'global' ----

library(gggibbous)

#### All traits combined

sim_moon_means =   
  simdata %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(method, moment, sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(ifelse(estimate > true_value,
                                    estimate - true_value,
                                    true_value - estimate))) 

sim_biased_moon_means =   
  simdata_biased %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(method, moment, sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(ifelse(estimate > true_value,
                                    estimate - true_value,
                                    true_value - estimate))) 

sim_biased_moon_means$moment = 
  ordered(sim_biased_moon_means$moment,levels = c("mean","variance","skewness","kurtosis"))

sim_moon_means$moment = 
  ordered(sim_moon_means$moment,levels = c("mean",
                                           "variance",
                                           "skewness",
                                           "kurtosis"))

moons <-
  ggplot(sim_moon_means %>%
           filter(sample_size %in% c(1,9,49,100,196,441))) + 
  geom_hline(aes(yintercept = 0), 
             color = "grey50",
             size = 1.5) +
  geom_smooth(aes(
    x = sample_size,
    y = sim_biased_moon_means %>%
      filter(sample_size %in% c(1,9,49,100,196,441)) %>%
      pull(deviation),
    color = method,
    linetype = "Biased"),
    se = FALSE,
    size = 0.4) +
  geom_smooth(aes(
    x = sample_size,
    y = deviation ,
    color = method,
    linetype = "Random"),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  ggblur::geom_point_blur(
    aes(
      x = sample_size,
      y = deviation,
      color = method
    ),
    color = "transparent",
    size = 4) +
  
  geom_point(aes(
    x = sample_size,
    y = deviation,
    color = method
  ), 
  size = 4,
  alpha = 0.9) +
  geom_moon(aes(
    x = sample_size,
    y = deviation,
    ratio = percentage, 
    fill = method
  ),
  color = "transparent",
  size = 4) + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.5),
                      labels = pal_df$l) +  
  scale_linetype_manual("Sampling",
                        values=c("Biased" = 2,
                                 "Random" = 1),
                        guide = guide_legend(override.aes = list(colour = "grey69"))) +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                     limits = c(0, 500)) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Sample Size",
       y = "Average deviation from true moment") +
  figure_theme +
  theme(
    legend.position = 'right',
    legend.title = element_text(size = 14, colour = "grey65"),
    strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                size = 14, face = "bold",
                                colour = "grey65"),
    strip.placement.y = "outside",
    strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                    size = 14, face = "bold",
                                    colour = "grey65"),
    panel.grid.major.y = element_line(size = 0.05,
                                      colour = "grey65"),
    legend.key = element_blank(),
    legend.text = element_text(colour = "grey65"),
    axis.title = element_text(colour = "grey65"),
    strip.background = element_blank(),
    axis.line = element_blank(),
    strip.placement = 'outside'
  )

cowplot::ggdraw(moons) +
  cowplot::draw_plot(moon_legend,
                     .795, .12, 
                     0.2, .23)

ggsave(here::here("figures/moons_biased_AllTraits.png"),
       height = 8, width = 12.5,
       units = "in", dpi = 300)

### Doughnut plots - winners ----

library(ggtext)

simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(moment, sample_size, site, trait) %>%
  count()


sim_radar = 
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  mutate(diff = ifelse(estimate >= true_value,
                       estimate - true_value,
                       true_value - estimate),
         hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(trait, moment, sample_size, site) %>%
  filter(hit == 2) %>%
  filter(diff == min(diff)) %>%
  group_by(trait, method, moment) %>%
  count() %>%
  group_by(moment, trait) %>%
  mutate(percentage = n/40) %>%
  select(-n) %>%
  mutate(percentage = ifelse(is.na(percentage),
                             0,
                             percentage))

sim_win_text =
  sim_radar %>%
  group_by(moment, trait) %>%
  filter(percentage == max(percentage)) %>%
  mutate(percentage = round(percentage*100))

sim_radar$moment <- factor(sim_radar$moment,
                           levels = c("mean",
                                      "variance",
                                      "skewness",
                                      "kurtosis"))

sim_radar$method <- factor(sim_radar$method,
                           levels = c("Cross-Site CWM",
                                      "Site-Specific CWM",
                                      "Parametric BS", 
                                      "Non-Parametric BS"))

sim_win_text$moment <- factor(sim_win_text$moment,
                              levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))

sim_win_text$method <- factor(sim_win_text$method,
                              levels = c("Cross-Site CWM",
                                         "Site-Specific CWM",
                                         "Parametric BS", 
                                         "Non-Parametric BS"))


ggplot(sim_radar) +
  geom_col(aes(
    x = 2,
    y = percentage,
    fill = method
  ),
  colour = 'grey96') +
  xlim(c(0.7, 2.5)) +
  ylim(c(0, 1))  +
  #annotation textboxes
  geom_text(data = sim_win_text,
            aes(x = 1.1,
                y = 0.25,
                colour = method,
                label = glue::glue("{percentage}%")),
            #label = glue::glue("{method} - {percentage}%")),
            #colour = 'grey90',
            hjust = 1,
            show.legend = FALSE,
            size = 4) +
  coord_polar(theta = 'y') +
  facet_grid(rows = vars(trait),
             cols = vars(moment),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y')  + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = pal_df$c,
                      breaks = pal_df$l) +
  # Theme
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14,
                                colour = "grey65"),
    plot.background = element_rect(fill = "#141438",
                                   colour = NA),
    panel.background = element_rect(fill = "#141438",
                                    colour = NA),
    strip.text.x = element_text(margin = margin(0, 0, 10, 0),
                                size = 16, face = "bold",
                                colour = "grey70"),
    strip.text.y.left = element_text(colour = "grey69",
                                     margin = margin(0, 10, 10, 10),
                                     angle = 0,
                                     size = 16),
    legend.text = element_text(colour = "grey65")
  )


ggsave(here::here("figures/WinnerDoughnuts.png"),
       height = 10, width = 10.4,
       units = "in", dpi = 300)


### Doughnut plots - across winners ----

simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(moment, sample_size, site, trait) %>%
  count()

group_size = rbind(simdata %>%
                     mutate(dataset = rep("Colorado", nrow(.))),
                   simdata_frogs %>%
                     mutate(dataset = rep("Frogs", nrow(.))#,
                            # method = ifelse(method == "Site-Specific CWM",
                            #                 "Cross-Site CWM",
                            #                 as.character(method))
                     ),
                   simdata_panama %>%
                     mutate(dataset = rep("Panama", nrow(.))),
                   simdata_rats %>%
                     mutate(dataset = rep("Rodents", nrow(.)))) %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  distinct(dataset, moment, sample_size, site, trait) %>%
  group_by(dataset, moment) %>%
  count() %>%
  rename(grp_mn = n)

sim_doughnuts_all = 
  rbind(simdata %>%
          mutate(dataset = rep("Colorado", nrow(.))),
        simdata_frogs %>%
          mutate(dataset = rep("Frogs", nrow(.))#,
                 # method = ifelse(method == "Site-Specific CWM",
                 #                 "Cross-Site CWM",
                 #                 as.character(method))
          ),
        simdata_panama %>%
          mutate(dataset = rep("Panama", nrow(.))),
        simdata_rats %>%
          mutate(dataset = rep("Rodents", nrow(.)))) %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  mutate(diff = ifelse(estimate >= true_value,
                       estimate - true_value,
                       true_value - estimate),
         hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1))%>%
  group_by(dataset, moment, sample_size, site, trait) %>%
  filter(hit == 2) %>%
  filter(diff == min(diff)) %>%
  group_by(dataset, method, moment) %>%
  count()  %>%
  left_join(.,
            group_size) %>%
  mutate(percentage = n/grp_mn)

sim_win_text =
  sim_doughnuts_all %>%
  group_by(moment, dataset) %>%
  filter(percentage == max(percentage)) %>%
  mutate(percentage = round(percentage*100))

sim_doughnuts_all$moment <- factor(sim_doughnuts_all$moment,
                                   levels = c("mean",
                                              "variance",
                                              "skewness",
                                              "kurtosis"))

sim_doughnuts_all$method <- factor(sim_doughnuts_all$method,
                                   levels = c("Cross-Site CWM",
                                              "Site-Specific CWM",
                                              "Parametric BS", 
                                              "Non-Parametric BS"))

sim_doughnuts_all$dataset <- factor(sim_doughnuts_all$dataset,
                                    levels = c("Colorado",
                                               "Frogs",
                                               "Panama", 
                                               "Rodents"))

sim_win_text$moment <- factor(sim_win_text$moment,
                              levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))

sim_win_text$method <- factor(sim_win_text$method,
                              levels = c("Cross-Site CWM",
                                         "Site-Specific CWM",
                                         "Parametric BS", 
                                         "Non-Parametric BS"))

sim_win_text$dataset <- factor(sim_win_text$dataset,
                               levels = c("Colorado",
                                          "Frogs",
                                          "Panama", 
                                          "Rodents"))

doughnut = 
  ggplot(sim_doughnuts_all) +
  geom_col(aes(
    x = 2,
    y = percentage,
    fill = method
  ),
  colour = 'grey96') +
  xlim(c(0.7, 2.5)) +
  ylim(c(0, 1)) +
  #annotation textboxes
  geom_text(data = sim_win_text,
            aes(x = 1,
                y = 0.26,
                colour = method,
                label = glue::glue("{percentage}%")),
            #label = glue::glue("{method} - {percentage}%")),
            #colour = 'grey90',
            hjust = 1,
            show.legend = FALSE,
            size = 4) +
  coord_polar(theta = 'y') +
  facet_grid(rows = vars(dataset),
             cols = vars(moment),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y')  + 
  scale_colour_manual(values = pal_df$c,
                      breaks = pal_df$l)  + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  # Theme
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14,
                                colour = "grey65"),
    plot.background = element_rect(fill = "#141438",
                                   colour = NA),
    panel.background = element_rect(fill = "#141438",
                                    colour = NA),
    strip.text.x = element_text(margin = margin(0, 0, 10, 0),
                                size = 16, face = "bold",
                                colour = "grey70"),
    strip.text.y.left = element_text(colour = "grey69",
                                     margin = margin(0, 10, 10, 10),
                                     angle = 0,
                                     size = 12,
                                     vjust = 0),
    legend.text = element_text(colour = "grey65")
  )

img1 = png::readPNG("images/Colorado.png")
img2 = png::readPNG("images/Coral.png")
img3 = png::readPNG("images/Panama.png")
img4 = png::readPNG("images/AZ.png")

cowplot::ggdraw(doughnut) +
  cowplot::draw_image(
    img1, x = 0.08, y = 0.89, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.06
  ) +
  cowplot::draw_image(
    img2, x = 0.08, y = 0.64, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.07
  ) +
  cowplot::draw_image(
    img3, x = 0.08, y = 0.41, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.06
  ) +
  cowplot::draw_image(
    img4, x = 0.08, y = 0.145, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.07
  )

ggsave(here::here("figures/WinnerDoughnuts_datasets_images.png"),
       height = 8.3, width = 10.4,
       units = "in", dpi = 300)
