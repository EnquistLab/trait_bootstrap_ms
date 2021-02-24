#analyze and plot simulation results


##############################################
source(source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R"))

#read in data
simdata <- readRDS("output_data/simulation_results.RDS")

simdata_biased <- readRDS("output_data/simulation_results_biased.RDS")

simdata_panama <- readRDS("output_data/panama_simulation_results.RDS")

simdata_rats <- readRDS("output_data/simulation_results_rodents.RDS")

#need to further tidy up data (add a column for moment)

#could probably do this with pipes, but I'll go oldschool and cut down the googling

library(tidyverse)

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

#Rename methods for plotting
unique(simdata$method)
simdata$method[which(simdata$method=="global cwm")] <- "Cross-Site CWM"
simdata$method[which(simdata$method=="site-specic CWM")] <- "Site-Specific CWM"
simdata$method[which(simdata$method=="nonparametric bs")] <- "Non-Parametric BS"
simdata$method[which(simdata$method=="parametric bs")] <- "Parametric BS"

simdata$method <- ordered(simdata$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametric BS","Non-Parametric BS"))

###Clenaing Biased sim data - the tidy way (for now...)

simdata_biased =
  simdata_biased %>%
  pivot_longer(cols = c('mean', 'var', 'skew', 'kurt'),
               names_to = 'moment',
               values_to = 'estimate') %>%
  pivot_longer(cols = contains('ci_high'),
               names_to = 'ci_high_moment',
               values_to = 'ci_high') %>%
  mutate(ci_high_moment = str_to_lower(str_extract(ci_high_moment,'[[:alpha:]]*$'))) %>%
  filter(ci_high_moment == moment) %>%
  pivot_longer(cols = contains('ci_low'),
               names_to = 'ci_low_moment',
               values_to = 'ci_low') %>%
  mutate(ci_low_moment = str_to_lower(str_extract(ci_low_moment,'[[:alpha:]]*$'))) %>%
  filter(ci_low_moment == moment) %>%
  pivot_longer(cols = contains('true'),
               names_to = 'true_moment',
               values_to = 'true_value') %>%
  mutate(true_moment = str_to_lower(str_extract(true_moment,'[[:alpha:]]*$')),
         moment = case_when(moment == 'var' ~ 'variance',
                            moment == 'kurt' ~ 'kurtosis',
                            moment == 'skew' ~ 'skewness',
                            TRUE ~ moment),
         method = case_when(method == 'global cwm' ~ 'Cross-Site CWM',
                            method == 'site-specic CWM' ~ 'Site-Specific CWM',
                            method == 'nonparametric bs' ~ 'Non-Parametric BS',
                            method == 'parametric bs' ~ 'Parametric BS',
                            TRUE ~ method)) %>%
  filter(true_moment == moment) %>%
  select(-c(global, true_moment, ci_low_moment, ci_high_moment))

simdata_biased$method <- ordered(simdata_biased$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametric BS","Non-Parametric BS"))


###Cleaning Panama sim data - the tidy way (for now...)

simdata_panama =
  simdata_panama %>%
  pivot_longer(cols = c('mean', 'var', 'skew', 'kurt'),
               names_to = 'moment',
               values_to = 'estimate') %>%
  pivot_longer(cols = contains('ci_high'),
               names_to = 'ci_high_moment',
               values_to = 'ci_high') %>%
  mutate(ci_high_moment = str_to_lower(str_extract(ci_high_moment,'[[:alpha:]]*$'))) %>%
  filter(ci_high_moment == moment) %>%
  pivot_longer(cols = contains('ci_low'),
               names_to = 'ci_low_moment',
               values_to = 'ci_low') %>%
  mutate(ci_low_moment = str_to_lower(str_extract(ci_low_moment,'[[:alpha:]]*$'))) %>%
  filter(ci_low_moment == moment) %>%
  pivot_longer(cols = contains('true'),
               names_to = 'true_moment',
               values_to = 'true_value') %>%
  mutate(true_moment = str_to_lower(str_extract(true_moment,'[[:alpha:]]*$')),
         moment = case_when(moment == 'var' ~ 'variance',
                            moment == 'kurt' ~ 'kurtosis',
                            moment == 'skew' ~ 'skewness',
                            TRUE ~ moment),
         method = case_when(method == 'global cwm' ~ 'Cross-Site CWM',
                            method == 'site-specic CWM' ~ 'Site-Specific CWM',
                            method == 'nonparametric bs' ~ 'Non-Parametric BS',
                            method == 'parametric bs' ~ 'Parametric BS',
                            TRUE ~ method)) %>%
  filter(true_moment == moment) %>%
  select(-c(global, true_moment, ci_low_moment, ci_high_moment))

simdata_panama$method <- ordered(simdata_panama$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametric BS","Non-Parametric BS"))


###Cleaning Panama sim data - the tidy way (for now...)

simdata_rats =
  simdata_rats %>%
  pivot_longer(cols = c('mean', 'var', 'skew', 'kurt'),
               names_to = 'moment',
               values_to = 'estimate') %>%
  pivot_longer(cols = contains('ci_high'),
               names_to = 'ci_high_moment',
               values_to = 'ci_high') %>%
  mutate(ci_high_moment = str_to_lower(str_extract(ci_high_moment,'[[:alpha:]]*$'))) %>%
  filter(ci_high_moment == moment) %>%
  pivot_longer(cols = contains('ci_low'),
               names_to = 'ci_low_moment',
               values_to = 'ci_low') %>%
  mutate(ci_low_moment = str_to_lower(str_extract(ci_low_moment,'[[:alpha:]]*$'))) %>%
  filter(ci_low_moment == moment) %>%
  pivot_longer(cols = contains('true'),
               names_to = 'true_moment',
               values_to = 'true_value') %>%
  mutate(true_moment = str_to_lower(str_extract(true_moment,'[[:alpha:]]*$')),
         moment = case_when(moment == 'var' ~ 'variance',
                            moment == 'kurt' ~ 'kurtosis',
                            moment == 'skew' ~ 'skewness',
                            TRUE ~ moment),
         method = case_when(method == 'global cwm' ~ 'Cross-Site CWM',
                            method == 'site-specic CWM' ~ 'Site-Specific CWM',
                            method == 'nonparametric bs' ~ 'Non-Parametric BS',
                            method == 'parametric bs' ~ 'Parametric BS',
                            TRUE ~ method)) %>%
  filter(true_moment == moment) %>%
  select(-c(global, true_moment, ci_low_moment, ci_high_moment))

simdata_rats$method <- ordered(simdata_rats$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametric BS","Non-Parametric BS"))


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
  #geom_linerange(aes(
  #  x = sample_size,
  #  ymax = deviation,
  #  ymin = 0),
  #  color = "grey50",
  #  size = 0.3) +
  geom_point(aes(
    x = sample_size,
    y = deviation,
    color = method
  ),
  size = 5,
  alpha = 0.9) +
  geom_moon(aes(
    x = sample_size,
    y = deviation,
    ratio = percentage,
    #right = right,
    fill = method
  ),
  color = "transparent",
  size = 5) +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top"),
                    values = pal_df$c,
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +
  scale_linetype_manual("Sampling",
                        values=c("Biased" = 2,
                                 "Random" = 1),
                        guide = guide_legend(override.aes = list(colour = "black"))) +
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
  #draw_key_moon(data.frame(x = 1:5, y = 0, ratio = 0:4 * 0.25))
  # Theme
  figure_theme +
  theme(
    legend.position = 'right',
    legend.title = element_text(size = 14, colour = "grey65"),
    strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                size = 14, face = "bold",
                                colour = "grey65"),
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
                     .79, .12,
                     0.2, .23)


### Bonus Plots----

#### All traits combined

sim_moon_panama =
  simdata_panama %>%
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

sim_moon_rats =
  simdata_rats %>%
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
unique(sim_moon_rats$sample_size)
sim_moon_panama$moment =
  ordered(sim_moon_panama$moment,levels = c("mean","variance","skewness","kurtosis"))

sim_moon_panama$moment =
  ordered(sim_moon_panama$moment,levels = c("mean",
                                            "variance",
                                            "skewness",
                                            "kurtosis"))

sim_moon_rats$moment =
  ordered(sim_moon_rats$moment,levels = c("mean","variance","skewness","kurtosis"))

sim_moon_rats$moment =
  ordered(sim_moon_rats$moment,levels = c("mean",
                                          "variance",
                                          "skewness",
                                          "kurtosis"))

unique(sim_moon_panama$sample_size)

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
    y = sim_moon_panama %>%
      filter(sample_size %in% c(1,9,49,100,196,256)) %>%
      pull(deviation),
    color = method,
    linetype = "Panama"),
    se = FALSE,
    size = 0.4) +
  geom_smooth(data = sim_moon_rats,
              aes(
                x = sample_size,
                y = deviation,
                color = method,
                linetype = "Rodents"),
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
  size = 5,
  alpha = 0.9,
  blur_size = 2) +
  geom_moon(aes(
    x = sample_size,
    y = deviation,
    ratio = percentage,
    #right = right,
    fill = method
  ),
  color = "transparent",
  size = 5) +
  geom_point(aes(
    x = sample_size,
    y = deviation,
    color = method
  ),
  size = 5,
  alpha = 0.9) +
  geom_moon(aes(
    x = sample_size,
    y = deviation,
    ratio = percentage,
    #right = right,
    fill = method
  ),
  color = "transparent",
  size = 5) +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.5),
                      labels = pal_df$l) +
  scale_linetype_manual("Sampling",
                        values=c("Biased" = 2,
                                 "Random" = 1,
                                 "Panama" = 3,
                                 "Rodents" = 4),
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
  #draw_key_moon(data.frame(x = 1:5, y = 0, ratio = 0:4 * 0.25))
  # Theme
  figure_theme +
  theme(
    legend.position = 'right',
    legend.title = element_text(size = 14, colour = "grey65"),
    strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                size = 14, face = "bold",
                                colour = "grey65"),
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
  cowplot::draw_plot(ggplot(data.frame(y = c(1,1.5,2,2.5),
                                       x = 0, ratio = 1:4 * 0.25),
                            aes(x = x, y = y)) +
                       geom_moon(aes(ratio = ratio), size = 5, fill = "grey69", colour = "grey69") +
                       geom_text(aes(x = x + 0.6,
                                     label = paste0(ratio*100,"%")),
                                 size = 2.7,
                                 colour = "grey65") +
                       coord_fixed() +
                       ggtitle("Uuum") +
                       lims(y = c(0.5, 2.7), x = c(-1, 1.4)) +
                       theme_void() +
                       theme(plot.title = element_text(hjust = 0.5, colour = "grey65")),
                     .80, .075,
                     0.2, .23)

