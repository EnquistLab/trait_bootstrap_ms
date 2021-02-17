#analyze and plot simulation results


##############################################
source(source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R"))

#read in data
simdata <- readRDS("output_data/simulation_results.RDS")

simdata_biased <- readRDS("output_data/simulation_results_biased.RDS")

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


##############################################################
#Accuracy vs sample size

ggplot(data = simdata[which(simdata$moment=="mean"),], mapping = aes(y=log10(estimate/true_value),x = sample_size,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point(alpha=0.25)+geom_smooth()+scale_x_continuous(trans = "sqrt",breaks = c(0,10,50,100,200,500))+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Mean")

ggplot(data = simdata[which(simdata$moment=="variance"),], mapping = aes(y=log10(estimate/true_value),x = sample_size,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point(alpha=0.25)+geom_smooth()+scale_x_continuous(trans = "sqrt",breaks = c(0,10,50,100,200,500))+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Variance")

ggplot(data = simdata[which(simdata$moment=="skewness"),], mapping = aes(y=log10(estimate/true_value),x = sample_size,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point(alpha=0.25)+geom_smooth()+scale_x_continuous(trans = "sqrt",breaks = c(0,10,50,100,200,500))+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Skewness")

ggplot(data = simdata[which(simdata$moment=="kurtosis"),], mapping = aes(y=log10(abs(estimate/true_value)),x = sample_size,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point(alpha=0.25)+geom_smooth()+scale_x_continuous(trans = "sqrt",breaks = c(0,10,50,100,200,500))+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Kurtosis")

### Accuracy vs sample size - Tanya's version ----

simdata %>%
  mutate(accuracy = abs(log10(abs(estimate/true_value)))) %>%
  group_by(moment, method, trait, sample_size) %>%
  slice_sample(n = 6) %>%
  ggplot(aes(y = accuracy,
             x = sample_size,
             color = method)) +
  geom_hline(yintercept = 0,
             color = "grey50",
             size = 1.5) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(data = simdata %>%
                mutate(accuracy = abs(log10(abs(estimate/true_value)))) %>%
                group_by(moment, method, trait, sample_size),
              aes(y = accuracy,
                  x = sample_size,
                  color = method),
              se = FALSE) +
  scale_x_continuous(trans = "sqrt",
                     breaks = c(0,10,50,100,200,500)) +
  facet_grid(cols = vars(moment),
             rows = vars(trait),
             switch = "y",
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             )) +
  scale_colour_manual(guide = guide_legend(title = "Method"),
                      values = pal_df$c,
                      labels = pal_df$l) +
  labs(x = "Sample Size",
       y = "Accuracy") +
  figure_theme

ggsave(here::here("figures/AccuracyLines.png"),
       height = 8.3, width = 15,
       units = "in", dpi = 600)

### Tanya tries to be accurate (creative) ----

simmeans = 
  simdata %>%
  group_by(trait, moment, site) %>%
  mutate(true_val = mean(true_value)) %>%
  group_by(trait, moment, method, site, true_val) %>%
  summarise(estimate = mean(estimate)) %>%
  filter(trait == 'leaf_area_mm2' &
           site == 'Road')

simdata_lollipop =
  simdata %>%
  filter(trait == 'leaf_area_mm2' &
           site == 'Road')

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


ggplot(simmeans) + 
  geom_vline(aes(xintercept = true_val), 
             color = "grey50",
             size = 1.5) +
  geom_jitter(data = simdata_lollipop,
              aes(x = estimate, 
                  y = method, 
                  fill = method,
                  size = sample_size), 
              color = "grey85", 
              width = 0, height = 0.2, alpha = 0.3, shape = 21) +
  geom_segment(data = simmeans,
               aes(x = true_val, 
                   xend = estimate, 
                   y = method, 
                   yend = method), 
               color = "grey50", 
               size = 1) +
  geom_point(data = simmeans,
             aes(x = estimate, 
                 y = method),
             color = "grey50", size = 6) + 
  geom_point(data = simmeans,
             aes(x = estimate, 
                 y = method,
                 color = method), 
             size = 5) +
  facet_wrap(vars(moment),
             scales = "free_x",
             labeller = labeller(.default = capitalize)) +
  scale_fill_manual(guide = guide_legend(title = "Method"),
                    values = pal_df$c,
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method"),
                      values = pal_df$c,
                      labels = pal_df$l) +
  scale_size(guide = guide_legend(title = "Sample Size")) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white",
                                   colour = NA),
    panel.background = element_rect(fill = "white",
                                    colour = NA),
    strip.text.x = element_text(margin = margin(0, 0, 10, 0),
                                size = 16, face = "bold"),
    strip.text.y.left = element_text(colour = "grey65",
                                     margin = margin(0, 10, 10, 10),
                                     angle = 0,
                                     size = 16)
  )

ggsave(here::here("figures/Lollipops_LeafArea.png"),
       height = 8.3, width = 15,
       units = "in", dpi = 600)

#All traits

simmeans = 
  simdata %>%
  group_by(trait, moment, site) %>%
  mutate(true_val = mean(true_value)) %>%
  group_by(trait, moment, method, site, true_val) %>%
  summarise(estimate = mean(estimate)) %>%
  filter(site == 'Road') %>%
  mutate(facet_lab = paste0(moment,"_",trait))

simdata_lollipop =
  simdata %>%
  filter(site == 'Road') %>%
  mutate(facet_lab = paste0(moment,"_",trait)) %>%
  group_by(trait, moment, method, site, sample_size) %>%
  slice_sample(n = 20)

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
                  fill = method,
                  size = sample_size), 
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
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method"),
                      values = pal_df$c,
                      labels = pal_df$l) +
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
        panel.background = element_rect(fill = "grey18",
                                        colour = NA),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(here::here("figures/Lollipops_All.png"),
       height = 8, width = 9,
       units = "in", dpi = 300)


############################################################

### Balloon plots - accuracy of moments - 'global' ----

library(ggforce)
library(prismatic)

f = 0.7 #shape of baloons

#create balloons
simdata_shapes =
  simdata %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(trait, method, moment) %>%
  #calcualte proportion of 'hits' per trait, methos, moment
  summarise(percentage = sum(hit - 1)/sum(hit)) %>%
  #filter(trait == 'height') %>%
  group_by(moment, trait) %>%
  mutate(category_nr = row_number()) %>%
  ungroup()%>%
  rowwise() %>%
  mutate(
    # Calculate points on circle for the "balloons", we need 4 x-y pairs for geom_bspline_closed
    # pi / length(pal_df$l) - creates the flower design by subdividing cricle
    x = list(c(0,
               f * percentage * sin(category_nr * 2 * pi / length(pal_df$l) - pi/4),
               percentage * sin(category_nr * 2 * pi / length(pal_df$l)), # real percentage for main "radius"
               f * percentage * sin(category_nr * 2 * pi / length(pal_df$l) + pi/5),
               0
    )),
    y = list(c(0,
               f * percentage * cos(category_nr * 2 * pi / length(pal_df$l) - pi/5),
               percentage * cos(category_nr * 2 * pi / length(pal_df$l)), # real percentage for main "radius"
               f * percentage * cos(category_nr * 2 * pi / 4 + pi/length(pal_df$l)),
               0
    ))
  ) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(moment, trait), names_from = category_nr, values_from = c(x, y)) %>%
  unnest(x_1:y_4)

#re-order to match moment 'numbers'
simdata_shapes$moment <- factor(simdata_shapes$moment,
                                levels = c("mean",
                                           "variance",
                                           "skewness",
                                           "kurtosis"))


# Plot - with reference ellipse
ggplot(simdata_shapes %>%
         mutate(combo = paste(moment, trait))) +
  geom_ellipse(aes(x0 = 0,y0 = 0,
                   a = 0.5, 
                   b = 0.5, angle = 0),
               colour = colorspace::lighten(unname(colors)[5], 0.8),
               linetype = 4) +
  geom_point(aes(0, 0), size = 0.01, colour = "grey30")  +  # Make a "center"
  # Plot a "balloon" for every category
  geom_bspline_closed(aes(x_1, y_1, group = combo, fill = pal_df$c[1]), alpha = 0.7) +
  geom_bspline_closed(aes(x_2, y_2, group = combo, fill = pal_df$c[2]), alpha = 0.7) +
  geom_bspline_closed(aes(x_3, y_3, group = combo, fill = pal_df$c[3]), alpha = 0.7) +
  geom_bspline_closed(aes(x_4, y_4, group = combo, fill = pal_df$c[4]), alpha = 0.7) +
  scale_fill_identity(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           override.aes = list(alpha = 0.7, shape = 2, size = 8),
                                           title.position="top",
                                           title.hjust = 0.5),
                      breaks = pal_df$c,
                      labels = pal_df$l) +
  coord_fixed(ratio = 1) +
  facet_grid(col = vars(moment),
             row = vars(trait),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y') +
  # Theme
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white",
                                   colour = NA),
    panel.background = element_rect(fill = "white",
                                    colour = NA),
    strip.text.x = element_text(margin = margin(0, 0, 10, 0),
                                size = 16, face = "bold"),
    strip.text.y.left = element_text(colour = "grey65",
                                     margin = margin(0, 10, 10, 10),
                                     angle = 0,
                                     size = 16)
  )

ggsave(here::here("figures/CIballoons_refEllipse.png"),
       height = 10, width = 7.6,
       units = "in", dpi = 600)

### Moon plots - accuracy of moments - 'global' ----

library(gggibbous)

#### All traits combined

sim_moon_means =   
  simdata %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         deviation = abs((estimate - true_value)/true_value)) %>%
  group_by(method, moment, sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/sum(hit),
            deviation = mean(abs((estimate - true_value)/true_value))) 

sim_biased_moon_means =   
  simdata_biased %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         deviation = abs((estimate - true_value)/true_value)) %>%
  group_by(method, moment, sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/sum(hit),
            deviation = mean(abs((estimate - true_value)/true_value))) 

sim_biased_moon_means$moment = 
  ordered(sim_biased_moon_means$moment,levels = c("mean","variance","skewness","kurtosis"))

ggplot(sim_biased_moon_means %>%
         filter(sample_size %in% c(1,9,49,100,196,441))) + 
  geom_hline(aes(yintercept = 0), 
             color = "grey50",
             size = 1.5) +
  geom_smooth(aes(
    x = sample_size,
    y = sim_moon_means %>%
      filter(sample_size %in% c(1,9,49,100,196,441)) %>%
      pull(deviation),
    color = method),
    linetype = 2,
    se = FALSE,
    size = 0.4) +
  geom_smooth(aes(
    x = sample_size,
    y = deviation ,
    color = method),
    alpha = 0.5,
    linetype = 1,
    se = FALSE,
    size = 0.8) +
  geom_linerange(aes(
    x = sample_size,
    ymax = deviation + 0.2,
    ymin = 0), 
    color = "grey50", 
    size = 0.3) +
  geom_point(aes(
    x = sample_size,
    y = deviation + 0.2,
    color = method
  ), 
  size = 5,
  alpha = 0.9) +
  geom_moon(aes(
    x = sample_size,
    y = deviation + 0.2,
    ratio = percentage, 
    #right = right, 
    fill = method
  ),
  color = "transparent",
  size = 5) + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="left",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="left",
                                           title.hjust = 0.5),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +
  geom_text(aes(
    x = sample_size,
    y = deviation + 0.8,
    label = glue::glue("{round(percentage*100, 0.1)}%")
    #color = region,
  ),
  size = 3.3,
  hjust = 0,
  #nudge_x = 8,
  colour = "grey69"
  ) +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                     limits = c(0, 500)) +
  scale_y_continuous(expand = c(0, 0.3)) +
  facet_grid(cols = vars(moment),
             rows = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y') +
  labs(x = "Sample Size",
       y = "Deviation from true mean") +
  # Theme
  figure_theme +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white",
                                   colour = NA),
    panel.background = element_rect(fill = "grey18",
                                    colour = NA),
    strip.text.x = element_text(margin = margin(0, 0, 10, 0),
                                size = 14, face = "bold"),
    strip.text.y.left = element_blank(),
    panel.grid.major.y = element_line(size = 0.05),
    legend.key = element_blank()
  )

ggsave(here::here("figures/moons_biased_AllTraits.png"),
       height = 6.5, width = 12.5,
       units = "in", dpi = 300)
