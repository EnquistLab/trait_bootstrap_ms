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

#Rename methods for plotting
unique(simdata$method)
simdata$method[which(simdata$method=="global cwm")] <- "Cross-Site CWM"
simdata$method[which(simdata$method=="site-specic CWM")] <- "Site-Specific CWM"
simdata$method[which(simdata$method=="nonparametric bs")] <- "Non-Parametric BS"
simdata$method[which(simdata$method=="parametric bs")] <- "Parametric BS"

simdata$method <- ordered(simdata$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametric BS","Non-Parametric BS"))

library(ggplot2)


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


### Accuracy (creative) ----

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
    plot.background = element_rect(fill = "#141438",
                                   colour = NA),
    legend.background = element_rect(fill = "#141438",
                                     colour = NA),
    panel.background = element_rect(fill = "#141438",
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
  #facet_grid(rows = vars(trait),
  #           cols = vars(moment),
  #                      labeller = labeller(
  #                        .default = capitalize,
  #                        trait = traits_parsed,
  #                        .multi_line = FALSE
  #                      ),
  #           scales = 'free',
  #           switch = 'y') +
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

ggsave(here::here("figures/Lollipops_All.png"),
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
  summarise(percentage = sum(hit - 1)/sum(hit),
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
  summarise(percentage = sum(hit - 1)/sum(hit),
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
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white",
                                   colour = NA),
    panel.background = element_rect(fill = "grey18",
                                    colour = NA),
    strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                size = 14, face = "bold"),
    strip.text.x.top = element_blank(),
    panel.grid.major.y = element_line(size = 0.05),
    legend.key = element_blank()
  )

cowplot::ggdraw(moons) +
  cowplot::draw_plot(ggplot(data.frame(y = c(1,1.5,2,2.5), 
                                       x = 0, ratio = 1:4 * 0.25), 
                            aes(x = x, y = y)) +
                       geom_moon(aes(ratio = ratio), size = 5, fill = "grey69", colour = "grey69") +
                       geom_text(aes(x = x + 0.6, 
                                     label = paste0(ratio*100,"%")),
                                 size = 2.5) +
                       coord_fixed() +
                       ggtitle("Uuum") +
                       lims(y = c(0.5, 2.7), x = c(-1, 1.4)) +
                       theme_void() +
                       theme(plot.title = element_text(hjust = 0.5)),
                     .79, .12, 
                     0.2, .23)

ggsave(here::here("figures/moons_biased_AllTraits.png"),
       height = 7, width = 12.5,
       units = "in", dpi = 300)

### Balloon plots - winners ----

f = 0.7 #shape of baloons





#create balloons
simdata_wins_trait =
  simdata %>%
  filter(sample_size < 25) %>%
  #get the absolute difference
  mutate(diff = ifelse(estimate > true_value,
                       estimate - true_value,
                       true_value - estimate)) %>%
  #group by these levels - basically as small 
  #a group as possible but keeping methods together
  group_by(moment, sample_size, site, trait) %>%
  #only keep the smallest value for each group (i.e. the closest)
  filter(diff == min(diff)) %>%
  group_by(trait, method, moment) %>%
  #count the number of times a mthod wins per trait and moment
  count() %>%
  group_by(trait, moment) %>%
  #convert to a percent
  mutate(percentage = n/sum(n)*100) %>%
  select(-n) %>%
  #This just makes sure we have zero values for methods that never win
  right_join(.,
             tibble(method = rep(rep(c("Cross-Site CWM","Site-Specific CWM","Parametric BS", "Non-Parametric BS"),
                                     4),5),
                    moment = rep(rep(c("mean", "variance", "skewness", "kurtosis"),
                                     each = 4),5),
                    trait = rep(c("biomass_per_ind", "dry_mass_mg", "height", "leaf_area_mm2", "LMA_mg_mm2"),
                                each = 16))) %>%
  mutate(percentage = ifelse(is.na(percentage),
                             0,
                             percentage)) %>%
  #manually set the categories becuase grr
  mutate(category_nr = case_when(method == "Cross-Site CWM" ~1,
                                 method == "Site-Specific CWM" ~2,
                                 method == "Parametric BS" ~3,
                                 method == "Non-Parametric BS" ~4)) %>%
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
  pivot_wider(id_cols = c(moment,trait), names_from = category_nr, values_from = c(x, y)) %>%
  unnest(x_1:y_4)

sim_win_text =
  simdata %>%
  mutate(diff = ifelse(estimate > true_value,
                       estimate - true_value,
                       true_value - estimate)) %>%
  group_by(moment, sample_size, site) %>%
  filter(diff == min(diff)) %>%
  group_by(method, moment) %>%
  count() %>%
  group_by(moment) %>%
  mutate(percentage = n/sum(n)*100) %>%
  filter(percentage == max(percentage))

#re-order to match moment 'numbers'
simdata_wins_trait$moment <- factor(simdata_wins_trait$moment,
                                    levels = c("mean",
                                               "variance",
                                               "skewness",
                                               "kurtosis"))

sim_win_text$moment <- factor(sim_win_text$moment,
                              levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))


# Plot
ggplot(simdata_wins_trait %>%
         mutate(combo = paste(moment, trait)))  +
  geom_point(aes(0, 0), size = 0.01, colour = "grey30")  +  # Make a "center"
  # Plot a "balloon" for every category
  geom_bspline_closed(aes(x_1, y_1, group = combo, fill = pal_df$c[1]), alpha = 0.7, colour = 'grey69', size = 0.5) +
  geom_bspline_closed(aes(x_2, y_2, group = combo, fill = pal_df$c[2]), alpha = 0.7, colour = 'grey69', size = 0.5) +
  geom_bspline_closed(aes(x_3, y_3, group = combo, fill = pal_df$c[3]), alpha = 0.7, colour = 'grey69', size = 0.5) +
  geom_bspline_closed(aes(x_4, y_4, group = combo, fill = pal_df$c[4]), alpha = 0.7, colour = 'grey69', size = 0.5) +
  scale_fill_identity(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           override.aes = list(alpha = 0.7, shape = 2, size = 8),
                                           title.position="top",
                                           title.hjust = 0.5),
                      breaks = pal_df$c,
                      labels = pal_df$l) +
  coord_fixed(ratio = 1) +
  #geom_text(data = sim_win_text,
  #          aes(x = -75,
  #              y = 50,
  #              label = glue::glue("{method} - {round(percentage, 0.1)}%")),
  #          colour = 'grey69',
  #          hjust = 0) +
  facet_grid(col = vars(moment),
             row = vars(trait),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y') +
  guides(colour = 'none') +
  # Theme
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white",
                                   colour = NA),
    panel.background = element_rect(fill = "#362e38",
                                    colour = NA),
    strip.text.x = element_text(margin = margin(0, 0, 10, 0),
                                size = 16, face = "bold"),
    strip.text.y.left = element_text(colour = "grey65",
                                     margin = margin(0, 10, 10, 10),
                                     angle = 0,
                                     size = 16)
  )

ggsave(here::here("figures/Winnerballoons_All.png"),
       height = 10, width = 10.6,
       units = "in", dpi = 300)


#create balloons
simdata_wins =
  simdata %>%
  mutate(diff = ifelse(estimate > true_value,
                       estimate - true_value,
                       true_value - estimate)) %>%
  group_by(moment, sample_size, site) %>%
  filter(diff == min(diff)) %>%
  group_by(method, moment) %>%
  count() %>%
  group_by(moment) %>%
  mutate(percentage = n/sum(n)*100) %>%
  select(-n) %>%
  right_join(.,
             tibble(method = rep(c("Cross-Site CWM","Site-Specific CWM","Parametric BS", "Non-Parametric BS"),
                                 4),
                    moment = rep(c("mean", "variance", "skewness", "kurtosis"),
                                 each = 4))) %>%
  mutate(percentage = ifelse(is.na(percentage),
                             0,
                             percentage)) %>%
  group_by(moment) %>%
  mutate(category_nr = row_number()) %>%
  arrange(moment) %>%
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
  pivot_wider(id_cols = c(moment), names_from = category_nr, values_from = c(x, y)) %>%
  unnest(x_1:y_4)

sim_win_text =
  simdata %>%
  mutate(diff = ifelse(estimate > true_value,
                       estimate - true_value,
                       true_value - estimate)) %>%
  group_by(moment, sample_size, site) %>%
  filter(diff == min(diff)) %>%
  group_by(method, moment) %>%
  count() %>%
  group_by(moment) %>%
  mutate(percentage = n/sum(n)*100) %>%
  filter(percentage == max(percentage))

#re-order to match moment 'numbers'
simdata_wins$moment <- factor(simdata_wins$moment,
                              levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))

sim_win_text$moment <- factor(sim_win_text$moment,
                              levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))


# Plot
ggplot(simdata_wins)  +
  geom_point(aes(0, 0), size = 0.01, colour = "grey30")  +  # Make a "center"
  # Plot a "balloon" for every category
  geom_bspline_closed(aes(x_1, y_1, group = moment, fill = pal_df$c[1]), alpha = 0.7, colour = 'grey69', size = 0.5) +
  geom_bspline_closed(aes(x_2, y_2, group = moment, fill = pal_df$c[2]), alpha = 0.7, colour = 'grey69', size = 0.5) +
  geom_bspline_closed(aes(x_3, y_3, group = moment, fill = pal_df$c[3]), alpha = 0.7, colour = 'grey69', size = 0.5) +
  geom_bspline_closed(aes(x_4, y_4, group = moment, fill = pal_df$c[4]), alpha = 0.7, colour = 'grey69', size = 0.5) +
  scale_fill_identity(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           override.aes = list(alpha = 0.7, shape = 2, size = 8),
                                           title.position="top",
                                           title.hjust = 0.5),
                      breaks = pal_df$c,
                      labels = pal_df$l) +
  coord_fixed(ratio = 1) +
  geom_text(data = sim_win_text,
            aes(x = -75,
                y = 50,
                label = glue::glue("{method} - {round(percentage, 0.1)}%")),
            colour = 'grey69',
            hjust = 0) +
  facet_wrap(vars(moment),
             #row = vars(trait),
             ncol = 2,
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             strip.position = 'top') +
  guides(colour = 'none') +
  # Theme
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white",
                                   colour = NA),
    panel.background = element_rect(fill = "grey18",
                                    colour = NA),
    strip.text.x = element_text(margin = margin(0, 0, 10, 0),
                                size = 16, face = "bold"),
    strip.background = element_rect(colour = 'black',
                                    size = 1)
  )

ggsave(here::here("figures/Winnerballoons.png"),
       height = 10, width = 10,
       units = "in", dpi = 300)

### Doughnut plots - winners ----

library(ggtext)

sim_radar = 
  simdata %>%
  mutate(diff = ifelse(estimate > true_value,
                       estimate - true_value,
                       true_value - estimate)) %>%
  group_by(moment, sample_size, site, trait) %>%
  filter(diff == min(diff)) %>%
  group_by(method, moment, trait) %>%
  count() %>%
  group_by(moment, trait) %>%
  mutate(percentage = n/sum(n)) %>%
  select(-n) %>%
  right_join(.,
             tibble(method = rep(rep(c("Cross-Site CWM","Site-Specific CWM","Parametric BS", "Non-Parametric BS"),
                                     4),5),
                    moment = rep(rep(c("mean", "variance", "skewness", "kurtosis"),
                                     each = 4),5),
                    trait = rep(c("biomass_per_ind", "dry_mass_mg", "height", "leaf_area_mm2", "LMA_mg_mm2"),
                                each = 16))) %>%
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
  colour = 'white') +
  xlim(c(0, 3)) +
  #annotation textboxes
  geom_text(data = sim_win_text,
            aes(x = 3,
                y = 0.5,
                label = glue::glue("{method} - {percentage}%")),
            colour = 'grey90',
            hjust = 0.5,
            size = 3) +
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
                    labels = pal_df$l) +
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

sim_doughnuts_all = 
  rbind(simdata %>%
          mutate(dataset = rep("Colorado", nrow(.))),
        simdata_biased %>%
          mutate(dataset = rep("Biased", nrow(.))),
        simdata_panama %>%
          mutate(dataset = rep("Panama", nrow(.))),
        simdata_rats %>%
          mutate(dataset = rep("Rodents", nrow(.)))) %>%
  mutate(diff = ifelse(estimate > true_value,
                       estimate - true_value,
                       true_value - estimate)) %>%
  group_by(moment, sample_size, site, dataset) %>%
  filter(diff == min(diff)) %>%
  group_by(method, moment, dataset) %>%
  count() %>%
  group_by(moment, dataset) %>%
  mutate(percentage = n/sum(n)) %>%
  select(-n)

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
                                              "Biased",
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
                                               "Biased",
                                               "Panama", 
                                               "Rodents"))


ggplot(sim_doughnuts_all) +
  geom_col(aes(
    x = 2,
    y = percentage,
    fill = method
  ),
  colour = 'grey69') +
  xlim(c(0, 3)) +
  #annotation textboxes
  geom_text(data = sim_win_text,
            aes(x = 3,
                y = 0.5,
                label = glue::glue("{method} - {percentage}%")),
            colour = 'grey90',
            hjust = 0.5,
            size = 3) +
  coord_polar(theta = 'y') +
  facet_grid(rows = vars(dataset),
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
                    labels = pal_df$l) +
  # Theme
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14,
                                colour = "grey65"),
    plot.background = element_rect(fill = "grey18",
                                   colour = NA),
    panel.background = element_rect(fill = "grey18",
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


ggsave(here::here("figures/WinnerDoughnuts_datasets.png"),
       height = 10, width = 10.4,
       units = "in", dpi = 300)
