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

### Tanya tries to be accurate (creative) ----

#To capitalise labels
capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

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
  filter(site == 'Road')

simdata_lollipop =
  simdata %>%
  filter(site == 'Road')

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
             color = "grey50", size = 4) + 
  geom_point(data = simmeans,
             aes(x = estimate, 
                 y = method,
                 color = method), 
             size = 3) +
  facet_grid(cols = vars(moment),
             rows = vars(trait),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             scales = 'free',
             switch = 'y') +
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

ggsave(here::here("figures/Lollipops_All.png"),
       height = 8.3, width = 15,
       units = "in", dpi = 600)


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
#rename traits
traits_parsed <- c(
  biomass_per_ind = "Biomass",
  dry_mass_mg = "Dry mass",
  height = "Height",
  leaf_area_mm2 = "Leaf area",
  LMA_mg_mm2 = "LMA"
)


# Plot
ggplot(simdata_shapes %>%
         mutate(combo = paste(moment, trait))) +
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

ggsave(here::here("figures/CIballoons.png"),
       height = 10, width = 7.6,
       units = "in", dpi = 600)

### Moon plots - accuracy of moments - 'global' ----

library(gggibbous)

sim_moon =   
  simdata %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         deviation = abs((estimate - true_value)/true_value)) %>%
  group_by(trait, method, moment, sample_size) %>%
  #calcualte proportion of 'hits' per trait, methos, moment
  summarise(percentage = sum(hit - 1)/sum(hit),
            deviation = mean(abs((estimate - true_value)/true_value))) 

ggplot(sim_moon %>%
         filter(trait == 'leaf_area_mm2')) + 
  geom_hline(aes(yintercept = 0), 
             color = "grey50",
             size = 1.5) +
  geom_linerange(aes(x = sample_size, 
                     ymin = 0, 
                     ymax = deviation), 
                 color = "grey50", 
                 size = 0.3) +
  geom_point(
    aes(
      x = sample_size,
      y = deviation,
      color = method
    ), 
    size = 5) +
  geom_moon(
    aes(
      x = sample_size,
      y = deviation,
      ratio = percentage, 
      #right = right, 
      fill = method
    ),
    color = "transparent",
    size = 5
  ) + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = colorspace::lighten(pal_df$c, amount = 0.5),
                      labels = pal_df$l) +
  facet_grid(cols = vars(moment),
             rows = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y') +
  # Theme
  figure_theme +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white",
                                   colour = NA),
    panel.background = element_rect(fill = "white",
                                    colour = NA),
    strip.text.x = element_text(margin = margin(0, 0, 10, 0),
                                size = 14, face = "bold"),
    strip.text.y.left = element_blank(),
    legend.key = element_blank()
  )

ggsave(here::here("figures/moons_LeafArea.png"),
       height = 8.3, width = 15,
       units = "in", dpi = 600)

#TODO: expand y axis and work on padding around text 
ggplot(sim_moon %>%
         filter(trait == 'leaf_area_mm2')) + 
  geom_hline(aes(yintercept = 0), 
             color = "grey50",
             size = 1.5) +
  geom_smooth(
    aes(
    x = sample_size,
    y = deviation,
    color = method),
    se = FALSE,
    alpha = 0.5,
    linetype = 2,
    size = 0.8) +
  geom_linerange(data = sim_moon %>%
                   filter(trait == 'leaf_area_mm2') %>%
                   slice(which(row_number() %% 5 == 1)),
                 aes(x = sample_size, 
                     ymin = 0, 
                     ymax = deviation), 
                 color = "grey50", 
                 size = 0.3) +
  geom_point(data = sim_moon %>%
               filter(trait == 'leaf_area_mm2') %>%
               slice(which(row_number() %% 5 == 1)),
    aes(
      x = sample_size,
      y = deviation,
      color = method
    ), 
    size = 9) +
  geom_moon(data = sim_moon %>%
              filter(trait == 'leaf_area_mm2') %>%
              slice(which(row_number() %% 5 == 1)),
    aes(
      x = sample_size,
      y = deviation,
      ratio = percentage, 
      #right = right, 
      fill = method
    ),
    color = "transparent",
    size = 9) + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = colorspace::lighten(pal_df$c, amount = 0.5),
                      labels = pal_df$l) +
  geom_text(
    data = sim_moon %>%
      filter(trait == 'leaf_area_mm2') %>%
      slice(which(row_number() %% 5 == 1)),
    aes(
      x = sample_size,
      y = deviation + 0.6,
      label = glue::glue("{round(percentage*100, 0.1)}%")
      #color = region,
    ),
    size = 3.5,
    hjust = 0,
    nudge_x = 8,
    colour = "grey65"
  ) +
  facet_grid(cols = vars(moment),
             rows = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y') +
  # Theme
  figure_theme +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white",
                                   colour = NA),
    panel.background = element_rect(fill = "white",
                                    colour = NA),
    strip.text.x = element_text(margin = margin(0, 0, 10, 0),
                                size = 14, face = "bold"),
    strip.text.y.left = element_blank(),
    legend.key = element_blank()
  )

ggsave(here::here("figures/moons_LeafArea_subset.png"),
       height = 8.3, width = 15.3,
       units = "in", dpi = 600)
