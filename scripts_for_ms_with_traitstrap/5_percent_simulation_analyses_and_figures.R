#analyze and plot simulation results


##############################################

library(tidyverse)
source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")

#read in data
colorado_percent <- readRDS("output_data/Colorado_percent_community_sims.RDS")

panama_percent <- readRDS("output_data/Panama_percent_community_sims.RDS")

colorado_percent$method <- ordered(colorado_percent$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametric BS","Non-Parametric BS"))
panama_percent$method <- ordered(panama_percent$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametric BS","Non-Parametric BS"))

### Moon plots - accuracy of moments - 'global' ----

library(gggibbous)

#### All traits combined

overunders = 
  colorado_percent %>%
  filter(estimate != is.na(estimate)) %>%
  mutate(pct_abd_sampled = round(pct_abd_sampled, digits = -1),
         overunder = ifelse(true_value <= estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate))) %>%
  group_by(moment, method, pct_abd_sampled, overunder) %>%
  summarise(dev = mean(deviation, na.rm = TRUE),
            tally = n()) %>%
  group_by(moment, method, pct_abd_sampled) %>%
  filter(tally == max(tally)) %>%
  group_by(moment, method) %>%
  mutate(x = dev/max(dev)) %>%
  mutate(x = ifelse(overunder == "under",
                    -1*x,
                    x))

sim_moon_means_colorado =
  colorado_percent %>%
  filter(estimate != is.na(estimate)) %>%
  #if true value falls in estimate's CI
  filter(estimate != is.na(estimate)) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         pct_abd_sampled = round(pct_abd_sampled, digits = -1)) %>%
  group_by(method, moment, pct_abd_sampled) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            group_size = n(),
            deviation = mean(ifelse(abs(estimate) > abs(true_value),
                                    abs(estimate) - abs(true_value),
                                    abs(true_value) - abs(estimate)),
                             na.rm = TRUE))

sim_moon_means_panama =
  panama_percent %>%
  #if true value falls in estimate's CI
  filter(estimate != is.na(estimate)) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         pct_abd_sampled = round(pct_abd_sampled, digits = -1)) %>%
  group_by(method, moment, pct_abd_sampled) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(ifelse(estimate > true_value,
                                    estimate - true_value,
                                    true_value - estimate),
                             na.rm = TRUE))


sim_moon_means_panama$moment =
  ordered(sim_moon_means_panama$moment,levels = c("mean","variance","skewness","kurtosis"))

sim_moon_means_colorado$moment =
  ordered(sim_moon_means_colorado$moment,levels = c("mean",
                                                    "variance",
                                                    "skewness",
                                                    "kurtosis"))

colorado_percent$moment =
  ordered(colorado_percent$moment,levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))

overunders$moment =
  ordered(overunders$moment,levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))

moons <-
  ggplot(sim_moon_means_colorado) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_hline(data = sim_moon_means %>%
               filter(sample_size == 9),
             aes(yintercept = deviation,
                 linetype = "Optimal"),
             color = "grey50",
             size = 0.5) +
  geom_smooth(
    data = colorado_percent %>%
      #if true value falls in estimate's CI
      filter(estimate != is.na(estimate)) %>%
      mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                          2,
                          1)) %>%
      group_by(method, moment, pct_abd_sampled) %>%
      #calcualte proportion of 'hits' per trait, methods, moment
      summarise(deviation = mean(ifelse(abs(estimate) > abs(true_value),
                                        abs(estimate) - abs(true_value),
                                        abs(true_value) - abs(estimate)),
                                 na.rm = TRUE)),
    aes(
      x = pct_abd_sampled,
      y = deviation ,
      color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  # ggblur::geom_point_blur(
  #   aes(
  #     x = pct_abd_sampled,
  #     y = deviation,
  #     color = method
  #   ),
  #   color = "transparent",
  #   size = 3) +
  geom_point(aes(
    x = pct_abd_sampled,
    y = deviation,
    color = method
  ),
  size = 3,
  alpha = 0.9) +
  geom_moon(aes(
    x = pct_abd_sampled,
    y = deviation,
    ratio = percentage,
    #right = right,
    fill = method
  ),
  color = "transparent",
  size = 3) +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +  
  scale_linetype_manual("Sampling",
                        values=c("Optimal" = 4),
                        guide = guide_legend(override.aes = list(colour = "grey69"))) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Percent abundance sampled",
       y = "Average deviation from true moment") +
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

inset =
  ggplot(overunders) +
  geom_col(aes(y = x,
               x = as.factor(pct_abd_sampled),
               fill = method,
               group = method),
           position = 'dodge',
           alpha = 0.5,
           show.legend = FALSE) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y')  + 
  geom_segment(aes(y = 0,
                   xend = 7.5,
                   x = 0.5, yend = 0),
               colour = '#4e5368',
               size = 0.5) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(y = c(-1.3,10)) + 
  expand_limits(x= c(-1, 22)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )

cowplot::ggdraw(moons) +
  cowplot::draw_plot(moon_legend,
                     .8, .12,
                     0.21, .22) +
  cowplot::draw_plot(inset,
                     width = 0.8,
                     height = 0.9,
                     x = 0.05,
                     y = 0.06)

ggsave(here::here("figures/moons_pct_abund.png"),
       height = 7.4, width = 12.5,
       units = "in", dpi = 300)

### Moon plots - rodents - 'global' ----

#read in data

rodent_percent <- readRDS("output_data/Rodent_percent_community_sims.RDS")

rodent_percent$method <- ordered(rodent_percent$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametric BS","Non-Parametric BS"))

overunders = 
  rodent_percent %>%
  filter(estimate != is.na(estimate)) %>%
  mutate(pct_abd_sampled = round(pct_abd_sampled, digits = -1),
         overunder = ifelse(true_value <= estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate))) %>%
  group_by(moment, method, pct_abd_sampled, overunder) %>%
  summarise(dev = mean(deviation, na.rm = TRUE),
            tally = n()) %>%
  group_by(moment, method, pct_abd_sampled) %>%
  filter(tally == max(tally)) %>%
  group_by(moment, method) %>%
  mutate(x = dev/max(dev)) %>%
  mutate(x = ifelse(overunder == "under",
                    -1*x,
                    x))

sim_moon_means_rodent =
  rodent_percent %>%
  filter(estimate != is.na(estimate)) %>%
  #if true value falls in estimate's CI
  filter(estimate != is.na(estimate)) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         pct_abd_sampled = round(pct_abd_sampled, digits = -1)) %>%
  group_by(method, moment, pct_abd_sampled) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            group_size = n(),
            deviation = mean(ifelse(abs(estimate) > abs(true_value),
                                    abs(estimate) - abs(true_value),
                                    abs(true_value) - abs(estimate)),
                             na.rm = TRUE))

sim_moon_means_rodent$moment =
  ordered(sim_moon_means_rodent$moment,levels = c("mean",
                                                    "variance",
                                                    "skewness",
                                                    "kurtosis"))

rodent_percent$moment =
  ordered(rodent_percent$moment,levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))

overunders$moment =
  ordered(overunders$moment,levels = c("mean",
                                       "variance",
                                       "skewness",
                                       "kurtosis"))

moons <-
  ggplot(sim_moon_means_rodent) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  # geom_hline(data = sim_moon_means %>%
  #              filter(sample_size == 9),
  #            aes(yintercept = deviation,
  #                linetype = "Optimal"),
  #            color = "grey50",
  #            size = 0.5) +
  geom_smooth(
    data = rodent_percent %>%
      #if true value falls in estimate's CI
      filter(estimate != is.na(estimate)) %>%
      mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                          2,
                          1)) %>%
      group_by(method, moment, pct_abd_sampled) %>%
      #calcualte proportion of 'hits' per trait, methods, moment
      summarise(deviation = mean(ifelse(abs(estimate) > abs(true_value),
                                        abs(estimate) - abs(true_value),
                                        abs(true_value) - abs(estimate)),
                                 na.rm = TRUE)),
    aes(
      x = pct_abd_sampled,
      y = deviation ,
      color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  # ggblur::geom_point_blur(
  #   aes(
  #     x = pct_abd_sampled,
  #     y = deviation,
  #     color = method
  #   ),
  #   color = "transparent",
  #   size = 3) +
  geom_point(aes(
    x = pct_abd_sampled,
    y = deviation,
    color = method
  ),
  size = 3,
  alpha = 0.9) +
  geom_moon(aes(
    x = pct_abd_sampled,
    y = deviation,
    ratio = percentage,
    #right = right,
    fill = method
  ),
  color = "transparent",
  size = 3) +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +  
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Percent abundance sampled",
       y = "Average deviation from true moment") +
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

inset =
  ggplot(overunders) +
  geom_col(aes(y = x,
               x = as.factor(pct_abd_sampled),
               fill = method,
               group = method),
           position = 'dodge',
           alpha = 0.5,
           show.legend = FALSE) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y')  + 
  geom_segment(aes(y = 0,
                   xend = 7.5,
                   x = 0.5, yend = 0),
               colour = '#4e5368',
               size = 0.5) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(y = c(-1.3,10)) + 
  expand_limits(x= c(-1, 22)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )

cowplot::ggdraw(moons) +
  cowplot::draw_plot(moon_legend,
                     .8, .17,
                     0.21, .22) +
  cowplot::draw_plot(inset,
                     width = 0.8,
                     height = 0.9,
                     x = 0.05,
                     y = 0.06)

ggsave(here::here("figures/moons_pct_abund_rodents.png"),
       height = 7.4, width = 12.5,
       units = "in", dpi = 300)



### Transition plots - accuracy & directionality of moments - 'global' ----

library(gggibbous)

#### All traits combined

sim_means =
  colorado_percent %>%
  #if true value falls in estimate's CI
  filter(estimate != is.na(estimate)) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         pct_abd_sampled = round(pct_abd_sampled, digits = -1)) %>%
  group_by(method, moment, pct_abd_sampled) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            group_size = n(),
            deviation = mean(ifelse(estimate > true_value,
                                    estimate - true_value,
                                    true_value - estimate),
                             na.rm = TRUE))

sim_moon_means_panama =
  panama_percent %>%
  #if true value falls in estimate's CI
  filter(estimate != is.na(estimate)) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         pct_abd_sampled = round(pct_abd_sampled, digits = -1)) %>%
  group_by(method, moment, pct_abd_sampled) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(ifelse(estimate > true_value,
                                    estimate - true_value,
                                    true_value - estimate),
                             na.rm = TRUE))


sim_moon_means_panama$moment =
  ordered(sim_moon_means_panama$moment,levels = c("mean","variance","skewness","kurtosis"))

sim_moon_means_colorado$moment =
  ordered(sim_moon_means_colorado$moment,levels = c("mean",
                                                    "variance",
                                                    "skewness",
                                                    "kurtosis"))

colorado_percent$moment =
  ordered(colorado_percent$moment,levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))

moons <-
  ggplot(sim_moon_means_colorado) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_hline(data = sim_moon_means %>%
               filter(sample_size == 9),
             aes(yintercept = deviation,
                 linetype = "Optimal"),
             color = "grey50",
             size = 0.5) +
  geom_smooth(
    data = colorado_percent %>%
      #if true value falls in estimate's CI
      filter(estimate != is.na(estimate)) %>%
      mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                          2,
                          1)) %>%
      group_by(method, moment, pct_abd_sampled) %>%
      #calcualte proportion of 'hits' per trait, methods, moment
      summarise(deviation = mean(ifelse(estimate > true_value,
                                        estimate - true_value,
                                        true_value - estimate),
                                 na.rm = TRUE)),
    aes(
      x = pct_abd_sampled,
      y = deviation ,
      color = method,
      linetype = "Colorado"),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  ggblur::geom_point_blur(
    aes(
      x = pct_abd_sampled,
      y = deviation,
      color = method
    ),
    color = "transparent",
    size = 3) +
  geom_point(aes(
    x = pct_abd_sampled,
    y = deviation,
    color = method
  ),
  size = 3,
  alpha = 0.9) +
  geom_moon(aes(
    x = pct_abd_sampled,
    y = deviation,
    ratio = percentage,
    #right = right,
    fill = method
  ),
  color = "transparent",
  size = 3) +
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
                        values=c("Optimal" = 4,
                                 "Colorado" = 1),
                        guide = guide_legend(
                          override.aes = list(colour = "black")
                        )) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Percent abundance sampled",
       y = "Average deviation from true moment") +
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
                     .80, .11,
                     0.2, .23)

ggsave(here::here("figures/moons_pct_abund.png"),
       height = 7.4, width = 12.5,
       units = "in", dpi = 300)



# End of script ----
