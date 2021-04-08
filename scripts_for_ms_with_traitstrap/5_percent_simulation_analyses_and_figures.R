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
  summarise(dev = mean(abs(deviation), na.rm = TRUE),
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

colorado_random =
  simdata %>%
  filter(sample_size == 9) %>%
  group_by(moment, method) %>%
  summarise(deviation = mean(abs(deviation)))


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

colorado_random$moment =
  ordered(colorado_random$moment,levels = c("mean",
                                       "variance",
                                       "skewness",
                                       "kurtosis"))

moons_co <-
  ggplot(sim_moon_means_colorado) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_smooth(
    data = colorado_percent %>%
      filter(estimate != is.na(estimate)) %>%
      mutate(deviation = ifelse(abs(estimate) > abs(true_value),
                                abs(estimate) - abs(true_value),
                                abs(true_value) - abs(estimate))),
    aes(
      x = pct_abd_sampled,
      y = deviation ,
      color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  geom_point(aes(
    x = pct_abd_sampled,
    y = deviation,
    color = method
  ),
  size = 4,
  alpha = 0.9) +
  geom_moon(aes(
    x = pct_abd_sampled,
    y = deviation,
    ratio = percentage,
    fill = method
  ),
  color = "transparent",
  size = 4) +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.25),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
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
  labs(x = "Percent cumulative abundance sampled",
       y = "Average deviation from true moment") +
  # Theme
  theme_moon

inset_co =
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

cowplot::ggdraw(moons_co) +
  cowplot::draw_plot(moon_legend,
                     .79, .17,
                     0.21, .22) +
  cowplot::draw_plot(inset_co,
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

rodent_random$moment =
  ordered(rodent_random$moment,levels = c("mean",
                                       "variance",
                                       "skewness",
                                       "kurtosis"))

moons_az <-
ggplot(sim_moon_means_rodent) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_smooth(
    data = rodent_percent %>%
      filter(estimate != is.na(estimate)) %>%
      mutate(deviation = ifelse(abs(estimate) > abs(true_value),
                                abs(estimate) - abs(true_value),
                                abs(true_value) - abs(estimate))),
    aes(
      x = pct_abd_sampled,
      y = deviation ,
      color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  geom_point(aes(
    x = pct_abd_sampled,
    y = deviation,
    color = method
  ),
  size = 4,
  alpha = 0.9) +
  geom_moon(aes(
    x = pct_abd_sampled,
    y = deviation,
    ratio = percentage,
    #right = right,
    fill = method
  ),
  color = "transparent",
  size = 4) +
  scale_linetype_manual("Sampling",
                        values=c("Biased" = 1,
                                 "Random" = 2),
                        guide = guide_legend(override.aes = list(colour = "grey69"))) +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.25),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
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
  labs(x = "Percent cumulative abundance sampled",
       y = "Average deviation from true moment") +
  # Theme
  theme_moon

inset_az =
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
                   xend = 9.5,
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

cowplot::ggdraw(moons_az) +
  cowplot::draw_plot(moon_legend,
                     .79, .17,
                     0.21, .22) +
  cowplot::draw_plot(inset_az,
                     width = 0.8,
                     height = 0.9,
                     x = 0.05,
                     y = 0.06)

ggsave(here::here("figures/moons_pct_abund_rodents.png"),
       height = 7.4, width = 12.5,
       units = "in", dpi = 300)

#### Combine CO and AZ ----

library(patchwork)

moon_legend2 = 
  ggplot(data.frame(y = c(1,2.7,4.4,6.1), 
                    x = 0, ratio = 1:4 * 0.25),
         aes(x = x, y = y)) +
  geom_moon(aes(ratio = ratio), size = 5, fill = "grey69", colour = "grey69") +
  geom_text(aes(x = x + 3,
                label = paste0(ratio*100,"%")),
            size = 4,
            colour = "grey65",
            family = "Noto") +
  coord_fixed() +
  ggtitle("Value in CI") +
  lims(y = c(0.5, 7), x = c(-1, 8)) +
  theme_void() +
  theme(plot.title = element_markdown(hjust = 0.5,
                                      halign = 0,
                                      size = rel(1.7)),
        plot.title.position = "panel",
        text = element_text(colour = "grey65",
                            family = "Noto"))

(moons_co +
  labs(title = "A: Herbs") + 
  theme(plot.title.position = 'panel',
        plot.title = element_text(size = 16, color = "grey65"),
        plot.margin = margin(40, 5, 10, 25)) +
    inset_element(inset_co,
                  left = 0,
                  bottom = 0,
                  right = 1,
                  top = 1)) +
  (moons_az  +
  labs(title = "B: Rodents") +   
  theme(plot.title.position = 'panel',
        plot.title = element_text(size = 16, color = "grey65"),
        strip.text.y = element_blank(),
        axis.title.y = element_blank()) +
    inset_element(inset_az,
                  left = 0,
                  bottom = 0,
                  right = 1,
                  top = 1)) +
  plot_layout(guides = 'collect',
              widths = c(1, 1)) +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "#141438", colour = NA))) +
  inset_element(moon_legend2, 
                left = 0.785, 
                bottom = 0.12, 
                right = 0.925, 
                top = 0.5,
                align_to = 'full')

ggsave(here::here("figures/moons_pct_abund_AB.png"),
       height = 9.2, width = 20.5,
       units = "in", dpi = 300)

#### Lollipops CO ----

co_pct_means = 
  colorado_percent %>%
  filter(estimate != is.na(estimate)) %>%
  mutate(pct_abd_sampled = round(pct_abd_sampled, digits = -1),
         overunder = ifelse(true_value <= estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate)),
         hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      'Yes',
                      'No')) %>%
  filter(pct_abd_sampled == 80) %>%
  mutate(deviation = ifelse(overunder == "under",
                            -1*deviation,
                            deviation)) %>%
  group_by(trait, moment, method) %>%
  summarise(estimate = mean(deviation),
            iqr = IQR(deviation))

co_simdata_lollipop =
  colorado_percent %>%
  filter(estimate != is.na(estimate)) %>%
  mutate(pct_abd_sampled = round(pct_abd_sampled, digits = -1),
         overunder = ifelse(true_value <= estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate)),
         hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      'Yes',
                      'No')) %>%
  filter(pct_abd_sampled == 80) %>%
  mutate(deviation = ifelse(overunder == "under",
                            -1*deviation,
                            deviation)) %>%
  group_by(trait, moment, method, pct_abd_sampled) %>%
  slice_sample(n = 20)

simmeans = 
  simdata %>%
  filter(sample_size == 9) %>%
  group_by(trait, moment, method) %>%
  summarise(estimate = mean(deviation),
            std_dev = IQR(deviation))

#re-order to match moment 'numbers'
co_pct_means$moment <- factor(co_pct_means$moment,
                          levels = c("mean",
                                     "variance",
                                     "skewness",
                                     "kurtosis"))


co_simdata_lollipop$moment <- factor(co_simdata_lollipop$moment,
                                  levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))

simmeans$moment <- factor(simmeans$moment,
                                     levels = c("mean",
                                                "variance",
                                                "skewness",
                                                "kurtosis"))


#TODO clean labelling

ggplot(co_pct_means) + 
  geom_vline(aes(xintercept = 0), 
             color = "grey50",
             size = 1) +
  geom_pointrange(data = simmeans,
                 aes(x = estimate,
                     xmax = estimate + std_dev,
                     xmin = estimate - std_dev,
                     y = method,
                     fill = method,
                     colour = method),
                 position = position_nudge(y = -0.3)) +
  geom_jitter(data = co_simdata_lollipop,
              aes(x = deviation, 
                  y = method, 
                  fill = method,
                  #size = as.factor(pct_abd_sampled),
                  alpha = hit), 
              color = "grey85", 
              width = 0, height = 0.2, shape = 21) +
  geom_pointrange(data = co_pct_means,
             aes(x = estimate,
                 xmin = estimate - iqr,
                 xmax = estimate + iqr,
                 y = method,
                 fill = method,
                 colour = method),
             shape = 23, size = 1) + 
  facet_grid(rows = vars(trait),
             cols = vars(moment),
             labeller = labeller(
               .default = capitalize,
               trait = traits_parsed
             ),
             switch = 'y',
             scales = 'free')  + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         override.aes = list(shape = 21)),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           override.aes = list(shape = 21)),
                      values = colorspace::darken(pal_df$c, 0.5),
                      breaks = pal_df$l) +
  # scale_size_discrete(guide = guide_legend(title = "Sample Size"),
  #                     range = c(.7, 2.7)) +
  scale_alpha_discrete(guide = guide_legend(title = "Value in CI",
                                            override.aes = list(shape = 16)),
                       range = c(0.2, 0.4)) +
  labs(
    x = "Deviation from true value",
    y = NULL
  ) +
  #guides(size = 'none') +
  figure_theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.background = element_rect(fill = "#141438",
                                       colour = NA),
        legend.background = element_rect(fill = "#141438",
                                         colour = NA),
        panel.background = element_rect(fill = "#141438",
                                        colour = 'grey69'),
        strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                    size = 14, face = "bold",
                                    colour = "grey65"),
        strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                        size = 12, face = "bold",
                                        colour = "grey65"),
        panel.grid.major.y = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(colour = "grey65"),
        axis.title = element_text(colour = "grey65"),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.title = element_text(colour = "grey65"),
        legend.position = 'bottom')




#### Lollipops AZ ----

az_pct_means = 
  rodent_percent %>%
  filter(estimate != is.na(estimate)) %>%
  mutate(pct_abd_sampled = round(pct_abd_sampled, digits = -1),
         overunder = ifelse(true_value <= estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate)),
         hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      'Yes',
                      'No')) %>%
  filter(pct_abd_sampled == 80) %>%
  mutate(deviation = ifelse(overunder == "under",
                            -1*deviation,
                            deviation)) %>%
  group_by(trait, moment, method) %>%
  summarise(estimate = mean(deviation),
            iqr = IQR(deviation))

az_simdata_lollipop =
  rodent_percent %>%
  filter(estimate != is.na(estimate)) %>%
  mutate(pct_abd_sampled = round(pct_abd_sampled, digits = -1),
         overunder = ifelse(true_value <= estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate)),
         hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      'Yes',
                      'No')) %>%
  filter(pct_abd_sampled == 80) %>%
  mutate(deviation = ifelse(overunder == "under",
                            -1*deviation,
                            deviation)) %>%
  group_by(trait, moment, method, pct_abd_sampled) %>%
  slice_sample(n = 20)

az_simmeans = 
  simdata_rats %>%
  filter(sample_size == 9) %>%
  group_by(trait, moment, method) %>%
  summarise(estimate = mean(deviation),
            std_dev = IQR(deviation))

#re-order to match moment 'numbers'
az_pct_means$moment <- factor(az_pct_means$moment,
                              levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))


az_simdata_lollipop$moment <- factor(az_simdata_lollipop$moment,
                                     levels = c("mean",
                                                "variance",
                                                "skewness",
                                                "kurtosis"))

az_simmeans$moment <- factor(az_simmeans$moment,
                          levels = c("mean",
                                     "variance",
                                     "skewness",
                                     "kurtosis"))


ggplot(az_pct_means) + 
  geom_vline(aes(xintercept = 0), 
             color = "grey50",
             size = 1) +
  # geom_segment(data = co_pct_means,
  #              aes(x = 0, 
  #                  xend = estimate, 
  #                  y = method, 
  #                  yend = method), 
  #              color = "grey50", 
  #              size = 0.5) +
  geom_pointrange(data = az_simmeans,
                  aes(x = estimate,
                      xmax = estimate + std_dev,
                      xmin = estimate - std_dev,
                      y = method,
                      fill = method,
                      colour = method),
                  position = position_nudge(y = -0.3)) +
  geom_jitter(data = az_simdata_lollipop,
              aes(x = deviation, 
                  y = method, 
                  fill = method,
                  #size = as.factor(pct_abd_sampled),
                  alpha = hit), 
              color = "grey85", 
              width = 0, height = 0.2, shape = 21) +
  geom_pointrange(data = az_pct_means,
                  aes(x = estimate,
                      xmin = estimate - iqr,
                      xmax = estimate + iqr,
                      y = method,
                      fill = method,
                      colour = method),
                  shape = 23, size = 1) + 
  facet_grid(rows = vars(trait),
             cols = vars(moment),
             labeller = labeller(
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free')  + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         override.aes = list(shape = 21)),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           override.aes = list(shape = 21)),
                      values = colorspace::darken(pal_df$c, 0.5),
                      breaks = pal_df$l) +
  # scale_size_discrete(guide = guide_legend(title = "Sample Size"),
  #                     range = c(.7, 2.7)) +
  scale_alpha_discrete(guide = guide_legend(title = "Value in CI",
                                            override.aes = list(shape = 16)),
                       range = c(0.2, 0.4)) +
  labs(
    x = "Deviation from true value",
    y = NULL
  ) +
  #guides(size = 'none') +
  figure_theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.background = element_rect(fill = "#141438",
                                       colour = NA),
        legend.background = element_rect(fill = "#141438",
                                         colour = NA),
        panel.background = element_rect(fill = "#141438",
                                        colour = 'grey69'),
        strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                    size = 14, face = "bold",
                                    colour = "grey65"),
        strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                        size = 12, face = "bold",
                                        colour = "grey65"),
        panel.grid.major.y = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(colour = "grey65"),
        axis.title = element_text(colour = "grey65"),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.title = element_text(colour = "grey65"),
        legend.position = 'bottom')




# Moon Panama ----

overunders = 
  panama_percent %>%
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

sim_moon_means_panama =
  panama_percent %>%
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

sim_moon_means_panama$moment =
  ordered(sim_moon_means_panama$moment,levels = c("mean",
                                                  "variance",
                                                  "skewness",
                                                  "kurtosis"))

overunders$moment =
  ordered(overunders$moment,levels = c("mean",
                                       "variance",
                                       "skewness",
                                       "kurtosis"))

panama_percent$moment =
  ordered(panama_percent$moment,levels = c("mean",
                                       "variance",
                                       "skewness",
                                       "kurtosis"))

moons_pa <-
  ggplot(sim_moon_means_panama) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_smooth(
    data = sim_moon_means_panama,
    aes(
      x = pct_abd_sampled,
      y = deviation ,
      color = method,
      linetype = "Biased"),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  geom_point(aes(
    x = pct_abd_sampled,
    y = deviation,
    color = method
  ),
  size = 4,
  alpha = 0.9) +
  geom_moon(aes(
    x = pct_abd_sampled,
    y = deviation,
    ratio = percentage,
    #right = right,
    fill = method
  ),
  color = "transparent",
  size = 4) +
  scale_linetype_manual("Sampling",
                        values=c("Biased" = 1,
                                 "Random" = 2),
                        guide = guide_legend(override.aes = list(colour = "grey69"))) +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.25),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
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
  labs(x = "Percent cumulative abundance sampled",
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

inset_pa =
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
                   xend = 9.5,
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

cowplot::ggdraw(moons_pa) +
  cowplot::draw_plot(moon_legend,
                     .79, .15,
                     0.21, .175) +
  cowplot::draw_plot(inset_pa,
                     width = 0.75,
                     height = 0.85,
                     x = 0.07,
                     y = 0.07)

ggsave(here::here("figures/moons_pct_abund_panama.png"),
       height = 7.4, width = 12.5,
       units = "in", dpi = 300)

#### Lollipops panama ----

az_pct_means = 
  panama_percent %>%
  filter(estimate != is.na(estimate)) %>%
  mutate(pct_abd_sampled = round(pct_abd_sampled, digits = -1),
         overunder = ifelse(true_value <= estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate)),
         hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      'Yes',
                      'No')) %>%
  filter(pct_abd_sampled == 80) %>%
  mutate(deviation = ifelse(overunder == "under",
                            -1*deviation,
                            deviation)) %>%
  group_by(trait, moment, method) %>%
  summarise(estimate = mean(deviation),
            iqr = IQR(deviation))

az_simdata_lollipop =
  panama_percent %>%
  filter(estimate != is.na(estimate)) %>%
  mutate(pct_abd_sampled = round(pct_abd_sampled, digits = -1),
         overunder = ifelse(true_value <= estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate)),
         hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      'Yes',
                      'No')) %>%
  filter(pct_abd_sampled == 80) %>%
  mutate(deviation = ifelse(overunder == "under",
                            -1*deviation,
                            deviation)) %>%
  group_by(trait, moment, method, pct_abd_sampled) %>%
  slice_sample(n = 20)

az_simmeans = 
  simdata_panama %>%
  filter(sample_size == 9) %>%
  group_by(trait, moment, method) %>%
  summarise(estimate = mean(deviation),
            std_dev = IQR(deviation))

#re-order to match moment 'numbers'
az_pct_means$moment <- factor(az_pct_means$moment,
                              levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))


az_simdata_lollipop$moment <- factor(az_simdata_lollipop$moment,
                                     levels = c("mean",
                                                "variance",
                                                "skewness",
                                                "kurtosis"))

az_simmeans$moment <- factor(az_simmeans$moment,
                             levels = c("mean",
                                        "variance",
                                        "skewness",
                                        "kurtosis"))


ggplot(az_pct_means) + 
  geom_vline(aes(xintercept = 0), 
             color = "grey50",
             size = 1) +
  geom_pointrange(data = az_simmeans,
                  aes(x = estimate,
                      xmax = estimate + std_dev,
                      xmin = estimate - std_dev,
                      y = method,
                      fill = method,
                      colour = method),
                  position = position_nudge(y = -0.3)) +
  geom_jitter(data = az_simdata_lollipop,
              aes(x = deviation, 
                  y = method, 
                  fill = method,
                  #size = as.factor(pct_abd_sampled),
                  alpha = hit), 
              color = "grey85", 
              width = 0, height = 0.2, shape = 21) +
  geom_pointrange(data = az_pct_means,
                  aes(x = estimate,
                      xmin = estimate - iqr,
                      xmax = estimate + iqr,
                      y = method,
                      fill = method,
                      colour = method),
                  shape = 23, size = 1) + 
  facet_grid(rows = vars(trait),
             cols = vars(moment),
             labeller = labeller(
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free')  + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         override.aes = list(shape = 21)),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           override.aes = list(shape = 21)),
                      values = colorspace::darken(pal_df$c, 0.5),
                      breaks = pal_df$l) +
  # scale_size_discrete(guide = guide_legend(title = "Sample Size"),
  #                     range = c(.7, 2.7)) +
  scale_alpha_discrete(guide = guide_legend(title = "Value in CI",
                                            override.aes = list(shape = 16)),
                       range = c(0.2, 0.4)) +
  labs(
    x = "Deviation from true value",
    y = NULL
  ) +
  #guides(size = 'none') +
  figure_theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.background = element_rect(fill = "#141438",
                                       colour = NA),
        legend.background = element_rect(fill = "#141438",
                                         colour = NA),
        panel.background = element_rect(fill = "#141438",
                                        colour = 'grey69'),
        strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                    size = 14, face = "bold",
                                    colour = "grey65"),
        strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                        size = 12, face = "bold",
                                        colour = "grey65"),
        panel.grid.major.y = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(colour = "grey65"),
        axis.title = element_text(colour = "grey65"),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.title = element_text(colour = "grey65"),
        legend.position = 'bottom')



# End of script ----
