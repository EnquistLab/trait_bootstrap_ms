# Plotting relative rank (somehow)

source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

#### Read Data ####

#read in data
multi_method =
  readRDS(
    "output_data/multidimensional_bootstrap_sample_size_and_method_sims.RDS") %>%
  ungroup() %>%
  pivot_longer(cols = c('FEve', 'FDis', 'RaoQ', 'FRic', 'FDiv'),
               names_to = 'measure',
               values_to = 'estimate') %>%
  pivot_longer(cols = contains('ci_high'),
               names_to = 'ci_high_measure',
               values_to = 'ci_high') %>%
  mutate(ci_high_measure = str_replace(ci_high_measure, 
                                       "ci_high_", 
                                       "")) %>%
  filter(ci_high_measure == measure) %>%
  pivot_longer(cols = contains('ci_low'),
               names_to = 'ci_low_measure',
               values_to = 'ci_low') %>%
  mutate(ci_low_measure = str_replace(ci_low_measure, 
                                      "ci_low_", 
                                      "")) %>%
  filter(ci_low_measure == measure) %>%
  pivot_longer(cols = contains('true'),
               names_to = 'true_measure',
               values_to = 'true_value') %>%
  mutate(true_measure = str_replace(true_measure, 
                                    "true_", 
                                    ""),
         method = case_when(method == 'global cwm' ~ 'Cross-Site CW',
                            method == 'site-specic CWM' ~ 'Site-Specific CW',
                            method == 'nonparametric bs' ~ 'Nonparametric BS',
                            method == 'parametric bs' ~ 'Parametric BS',
                            method == 'original' ~ 'Traditional CWM',
                            TRUE ~ method)) %>%
  filter(true_measure == measure) %>%
  select(-c(true_measure, ci_low_measure, ci_high_measure)) %>%
  mutate(method = ordered(method,levels = c("Traditional CWM",
                                            "Cross-Site CW",
                                            "Site-Specific CW",
                                            "Nonparametric BS")),
         overunder = ifelse(true_value <= estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate)),
         hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      'Yes',
                      'No')) %>%
  mutate(deviation = ifelse(overunder == "under",
                            -1*deviation,
                            deviation),
         measure = ordered(measure,
                           levels = c('FEve', 'FDis', 'RaoQ', 'FRic', 'FDiv')))


multi_rarity =
  readRDS(
    "output_data/multidimensional_bootstrap_sample_size_and_method_rarity_sims.RDS") %>%
  ungroup() %>%
  pivot_longer(cols = c('avg_uniqueness', 'avg_distinctiveness'),
               names_to = 'measure',
               values_to = 'estimate') %>%
  pivot_longer(cols = contains('ci_high'),
               names_to = 'ci_high_measure',
               values_to = 'ci_high') %>%
  mutate(ci_high_measure = str_replace(ci_high_measure, 
                                       "ci_high_", 
                                       "")) %>%
  filter(ci_high_measure == measure) %>%
  pivot_longer(cols = contains('ci_low'),
               names_to = 'ci_low_measure',
               values_to = 'ci_low') %>%
  mutate(ci_low_measure = str_replace(ci_low_measure, 
                                      "ci_low_", 
                                      "")) %>%
  filter(ci_low_measure == measure) %>%
  pivot_longer(cols = contains('true'),
               names_to = 'true_measure',
               values_to = 'true_value') %>%
  mutate(true_measure = case_when(true_measure == "true_mean_uniqueness" ~ "avg_uniqueness",
                                  true_measure == "true_mean_distinctiveness" ~ "avg_distinctiveness"),
         method = case_when(method == 'global cwm' ~ 'Cross-Site CW',
                            method == 'site-specic CWM' ~ 'Site-Specific CW',
                            method == 'nonparametric bs' ~ 'Nonparametric BS',
                            method == 'parametric bs' ~ 'Parametric BS',
                            method == 'original' ~ 'Traditional CWM',
                            TRUE ~ method)) %>%
  filter(true_measure == measure) %>%
  select(-c(true_measure, ci_low_measure, ci_high_measure)) %>%
  mutate(method = ordered(method,levels = c("Traditional CWM",
                                            "Cross-Site CW",
                                            "Site-Specific CW",
                                            "Nonparametric BS")),
         overunder = ifelse(true_value <= estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate)),
         hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      'Yes',
                      'No')) %>%
  mutate(measure = case_when(measure == "avg_uniqueness" ~ "Mean uniqueness",
                             measure == "avg_distinctiveness" ~ "Mean distinctiveness")) %>%
  mutate(deviation = ifelse(overunder == "under",
                            -1*deviation,
                            deviation),
         measure = ordered(measure,
                           levels = c('Mean uniqueness', 'Mean distinctiveness')))

#### Plotting moons ----

overunders = 
  rbind(multi_method,
        multi_rarity) %>%
  filter(estimate != is.na(estimate)) %>%
  mutate(overunder = ifelse(true_value <= estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate))) %>%
  group_by(measure, method, sample_size, overunder) %>%
  summarise(dev = mean(abs(deviation), na.rm = TRUE),
            tally = n()) %>%
  group_by(measure, method, sample_size) %>%
  filter(tally == max(tally)) %>%
  group_by(measure, method) %>%
  mutate(x = dev/max(dev)) %>%
  mutate(x = ifelse(overunder == "under",
                    -1*x,
                    x))

sim_moon_means =
  rbind(multi_method,
        multi_rarity) %>%
  filter(estimate != is.na(estimate)) %>%
  #if true value falls in estimate's CI
  filter(estimate != is.na(estimate)) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(method, measure, sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = as.numeric(sum(hit - 1)/n()),
            deviation = mean(ifelse(abs(estimate) > abs(true_value),
                                    abs(estimate) - abs(true_value),
                                    abs(true_value) - abs(estimate)),
                             na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(method = ordered(method,levels = c("Traditional CWM",
                                            "Cross-Site CW",
                                            "Site-Specific CW",
                                            "Nonparametric BS")))

moon_plots <-
  ggplot(sim_moon_means %>%
           filter(method != "Traditional CWM")) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = .3) +
  geom_smooth(
    data = rbind(multi_method,
                 multi_rarity) %>%
      filter(method != "Traditional CWM") %>%
      filter(estimate != is.na(estimate)) %>%
      mutate(deviation = ifelse(abs(estimate) > abs(true_value),
                                abs(estimate) - abs(true_value),
                                abs(true_value) - abs(estimate))) %>%
      mutate(method = ordered(method,levels = c("Traditional CWM",
                                                "Cross-Site CW",
                                                "Site-Specific CW",
                                                "Nonparametric BS"))),
    aes(
      x = sample_size,
      y = deviation ,
      color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.4,
    linetype = 4) +
  geom_point(aes(
    x = sample_size,
    y = deviation,
    color = method
  ),
  size = 1,
  alpha = 0.9) +
  geom_moon(
    aes(
    x = sample_size,
    y = deviation,
    ratio = percentage,
    fill = method
  ),
  color = "transparent",
  size = 1.2) +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c[c(1,2,4)], 
                                                amount = 0.25),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c[c(1,2,4)], 
                                                   amount = 0.1),
                      labels = pal_df$l) +
  facet_grid(rows = vars(measure),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Sample size",
       y = "Average deviation from true metric") +
  # Theme
  theme_moon +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(size = rel(.3)),
    axis.title = element_text(size = rel(.5)),
    legend.text = element_text(size = rel(.3)),
    legend.title = element_text(size = rel(.5)),
    strip.text.y = element_text(margin = margin(0, 0, 3, 0),
                                size = rel(.5), face = "bold"),
    strip.text.x.top = element_text(margin = margin(0, 0, 3, 0),
                                    size = rel(.5), face = "bold"),
    panel.grid.major.y = element_line(size = 0.03),
    strip.background = element_blank(),
    axis.line = element_blank(),
    strip.placement = 'outside',
    panel.background = element_rect(colour = colorspace::lighten("#141438", 0.1),
                                    size = 0.3),
    plot.title.position = "panel",
    plot.title = element_text(margin = margin(0, 0, 10, 0),
                              size = rel(.7), face = "bold"),
    legend.position = 'right',
    plot.margin = margin(2, 2, 2, 2),
    legend.key.size = unit(3, "mm"),
    axis.ticks.length=unit(0.25, "mm")
  )

inset_plots =
  ggplot(overunders) +
  geom_col(aes(y = x,
               x = as.factor(sample_size),
               fill = method,
               group = method),
           position = 'dodge',
           alpha = 0.5,
           show.legend = FALSE) +
  facet_grid(rows = vars(measure),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y')  + 
  geom_segment(aes(y = 0,
                   xend = 7.5,
                   x = 0.5, yend = 0),
               colour = 'grey69',
               size = 0.2) +
  scale_fill_manual(values = pal_df$c[c(1,2,4)],
                    breaks = pal_df$l[c(1,2,4)]) +
  lims(y = c(-1.3,10))  +
  scale_x_continuous(trans = 'sqrt', breaks = c(1,4,9,16,25, 36,49),
                     limits = c(1, 50))+ 
  expand_limits(x= c(0, 23)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )


((moon_plots +
    theme(legend.position = 'right',
          plot.tag = element_text(size = rel(0.7), 
                                  face = "bold")) +
    inset_element(img1,
                  left = 0.00,
                  bottom = 0.95,
                  right = 0.05,
                  top = 1, 
                  align_to = 'full') + theme_void())) +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = 'right')) +
  inset_element(moon_legend,
                left = 0.88,
                bottom = 0.57,
                right = 1,
                top = 0.7, 
                align_to = 'full') + theme_void()

ggsave(here::here("figures/multi_dim.png"),
       height = 150, width = 180,
       units = "mm", dpi = 600)
# ggsave(here::here("figures/pdf/Rel_rank.pdf"),
#        height = 120, width = 180,
#        units = "mm", dpi = 600)

#### Plotting relative rank ----

cc <- c("#35BBCA", "#0191B4", "#F8D90F", "#D3DD18", "#FE7A15")
cc <- c("#031B88", "#6096FD", "#AAB6FB", "#FB7B8E", "#FAA7B8")


(ggplot(rbind(multi_method,
             multi_rarity)  %>%
         left_join(rbind(multi_method,
                         multi_rarity) %>%
                     distinct(measure, site, true_value) %>%
                     group_by(measure) %>%
                     mutate(my_ranks = as.factor(order(
                       order(true_value, 
                             decreasing=TRUE)))))) +
  geom_hline(aes(yintercept = true_value,
                 color = my_ranks),
             size = .3) +
  # geom_ribbon(
  #   data = rbind(multi_method,
  #                multi_rarity) %>%
  #     group_by(measure, method, site) %>%
  #     summarise(spline_x = spline(sample_size, 
  #                                 ci_high)$x,
  #               spline_hi = spline(sample_size, 
  #                                  ci_high)$y,
  #               spline_lo = spline(sample_size, 
  #                                  ci_low)$y) %>%
  #     left_join(rbind(multi_method,
  #                     multi_rarity) %>%
  #                 distinct(measure, site, true_value) %>%
  #                 group_by(measure) %>%
  #                 mutate(my_ranks = as.factor(order(
  #                   order(true_value, 
  #                         decreasing=TRUE))))),
  #   aes(
  #     x = spline_x,
  #     ymin = spline_lo,
  #     ymax = spline_hi,
  #     fill = my_ranks),
  #   alpha = 0.2) +
  geom_smooth(
    aes(
      x = sample_size,
      y = estimate,
      color = my_ranks),
    alpha = 0.5,
    se = FALSE,
    size = 0.4,
    linetype = 4) +
  facet_grid(cols = vars(method),
             rows = vars(measure),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Sample size",
       y = "") +
  scale_colour_manual(values=cc) +
  scale_fill_manual(values=cc) +
  # Theme
  theme_moon +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(size = rel(.3)),
    axis.title = element_text(size = rel(.5)),
    legend.text = element_text(size = rel(.3)),
    legend.title = element_text(size = rel(.5)),
    strip.text.y = element_text(margin = margin(0, 0, 3, 0),
                                size = rel(.5), face = "bold"),
    strip.text.x.top = element_text(margin = margin(0, 0, 3, 0),
                                    size = rel(.5), face = "bold"),
    panel.grid.major.y = element_line(size = 0.03),
    strip.background = element_blank(),
    axis.line = element_blank(),
    strip.placement = 'outside',
    panel.background = element_rect(colour = colorspace::lighten("#141438", 0.1),
                                    size = 0.3),
    plot.title.position = "panel",
    plot.title = element_text(margin = margin(0, 0, 10, 0),
                              size = rel(.7), face = "bold"),
    legend.position = 'none',
    plot.margin = margin(2, 2, 2, 2),
    legend.key.size = unit(3, "mm"),
    axis.ticks.length=unit(0.25, "mm")
  ) +
  inset_element(img1,
                left = 0.00,
                bottom = 0.95,
                right = 0.06,
                top = 1, 
                align_to = 'full') + theme_void())

ggsave(here::here("figures/mutli_rel_rank.png"),
       height = 180, width = 120,
       units = "mm", dpi = 600)
