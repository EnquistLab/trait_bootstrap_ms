#analyze and plot simulation results


##############################################
source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

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

### Moon plots - accuracy of moments - 'global' ----

#### All traits combined

overunders = 
  simdata %>%
  filter(sample_size %in% c(1,9,49,100,196,441))  %>%
  group_by(moment, method, sample_size, overunder) %>%
  summarise(dev = mean(abs(deviation)),
            tally = n()) %>%
  group_by(moment, method, sample_size) %>%
  filter(tally == max(tally)) %>%
  group_by(moment, method) %>%
  mutate(x = dev/max(dev)) %>%
  mutate(x = ifelse(overunder == "under",
                    -1*x,
                    x))

sim_moon_means =
  simdata %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(method, moment, sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation)))

sim_biased_moon_means =
  simdata_biased %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(method, moment, sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation)))

inset <-
  ggplot(overunders) +
  geom_col(aes(y = x,
               x = as.factor(sample_size),
               fill = method),
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
                   xend = 6.5,
                   x = 0.5, yend = 0),
               colour = '#4e5368',
               size = 0.5) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(y = c(-5,5)) + 
  expand_limits(x= c(-10, 12)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )


moons <-
ggplot(sim_moon_means %>%
         filter(sample_size %in% c(1,9,49,100,196,441))) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_smooth(data = sim_biased_moon_means,
              aes(
                x = sample_size,
                y = deviation,
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
    fill = method
  ),
  color = "transparent",
  size = 5) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
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
  # Theme
  theme_moon


cowplot::ggdraw(moons) +
  cowplot::draw_plot(moon_legend,
                     x = .78, y = .11,
                     width = 0.22, height = .22) +
  cowplot::draw_plot(inset,
                     width = 0.74,
                     height = 0.9,
                     x = 0.12,
                     y = 0.07) +
  cowplot::draw_image(
    img1, x = 0.03, y = 0.93, hjust = 0.5, vjust = 0.5,
    width = 0.045
  )

ggsave(here::here("figures/moons_biased_directionality.png"),
       height = 7.4, width = 13.2,
       units = "in", dpi = 300)

#### Moons - all datasets resticted ss ----

#limit ss 1- 49

sim_moon_means =
  rbind(simdata %>%
          mutate(dataset = rep("Herbs", nrow(.))),
        simdata_frogs %>%
          mutate(dataset = rep("Tadpoles", nrow(.))),
        simdata_panama %>%
          mutate(dataset = rep("Trees", nrow(.))),
        simdata_rats %>%
          mutate(dataset = rep("Rodents", nrow(.)))) %>%
  #if true value falls in estimate's CI
  filter(sample_size < 50) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(dataset, method, moment, sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation)))

sim_moon_means$moment =
  ordered(sim_moon_means$moment,levels = c("mean",
                                                  "variance",
                                                  "skewness",
                                                 "kurtosis"))
herbs = 
ggplot(sim_moon_means %>%
         filter(dataset == 'Herbs')) +
  geom_hline(aes(yintercept = 0),
                       color = "grey50",
                       size = 1.5) +
  geom_smooth(aes(
    x = sample_size,
    y = deviation ,
    color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
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
    fill = method
  ),
  color = "transparent",
  size = 5) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +
  # scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
  #                    limits = c(0, 500)) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Sample Size",
       y = "Average deviation from true moment",
       title = "A: Herbs") +
  #draw_key_moon(data.frame(x = 1:5, y = 0, ratio = 0:4 * 0.25))
  # Theme
  theme_moon

frogs = 
  ggplot(sim_moon_means %>%
           filter(dataset == 'Tadpoles')) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_smooth(aes(
    x = sample_size,
    y = deviation ,
    color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
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
    fill = method
  ),
  color = "transparent",
  size = 5) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +
  # scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
  #                    limits = c(0, 500)) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Sample Size",
       y = "Average deviation from true moment",
       title = "B: Tadpoles") +
  # Theme
  theme_moon

  
panama = 
  ggplot(sim_moon_means %>%
           filter(dataset == 'Trees')) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_smooth(aes(
    x = sample_size,
    y = deviation ,
    color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
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
    fill = method
  ),
  color = "transparent",
  size = 5) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +
  # scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
  #                    limits = c(0, 500)) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Sample Size",
       y = "Average deviation from true moment",
       title = "C: Trees") +
  #draw_key_moon(data.frame(x = 1:5, y = 0, ratio = 0:4 * 0.25))
  # Theme
  theme_moon


rodents = 
  ggplot(sim_moon_means %>%
           filter(dataset == 'Rodents')) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_smooth(aes(
    x = sample_size,
    y = deviation ,
    color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
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
    fill = method
  ),
  color = "transparent",
  size = 5) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +
  # scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
  #                    limits = c(0, 500)) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Sample Size",
       y = "Average deviation from true moment",
       title = "D: Rodents") +
  #draw_key_moon(data.frame(x = 1:5, y = 0, ratio = 0:4 * 0.25))
  # Theme
  theme_moon

(herbs + theme(legend.position = 'bottom') + 
    frogs + theme(legend.position = 'bottom'))/
  (panama + theme(legend.position = 'bottom') + 
     rodents + theme(legend.position = 'bottom')) +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
                    plot.background = element_rect(fill = "#141438", colour = NA),
                    panel.background = element_rect(fill = "#141438", colour = NA),
                    legend.position = 'bottom')) 

ggsave(here::here("figures/restricted_sample_datasets.png"),
       height = 14, width = 2,
       units = "in", dpi = 300)


### Bonus Plots----

#### --- All traits combined----

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

#### Lollipops ----

biased_means = 
  simdata_biased %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(trait, moment, method) %>%
  summarise(estimate = mean(deviation),
            iqr = IQR(deviation))

biased_lollipop =
  simdata_biased %>%
  filter(sample_size < 26 &
           sample_size > 8)  %>%
  group_by(trait, moment, method, sample_size) %>%
  slice_sample(n = 20)

simmeans = 
  simdata  %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(trait, moment, method) %>%
  summarise(estimate = mean(deviation),
            std_dev = IQR(deviation))

#re-order to match moment 'numbers'
biased_means$moment <- factor(biased_means$moment,
                              levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))


biased_lollipop$moment <- factor(biased_lollipop$moment,
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

ggplot(biased_means) + 
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
  geom_pointrange(data = simmeans,
                  aes(x = estimate,
                      xmax = estimate + std_dev,
                      xmin = estimate - std_dev,
                      y = method,
                      fill = method,
                      colour = method),
                  position = position_nudge(y = -0.3)) +
  geom_jitter(data = biased_lollipop,
              aes(x = deviation, 
                  y = method, 
                  fill = method,
                  size = as.factor(sample_size),
                  alpha = hit), 
              color = "grey85", 
              width = 0, height = 0.2, shape = 21) +
  geom_pointrange(data = biased_means,
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
  scale_size_discrete(guide = guide_legend(title = "Sample Size"),
                      range = c(.7, 2.7)) +
  scale_alpha_discrete(guide = guide_legend(title = "Value in CI",
                                            override.aes = list(shape = 16)),
                       range = c(0.2, 0.4)) +
  labs(
    x = "Deviation from true value",
    y = NULL
  ) +
  #guides(size = 'none') +
  theme_lollipop


# End of script ----
