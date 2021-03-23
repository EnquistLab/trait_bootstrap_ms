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
            deviation = mean(mean(abs(deviation))))

sim_biased_moon_means$moment =
  ordered(sim_biased_moon_means$moment,levels = c("mean",
                                                  "variance",
                                                  "skewness",
                                                  "kurtosis"))

sim_moon_means$moment =
  ordered(sim_moon_means$moment,levels = c("mean",
                                           "variance",
                                           "skewness",
                                           "kurtosis"))

overunders$moment =
  ordered(overunders$moment,levels = c("mean",
                                       "variance",
                                       "skewness",
                                       "kurtosis"))

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
    strip.placement = 'outside',
    panel.background = element_rect(colour = colorspace::lighten("#141438", 0.1),
                                    size = 1)
  ) 


cowplot::ggdraw(moons) +
  cowplot::draw_plot(moon_legend,
                     .8, .12,
                     0.21, .22) +
  cowplot::draw_plot(inset,
                     width = 0.8,
                     height = 0.9,
                     x = 0.1,
                     y = 0.07)

ggsave(here::here("figures/moons_biased_directionality.png"),
       height = 7.4, width = 12.5,
       units = "in", dpi = 300)

### Transition plots - accuracy & directionality of moments - 'global' ----


#### All traits combined

sim_means =
  simdata %>%
  #filter(sample_size %in% c(1,9,49,100,196,441)) %>%
  group_by(method, moment, sample_size) %>%
  mutate(group_size = n()) %>%
  #if true value falls in estimate's CI
  mutate(deviation = ifelse(overunder == "over",
                            deviation,
                            -1*deviation)) %>%
  group_by(method, moment, sample_size) %>%
  summarise(mean_dev = mean(deviation)) %>%
  group_by(moment, method) %>%
  mutate(dev_pct = mean_dev/max(abs(mean_dev)))




sim_means$moment =
  ordered(sim_means$moment,levels = c("mean",
                                      "variance",
                                      "skewness",
                                      "kurtosis"))
inset =
  ggplot(sim_means %>%
           filter(sample_size %in% c(1,9,49,100,196,441))) +
  geom_col(aes(y = dev_pct,
               x = as.factor(sample_size),
               fill = method,
               group = method),
           position = 'dodge',
           show.legend = FALSE) +
  geom_hline(aes(yintercept = 0),
             colour = "grey69",
             size = 1.2) +
  # facet_grid(rows = vars(method),
  #            cols = vars(moment),
  #            labeller = labeller(
  #              trait = traits_parsed,
  #              .default = capitalize
  #            ),
  #            switch = 'y',
  #            scales = 'free') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_alpha_manual(values = c(0.4,1)) +
  guides(alpha = 'none') +
  labs(x = 'sample size') +
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
    panel.grid.major.x = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(colour = "grey65"),
    axis.title = element_blank(),
    strip.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    strip.placement = 'outside'
  )

#tiles

sim_means =
  simdata %>%
  #filter(sample_size %in% c(1,9,49,100,196,441)) %>%
  #if true value falls in estimate's CI
  mutate(deviation = ifelse(estimate > true_value,
                            estimate - true_value,
                            true_value - estimate)) %>%
  mutate(overunder = ifelse(true_value < estimate,
                            "over",
                            "under"),
         deviation = ifelse(true_value < estimate,
                            deviation,
                            -1*deviation)) %>%
  group_by(trait, moment, method, overunder) %>%
  mutate(max_dev = ifelse(overunder == "over",
                          max(deviation),
                          min(deviation))) %>%
  group_by(trait, moment, method, sample_size, overunder, max_dev) %>%
  count() %>%
  group_by(trait, moment, method, sample_size) %>%
  mutate(percentage = ifelse(overunder == "over",
                             n/sum(n),
                             -1*n/sum(n)))

ggplot(sim_means %>%
         filter(trait == 'height')) +
  geom_col(aes(x = as.factor(sample_size),
               y = percentage,
               group = method,
               fill = method),
           position = 'dodge') +
  facet_grid(cols = vars(method),
             rows = vars(moment),
             scales = "free")






ggplot(sim_means %>%
         filter(trait == "biomass_per_ind")) +
  geom_rect(aes(xmin = min_dev,
                xmax = max_dev,
                ymin = sample_size - 0.5,
                ymax = sample_size + 0.5,
                alpha = hit,
                fill = method)) +
  facet_wrap(vars(method,moment),
             scales = 'free') +
  scale_y_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                     limits = c(1, 484)) +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l)


ggplot(sim_means) +
  ggstream::geom_stream(aes(x = sample_size,
                            y = percentage,
                            alpha = hit,
                            group = paste0(overunder,hit),
                            fill = method),
                        method = "raw", bw = .6) +
  ggstream::geom_stream(aes(x = sample_size,
                            y = percentage,
                            colour = overunder,
                            group = paste0(overunder,hit)),
                        method = "raw", bw = .6, fill = NA) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  
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


ggplot(sim_means) +
  geom_col(aes(x = as.factor(sample_size),
               y = percentage,
               alpha = hit,
               group = paste0(overunder,hit),
               fill = method)) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
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

ggplot(sim_means) +
  geom_area(aes(x = sample_size,
                y = percentage,
                alpha = hit,
                group = paste0(overunder,hit, method),
                fill = method)) +
  geom_line(data = sim_means %>%
              filter(overunder == "under") %>%
              group_by(trait, moment, sample_size) %>%
              summarise(divide = sum(percentage)),
            aes(x = sample_size,
                y = divide),
            colour = "grey69",
            size = 1.2,
            linetype = 4) +
  facet_grid(cols = vars(moment),
             rows = vars(trait),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  guides(alpha = 'none') +
  labs(y = "Sample size") +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                     limits = c(1, 484)) +
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
    strip.placement = 'outside',
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

cowplot::ggdraw(moons) +
  cowplot::draw_plot(moon_legend,
                     .80, .11,
                     0.2, .23)

ggsave(here::here("figures/transition.png"),
       height = 7.4, width = 12.5,
       units = "in", dpi = 300)






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

