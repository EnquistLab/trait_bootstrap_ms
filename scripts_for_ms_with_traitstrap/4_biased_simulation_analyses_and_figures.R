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

