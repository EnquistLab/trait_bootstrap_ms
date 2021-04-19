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

##TODO
simdata_plankton <- 
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
               colour = 'grey69',
               size = 0.3) +
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
             size = 0.7) +
  geom_smooth(data = sim_biased_moon_means,
              aes(
                x = sample_size,
                y = deviation,
                color = method,
                linetype = "Biased"),
              se = FALSE,
              size = 0.5) +
  geom_smooth(aes(
    x = sample_size,
    y = deviation ,
    color = method,
    linetype = "Random"),
    alpha = 0.5,
    se = FALSE,
    size = 0.5) +
  geom_point(aes(
    x = sample_size,
    y = deviation,
    color = method
  ),
  size = 3,
  alpha = 0.9) +
  geom_moon(aes(
    x = sample_size,
    y = deviation,
    ratio = percentage,
    fill = method
  ),
  color = "transparent",
  size = 3) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.2),
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
  theme_moon +
  theme(legend.key.size = unit(5, "mm"),
        axis.text = element_text(size = rel(.55)))


cowplot::ggdraw(moons) +
  cowplot::draw_plot(inset,
                     width = 0.70,
                     height = 0.8,
                     x = 0.135,
                     y = 0.10) +
  cowplot::draw_image(
    img1, x = 0.03, y = 0.93, hjust = 0.5, vjust = 0.5,
    width = 0.045
  )

ggsave(here::here("figures/moons_biased_directionality.png"),
       height = 110, width = 180,
       units = "mm", dpi = 600)

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
          mutate(dataset = rep("Rodents", nrow(.))),
        simdata_plankton %>%
          mutate(dataset = rep("Plankton", nrow(.)))) %>%
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

sim_moon_means$dataset =
  ordered(sim_moon_means$dataset,levels = c("Herbs",
                                            "Tadpoles",
                                            "Trees", 
                                            "Rodents",
                                            "Plankton"))

plots <- vector('list', 5)

for (i in 1:5) {
  
  plots[[i]] = 
    ggplot(sim_moon_means %>%
             filter(dataset == levels(sim_moon_means$dataset)[i])) +
    geom_hline(aes(yintercept = 0),
               color = "grey50",
               size = .6) +
    geom_smooth(aes(
      x = sample_size,
      y = deviation ,
      color = method),
      alpha = 0.5,
      se = FALSE,
      size = 0.5) +
    geom_point(aes(
      x = sample_size,
      y = deviation,
      color = method
    ),
    size = 2,
    alpha = 0.9) +
    geom_moon(aes(
      x = sample_size,
      y = deviation,
      ratio = percentage,
      fill = method
    ),
    color = "transparent",
    size = 2) +
    coord_cartesian(clip = 'off') +
    scale_fill_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = colorspace::darken(pal_df$c, amount = 0.2),
                      labels = pal_df$l) +
    scale_colour_manual(guide = guide_legend(title = "Method",
                                             title.position="top",
                                             title.hjust = 0.5),
                        values = colorspace::lighten(pal_df$c, amount = 0.2),
                        labels = pal_df$l) +
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
         title = paste(levels(sim_moon_means$dataset)[i])) +
    # Theme
    figure_theme +
    theme(
      axis.text.x = element_text(size = rel(.5)),
      axis.text.y = element_text(size = rel(.5)),
      axis.title.x = element_text(size = rel(.5)),
      axis.title.y = element_text(size = rel(.5)),
      legend.text = element_text(size = rel(.5)),
      legend.title = element_text(size = rel(.6)),
      strip.text.y = element_text(margin = margin(0, 0, 3, 0),
                                  size = rel(.6), face = "bold"),
      strip.text.x.top = element_text(margin = margin(0, 0, 3, 0),
                                      size = rel(.6),face = "bold"),
      panel.grid.major.y = element_line(size = 0.03),
      strip.background = element_blank(),
      axis.line = element_blank(),
      strip.placement = 'outside',
      panel.background = element_rect(colour = colorspace::darken("#dddddd", 0.1),
                                      size = 0.4),
      plot.title.position = "panel",
      plot.title = element_text(margin = margin(0, 0, 10, 0),
                                size = rel(.7), face = "bold"
                                ),
      legend.position = 'none',
      plot.margin = margin(2, 2, 2, 2)
    ) 
  
}

(plots[[2]] +
    labs(title = "A: Tadpoles") + 
    plots[[3]] +
    labs(title = "B: Trees"))/
  (plots[[4]] +
     labs(title = "C: Rodents") + 
     plots[[5]] +
     labs(title = "D: Plankton")) +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = 'bottom')) 

ggsave(here::here("figures/restricted_sample_datasets.png"),
       height = 140, width = 180,
       units = "mm", dpi = 600)

# End of script ----
