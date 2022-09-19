# plotting of abundance-biased simulations

source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

#### Read Data ####
#read in data
herb_abun =
  tidy_simdata(readRDS("output_data/simulation_results_abundance_biased.RDS"))

rats_abun =
  tidy_simdata(readRDS("output_data/simulation_results_rodents_abundance_biased.RDS"))

#### Herb Data ####

herb_abun_plot = 
ggplot(herb_abun %>%
         mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                             2,
                             1)) %>%
         group_by(method, moment, sample_size, sampling_bias) %>%
         summarise(percentage = sum(hit - 1)/n(),
                   deviation = mean(abs(deviation)))) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = .6) +
  geom_smooth(aes(
    x = sample_size,
    y = deviation ,
    color = method,
    linetype = sampling_bias),
    alpha = 0.5,
    se = FALSE,
    size = 0.3) +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                     limits = c(0, 500)) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = colorspace::lighten(pal_df$c, amount = 0.1),
                      labels = pal_df$l) +
  scale_linetype_manual(guide = guide_legend(title = "Sampling Bias",
                                           title.position="top",
                                           title.hjust = 0.5,
                                           override.aes = list(colour = "black")),
                      values = c(2,1,3),
                      labels = c("Common species bias", "No bias", "Rare species bias")) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Sample size",
       y = "Average deviation from true moment") +
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
    plot.margin = margin(2, 2, 2, 2)
  ) 

#### Rodent Data ####

rats_abun_plot = 
  ggplot(rats_abun %>%
           mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                               2,
                               1)) %>%
           group_by(method, moment, sample_size, sampling_bias) %>%
           summarise(percentage = sum(hit - 1)/n(),
                     deviation = mean(abs(deviation)))) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = .6) +
  geom_smooth(aes(
    x = sample_size,
    y = deviation ,
    color = method,
    linetype = sampling_bias),
    alpha = 0.5,
    se = FALSE,
    size = 0.3) +
  coord_cartesian(clip = 'off') +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200),
                     limits = c(0, 300)) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = colorspace::lighten(pal_df$c, amount = 0.1),
                      labels = pal_df$l) +
  scale_linetype_manual(guide = guide_legend(title = "Sampling Bias",
                                             title.position="top",
                                             title.hjust = 0.5,
                                             override.aes = list(colour = "black")),
                        values = c(2,1,3),
                        labels = c("Common species bias", "No bias", "Rare species bias")) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Sample size",
       y = "Average deviation from true moment") +
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
    plot.margin = margin(2, 2, 2, 2)
  ) 


#### Make plot panels ####

herb_abun_plot +
  labs(title = "A: Herbs") +
  inset_element(img1,
                left = 0.02,
                bottom = 0.87,
                right = 0.09,
                top = 1, 
                align_to = 'full') + theme_void() +
  rats_abun_plot +
  labs(title = "B: Rodents") +
  inset_element(img4,
                left = 0.02,
                bottom = 0.89,
                right = 0.14,
                top = 1, 
                align_to = 'full') + theme_void() +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = 'bottom')) 

ggsave(here::here("figures/Abun_bias.png"),
       height = 100, width = 180,
       units = "mm", dpi = 600)
# ggsave(here::here("figures/pdf/Abun_bias.pdf"),
#        height = 100, width = 180,
#        units = "mm", dpi = 600)