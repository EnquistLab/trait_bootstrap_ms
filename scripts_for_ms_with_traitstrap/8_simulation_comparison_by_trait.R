#analyze and plot simulation results by trait

source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

#### Read Data ####


#read in data
simdata =
  tidy_simdata(readRDS("output_data/simulation_results.RDS"))

simdata_rats <-
  tidy_simdata(readRDS("output_data/simulation_results_rodents.RDS"))

#### Moon plots data wrangle####

sim_moon_means =
  simdata %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(method, moment, sample_size, trait) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation))) %>%
  filter(sample_size < 51)

sim_moon_means_AZ =
  simdata_rats %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(method, moment, sample_size, trait) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation))) %>%
  filter(sample_size < 51) %>%
  mutate(trait = case_when(trait == "log10_weight" ~ "Weight"))

#### Moon plots ####

ggplot(sim_moon_means %>%
         filter(method == "Nonparametric BS")) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 0.3) +
  geom_smooth(data = sim_moon_means,
              aes(
                x = sample_size,
                y = deviation,
                color = method,
                linetype = method),
              se = FALSE,
              size = 0.4,
              alpha = 0.5) +
  geom_point(aes(
    x = sample_size,
    y = deviation,
    color = method
  ),
  size = 1,
  alpha = 0.9) +
  geom_moon(aes(
    x = sample_size,
    y = deviation,
    ratio = percentage,
    fill = method
  ),
  color = "transparent",
  size = 1.2) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.1),
                      breaks = pal_df$l) +
  scale_linetype_manual(values=c(2,2,2,1),
                        guide = guide_legend(override.aes = list(colour = "grey69"))) +
  facet_grid(rows = vars(moment),
             cols = vars(trait),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  guides(linetype = 'none',
         fill = 'none',
         colour = guide_legend(override.aes = list(linetype = 0),
                               title = "Method",
                               title.position = "top",
                               title.hjust = 0.5)) +
  labs(x = "Sample Size",
       y = "Average deviation from true moment",
       title = "A: Herbs") +
  theme_moon +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(size = rel(.3)),
    axis.title = element_text(size = rel(.5)),
    legend.text = element_text(size = rel(.4)),
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
    plot.title = element_text(margin = margin(0, 1, 10, 0),
                              size = rel(.7), face = "bold"),
    legend.position = 'bottom',
    plot.margin = margin(2, 0, 2, 2),
    legend.key.size = unit(3, "mm"),
    axis.ticks.length=unit(0.25, "mm")
  ) +
  inset_element(img1,
                left = 0.01,
                bottom = 0.90,
                right = 0.05,
                top = 0.98, 
                align_to = 'full') + theme_void() +
  #AZ Rodents
  ggplot(sim_moon_means_AZ %>%
           filter(method == "Nonparametric BS")) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 0.3) +
  geom_smooth(data = sim_moon_means_AZ,
              aes(
                x = sample_size,
                y = deviation,
                color = method,
                linetype = method),
              se = FALSE,
              size = 0.4,
              alpha = 0.5) +
  geom_point(aes(
    x = sample_size,
    y = deviation,
    color = method
  ),
  size = 1,
  alpha = 0.9) +
  geom_moon(aes(
    x = sample_size,
    y = deviation,
    ratio = percentage,
    fill = method
  ),
  color = "transparent",
  size = 1.2) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.1),
                      breaks = pal_df$l) +
  scale_linetype_manual(values=c(2,2,2,1),
                        guide = guide_legend(override.aes = list(colour = "grey69"))) +
  facet_grid(rows = vars(moment),
             cols = vars(trait),
             labeller = labeller(
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  guides(linetype = 'none',
         fill = 'none',
         colour = guide_legend(override.aes = list(linetype = 0),
                               title = "Method",
                               title.position = "top",
                               title.hjust = 0.5)) +
  labs(x = "Sample Size",
       y = "Average deviation from true moment",
       title = "B: Rodents") +
  theme_moon +
  theme(
    axis.ticks = element_blank(),
    axis.text= element_text(size = rel(.3)),
    axis.title.x = element_text(size = rel(.5)),
    axis.title.y = element_blank(),
    legend.text = element_text(size = rel(.4)),
    legend.title = element_text(size = rel(.5)),
    strip.text.y = element_blank(),
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
    legend.position = 'bottom',
    plot.margin = margin(2, 2, 2, 0),
    legend.key.size = unit(3, "mm"),
    axis.ticks.length=unit(0.25, "mm")
  ) +
  inset_element(img4,
                left = 0.01,
                bottom = 0.90,
                right = 0.23,
                top = 0.98, 
                align_to = 'full') + theme_void() +
  plot_layout(guides = 'collect',
              widths = c(1, 0.21)) +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = 'bottom')) 

ggsave(here::here("figures/Figure_SI_8.png"),
       height = 90, width = 150,
       units = "mm", dpi = 600)
ggsave(here::here("figures/pdf/Figure_SI_8.pdf"),
       height = 90, width = 150,
       units = "mm", dpi = 600)  
