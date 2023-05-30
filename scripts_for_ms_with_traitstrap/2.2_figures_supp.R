# Admin ----

#load script that determines plotting aesthetics
source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")
library(patchwork)

# Figure S1 ----

source("scripts_for_ms_with_traitstrap/3_distribution_figure.R")

# Figure S2 ----
## a) load data ----

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

simdata_plankton <- 
  tidy_simdata(readRDS("output_data/simulation_results_phyto_subset_scaled.RDS"))

bumps =
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
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate))) %>%
  # group_by(dataset, moment, sample_size, site, trait) %>%
  filter(hit == 2)  %>%
  group_by(dataset, method, moment, trait, site, sample_size) %>%
  summarise(n = min(deviation)) %>%
  group_by(dataset, method, moment, sample_size) %>%
  summarise(n = mean(n)) %>%
  group_by(dataset, moment, sample_size) %>%
  mutate(rank = rank(n)) %>%
  mutate(rank = ifelse(is.na(rank),
                       5,
                       rank))

bumps$dataset <- factor(bumps$dataset,
                        levels = c("Herbs",
                                   "Tadpoles",
                                   "Trees",
                                   "Rodents",
                                   "Plankton"))

## b) plot ----
ggplot(bumps %>%
         filter(sample_size < 50)) +
  with_blur(
    geom_bump(aes(x = sample_size,
                  y = -rank,
                  colour = method),
              size = 0.7, smooth = 8),
    sigma = 1) +
  geom_point(aes(x = sample_size,
                 y = -rank,
                 colour = method),
             size = 1) +
  facet_grid(cols = vars(moment),
             rows = vars(dataset),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y') +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = pal_df$c,
                      labels = pal_df$l) +
  labs(x = 'Sample size',
       y = "Rank") +
  scale_x_continuous(trans = 'sqrt', breaks = c(1,4,9,16,25,36,49),
                     limits = c(1, 50)) +
  scale_y_continuous(breaks = c(-1,-4),
                     labels = c("Best", "Worst"),
                     limits = c(-4.25, -0.75)) +
  # Theme
  figure_theme +
  theme(panel.background = element_rect(colour = colorspace::darken("#dddddd", 0.1),
                                        size = 1),
        strip.text.y.left = element_text(margin = margin(0, 5, 2, 0),
                                         size = rel(0.7), vjust = 0,
                                         angle = 0,face = "bold"),
        strip.text.x.top = element_text(margin = margin(0, 0, 2, 0),
                                        size = rel(1),face = "bold"),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        #strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'bottom',
        legend.key.size = unit(3, "mm")) +
  inset_element(img5,
                left = 0.0,
                bottom = 0.21,
                right = 0.07,
                top = 0.30, 
                align_to = 'full') + theme_void() +
  inset_element(img4,
                left = 0.0,
                bottom = 0.37,
                right = 0.07,
                top = 0.44, 
                align_to = 'full') + theme_void() +
  inset_element(img3,
                left = 0.01,
                bottom = 0.52,
                right = 0.08,
                top = 0.61, 
                align_to = 'full') + theme_void() +
  inset_element(img2,
                left = 0.01,
                bottom = 0.66,
                right = 0.08,
                top = 0.78, 
                align_to = 'full') + theme_void() +
  inset_element(img1,
                left = 0.0,
                bottom = 0.83,
                right = 0.07,
                top = 0.92, 
                align_to = 'full') + theme_void()

ggsave(here::here("figures/Figure_S2.png"),
       height = 130, width = 180,
       units = "mm", dpi = 600)

# Figure S3 ----

## a) data ----
simdata_lollipop =
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(trait, moment, method, sample_size) %>%
  slice_sample(n = 20)

simmeans =
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(trait, moment, method) %>%
  summarise(estimate = mean(deviation))

simdata_lollipop =
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(trait, moment, method, sample_size) %>%
  slice_sample(n = 20)

## b) plot ----
cowplot::ggdraw(
  ggplot(simmeans) +
    geom_vline(aes(xintercept = 0),
               color = "grey69",
               size = 0.7) +
    geom_segment(data = simmeans,
                 aes(x = 0,
                     xend = estimate,
                     y = method,
                     yend = method),
                 color = "grey69",
                 size = 0.3) +
    geom_point(data = simdata_lollipop,
               aes(x = deviation,
                   y = method,
                   fill = method,
                   alpha = hit),
               color = colorspace::lighten("#5e5e5e", 0.3),
               size = 1, stroke = 0.2,
               position = position_jitternormal(sd_x = 0, sd_y = 0.1), 
               shape = 21) +
    geom_point(data = simmeans,
               aes(x = estimate,
                   y = method,
                   fill = method,
                   colour = method),
               shape = 23, size = 1.7) +
    facet_grid(rows = vars(trait),
               cols = vars(moment),
               labeller = labeller(
                 .default = capitalize,
                 trait = traits_parsed
               ),
               switch = 'y',
               scales = 'free')  +
    scale_fill_manual(values = pal_df$c,
                      breaks = pal_df$l) +
    scale_colour_manual(values = colorspace::darken(pal_df$c, 0.5),
                        breaks = pal_df$l) +
    scale_alpha_discrete(range = c(0.5, 0.9)) +
    labs(x = "Deviation from true value",
         y = NULL) +
    guides(colour = guide_legend(title = "Method",
                                 title.position="top",
                                 title.hjust = 0.5,
                                 override.aes = list(shape = 21,
                                                     size = 3,
                                                     order = 1)),
           fill = guide_legend(title = "Method",
                               title.position="top",
                               title.hjust = 0.5,
                               override.aes = list(shape = 21,
                                                   size = 3,
                                                   order = 1)),
           alpha = guide_legend(title = "Value in CI",
                                title.position="top",
                                title.hjust = 0.5,
                                override.aes = list(shape = 16,
                                                    size = 3,
                                                    order = 2,
                                                    colour = "#5e5e5e"))) +
    theme_lollipop +
    theme(plot.margin = margin(15, 15, 10, 20))
) +
  cowplot::draw_image(
    img1, x = 0.03, y = 0.94, hjust = 0.5, vjust = 0.5,
    width = 0.04
  )

ggsave(here::here("figures/Figure_SI2.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)

