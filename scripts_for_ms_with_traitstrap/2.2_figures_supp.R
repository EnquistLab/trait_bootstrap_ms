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

simmeans =
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
  group_by(dataset, moment) %>%
  mutate(true_val = mean(true_value)) %>%
  group_by(dataset, moment, method, true_val) %>%
  summarise(estimate = mean(deviation))

simdata_lollipop =
  rbind(simdata %>%
          mutate(dataset = rep("Herbs", nrow(.))),
        simdata_frogs %>%
          mutate(dataset = rep("Tadpoles", nrow(.))),
        simdata_panama %>%
          mutate(dataset = rep("Trees", nrow(.))),
        simdata_rats %>%
          mutate(dataset = rep("Rodents", nrow(.))),
        simdata_plankton %>%
          mutate(dataset = rep("Plankton", nrow(.))))  %>%
  filter(deviation < 10 &
           deviation > -10 ) %>%
  group_by(dataset, moment, method, sample_size) %>%
  slice_sample(n = 20)

#re-order to match moment 'numbers'

simdata_lollipop$dataset <- factor(simdata_lollipop$dataset,
                                   levels = c("Herbs",
                                              "Tadpoles",
                                              "Trees", 
                                              "Rodents",
                                              "Plankton"))

simmeans$dataset <- factor(simmeans$dataset,
                           levels = c("Herbs",
                                      "Tadpoles",
                                      "Trees", 
                                      "Rodents",
                                      "Plankton"))
## b) plot ----

lollipop_all =
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
             shape = 23, size = 2) +
  facet_grid(rows = vars(dataset),
             cols = vars(moment),
             labeller = labeller(
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free')  +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top",
                                         title.hjust = 0.5,
                                         override.aes = list(shape = 21)),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5,
                                           override.aes = list(shape = 21)),
                      values = colorspace::darken(pal_df$c, 0.5),
                      breaks = pal_df$l) +
  scale_alpha_discrete(guide = guide_legend(title = "Value in CI",
                                            title.position="top",
                                            title.hjust = 0.5,
                                            override.aes = list(shape = 16,
                                                                size = 3,
                                                                colour = "#5e5e5e")),
                       range = c(0.5, 0.9)) +
  labs(
    x = "Deviation from true value",
    y = NULL
  ) +
  theme_lollipop

lollipop_all  +
  inset_element(img5,
                left = 0.0,
                bottom = 0.26,
                right = 0.07,
                top = 0.35, 
                align_to = 'full') + theme_void() +
  inset_element(img4,
                left = 0.0,
                bottom = 0.4,
                right = 0.07,
                top = 0.47, 
                align_to = 'full') + theme_void() +
  inset_element(img3,
                left = 0.01,
                bottom = 0.54,
                right = 0.08,
                top = 0.63, 
                align_to = 'full') + theme_void() +
  inset_element(img2,
                left = 0.01,
                bottom = 0.67,
                right = 0.08,
                top = 0.78, 
                align_to = 'full') + theme_void() +
  inset_element(img1,
                left = 0.0,
                bottom = 0.83,
                right = 0.07,
                top = 0.92, 
                align_to = 'full') + theme_void()


ggsave(here::here("figures/Figure_S3.png"),
       height = 115, width = 180,
       units = "mm", dpi = 600)

# Figure S4 ----

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

ggsave(here::here("figures/Figure_S4.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)


# Figure S5 ----
## a) data ----
sim_radar =
  simdata_panama %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  mutate(diff = ifelse(abs(estimate) > abs(true_value),
                       abs(estimate) - abs(true_value),
                       abs(true_value) - abs(estimate)),
         hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(trait, moment, sample_size, site) %>%
  filter(hit == 2) %>%
  filter(diff == min(diff)) %>%
  group_by(trait, method, moment) %>%
  count() %>%
  group_by(moment, trait) %>%
  mutate(percentage = n/60) %>%
  select(-n) %>%
  mutate(percentage = ifelse(is.na(percentage),
                             0,
                             percentage))

sim_win_text =
  sim_radar %>%
  group_by(moment, trait) %>%
  filter(percentage == max(percentage)) %>%
  mutate(percentage = round(percentage*100))

sim_radar$trait <- factor(sim_radar$trait,
                          levels = c("Area",
                                     "Dry.weight",
                                     "Fresh.weight",
                                     "LDMC",
                                     "LMA",
                                     "LCC",
                                     "LNC",
                                     "N.C"))

sim_win_text$trait <- factor(sim_win_text$trait,
                             levels = c("Area",
                                        "Dry.weight",
                                        "Fresh.weight",
                                        "LDMC",
                                        "LMA",
                                        "LCC",
                                        "LNC",
                                        "N.C"))

over_under =
  simdata_panama %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  mutate(overunder = ifelse(true_value < estimate,
                            "over",
                            "under"),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate))) %>%
  group_by(trait, moment, method, overunder) %>%
  summarise(dev = mean(deviation),
            tally = n()) %>%
  group_by(trait, moment, method) %>%
  filter(tally == max(tally)) %>%
  group_by(trait, moment, overunder) %>%
  mutate(x = dev/max(dev)) %>%
  mutate(x = ifelse(overunder == "under",
                    -1*x,
                    x))

over_under$trait <- factor(over_under$trait,
                           levels = c("Area",
                                      "Dry.weight",
                                      "Fresh.weight",
                                      "LDMC",
                                      "LMA",
                                      "LCC",
                                      "LNC",
                                      "N.C"))

## b) plot ----

doughnut =
  ggplot(sim_radar) +
  geom_col(aes(
    x = 2,
    y = percentage,
    fill = method
  ),
  colour = 'grey96') +
  xlim(c(0.5, 2.5)) +
  ylim(c(0, 1)) +
  #annotation textboxes
  geom_text(data = sim_win_text,
            aes(x = 0.5,
                y = 0.25,
                colour = method,
                label = glue::glue("{percentage}%")),
            hjust = 0.5,
            show.legend = FALSE,
            fontface = 'bold',
            size = 4.5) +
  coord_polar(theta = 'y') +
  facet_grid(rows = vars(trait),
             cols = vars(moment),
             labeller = labeller(
               trait = traits_panama,
               .default = capitalize
             ),
             switch = 'y')  +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = colorspace::darken(pal_df$c, 0.2),
                      breaks = pal_df$l) +
  # Theme
  theme_doughnut

inset =
  ggplot(over_under) +
  geom_col(aes(y = x,
               x = method,
               fill = method),
           alpha = 0.5,
           show.legend = FALSE) +
  facet_grid(rows = vars(trait),
             cols = vars(moment),
             labeller = labeller(
               trait = traits_panama,
               .default = capitalize
             ),
             switch = 'y')  +
  geom_segment(aes(y = 0,
                   xend = 4.5,
                   x = 0.5, yend = 0),
               colour = 'grey69',
               size = 0.7) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(y = c(-5,5)) +
  expand_limits(x= c(-9, 11)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )


cowplot::ggdraw(doughnut) +
  cowplot::draw_plot(inset,
                     width = 0.76,
                     height = 0.92,
                     x = 0.26,
                     y = 0.09) +
  cowplot::draw_image(
    img3, x = 0.06, y = 0.965, hjust = 0.5, vjust = 0.5,
    width = 0.07
  )

ggsave(here::here("figures/Figure_S5.png"),
       height = 314, width = 180,
       units = "mm", dpi = 600)

# Figure S6 ----
## a) data ----
sim_radar =
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate))) %>%
  group_by(trait, moment, sample_size, site) %>%
  filter(hit == 2) %>%
  filter(deviation == min(deviation)) %>%
  group_by(trait, method, moment) %>%
  count() %>%
  group_by(moment, trait) %>%
  mutate(percentage = n/15) %>%
  select(-n) %>%
  mutate(percentage = ifelse(is.na(percentage),
                             0,
                             percentage))

sim_win_text =
  sim_radar %>%
  group_by(moment, trait) %>%
  filter(percentage == max(percentage)) %>%
  mutate(percentage = round(percentage*100))

## b) plot ----

doughnut_CO =
  ggplot(sim_radar) +
  geom_col(aes(
    x = 2,
    y = percentage,
    fill = method
  ),
  colour = 'grey96') +
  xlim(c(0.5, 2.5)) +
  ylim(c(0, 1))+
  #annotation textboxes
  geom_text(data = sim_win_text,
            aes(x = 0.5,
                y = 0.25,
                colour = method,
                label = glue::glue("{percentage}%")),
            #label = glue::glue("{method} - {percentage}%")),
            #colour = 'grey90',
            hjust = 0.5,
            size = 4,
            show.legend = FALSE,
            fontface = 'bold') +
  coord_polar(theta = 'y') +
  facet_grid(rows = vars(trait),
             cols = vars(moment),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y')  +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values =colorspace::darken(pal_df$c, 0.2),
                      breaks = pal_df$l) +
  # Theme
  theme_doughnut

over_under =
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(trait, moment, method, overunder) %>%
  summarise(dev = mean(abs(deviation)),
            tally = n()) %>%
  group_by(trait, moment, method) %>%
  filter(tally == max(tally)) %>%
  group_by(trait, moment, overunder) %>%
  mutate(x = dev/max(dev)) %>%
  mutate(x = ifelse(overunder == "under",
                    -1*x,
                    x))


over_under$moment <- factor(over_under$moment,
                            levels = c("mean",
                                       "variance",
                                       "skewness",
                                       "kurtosis"))

inset =
  ggplot(over_under) +
  geom_col(aes(y = x,
               x = method,
               fill = method),
           alpha = 0.5,
           show.legend = FALSE) +
  facet_grid(rows = vars(trait),
             cols = vars(moment),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y')  +
  geom_segment(aes(y = 0,
                   xend = 4.5,
                   x = 0.5, yend = 0),
               colour = 'grey69',
               size = 0.7) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(y = c(-5,5)) +
  expand_limits(x= c(-9, 11)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )


cowplot::ggdraw(doughnut_CO) +
  cowplot::draw_plot(inset,
                     width = 0.81,
                     height = 0.87,
                     x = 0.22,
                     y = 0.14) +
  cowplot::draw_image(
    img1, x = 0.05, y = 0.94, hjust = 0.5, vjust = 0.5,
    width = 0.08
  )

ggsave(here::here("figures/Figure_S6.png"),
       height = 180, width = 147,
       units = "mm", dpi = 600)
# Figure S7 ----
## a) data ----
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
            deviation = mean(abs(deviation)),
            avg_error = mean(abs((true_value-estimate)/true_value))*100)

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

## b) plots ----

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
      y = avg_error ,
      color = method),
      alpha = 0.5,
      se = FALSE,
      size = 0.5) +
    geom_point(aes(
      x = sample_size,
      y = avg_error,
      color = method
    ),
    size = 2,
    alpha = 0.9) +
    geom_moon(aes(
      x = sample_size,
      y = avg_error,
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
                        values = colorspace::lighten(pal_df$c, amount = 0.1),
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
         y = "Average percent error (%)",
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
      #strip.placement = 'outside',
      panel.background = element_rect(colour = colorspace::darken("#dddddd", 0.1),
                                      size = 0.4),
      plot.title.position = "panel",
      plot.title = element_text(margin = margin(0, 0, 10, 0),
                                size = rel(.7), face = "bold"
      ),
      legend.position = 'bottom',
      plot.margin = margin(2, 2, 2, 2)
    ) 
  
}

(plots[[2]] +
    labs(title = "A: Tadpoles") +
    inset_element(img2,
                  left = 0.02,
                  bottom = 0.87,
                  right = 0.11,
                  top = 1, 
                  align_to = 'full') + theme_void() + 
    plots[[3]] +
    labs(title = "B: Trees") +
    inset_element(img3,
                  left = 0.02,
                  bottom = 0.89,
                  right = 0.11,
                  top = 1, 
                  align_to = 'full') + theme_void())/
  (plots[[4]] +
     labs(title = "C: Rodents") +
     inset_element(img4,
                   left = 0.02,
                   bottom = 0.89,
                   right = 0.11,
                   top = 1, 
                   align_to = 'full') + theme_void() + 
     plots[[5]] +
     labs(title = "D: Plankton") +
     inset_element(img5,
                   left = 0.02,
                   bottom = 0.89,
                   right = 0.11,
                   top = 1, 
                   align_to = 'full') + theme_void()) +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = 'bottom')) 

ggsave(here::here("figures/Figure_S7.png"),
       height = 160, width = 180,
       units = "mm", dpi = 600)
# Figure S8 ----
## a) data ----
simdata =
  tidy_simdata(readRDS("output_data/simulation_results.RDS"))

simdata_rats <-
  tidy_simdata(readRDS("output_data/simulation_results_rodents.RDS"))

sim_moon_means =
  simdata %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(method, moment, sample_size, trait) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation)),
            avg_error = mean(abs((true_value-estimate)/true_value))*100) %>%
  filter(sample_size < 51)

sim_moon_means_AZ =
  simdata_rats %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(method, moment, sample_size, trait) %>%
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation)),
            avg_error = mean(abs((true_value-estimate)/true_value))*100) %>%
  filter(sample_size < 51) %>%
  mutate(trait = case_when(trait == "log10_weight" ~ "Weight"))
## b) plots ----
ggplot(sim_moon_means %>%
         filter(method == "Nonparametric BS")) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 0.3) +
  geom_smooth(data = sim_moon_means,
              aes(
                x = sample_size,
                y = avg_error,
                color = method,
                linetype = method),
              se = FALSE,
              size = 0.4,
              alpha = 0.5) +
  geom_point(aes(
    x = sample_size,
    y = avg_error,
    color = method
  ),
  size = 1,
  alpha = 0.9) +
  geom_moon(aes(
    x = sample_size,
    y = avg_error,
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
       y = "Average percent error (%)",
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
       y = "Average percent error (%)",
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

ggsave(here::here("figures/Figure_S8.png"),
       height = 90, width = 150,
       units = "mm", dpi = 600)
# Figure S9 ----
## a) data ----
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
                             na.rm = TRUE),
            avg_error = mean(abs((true_value-estimate)/true_value))*100)

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
## b) plots ----
moons_pa <-
  ggplot(sim_moon_means_panama) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = .3) +
  geom_smooth(
    data = sim_moon_means_panama,
    aes(
      x = pct_abd_sampled,
      y = avg_error ,
      color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.5,
    linetype = 4) +
  geom_point(aes(
    x = pct_abd_sampled,
    y = avg_error,
    color = method
  ),
  size = 2,
  alpha = 0.9) +
  geom_moon(aes(
    x = pct_abd_sampled,
    y = avg_error,
    ratio = percentage,
    #right = right,
    fill = method
  ),
  color = "transparent",
  size = 2) +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.25),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.1),
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
       y = "Average percent error (%)") +
  # Theme
  theme_moon +
  theme(
    axis.ticks = element_line(size = 0.03),
    axis.text = element_text(size = rel(.5)),
    axis.title = element_text(size = rel(.6)),
    strip.text.y = element_text(margin = margin(0, 0, 3, 0),
                                size = rel(.7), face = "bold"),
    strip.text.x.top = element_text(margin = margin(0, 0, 3, 0),
                                    size = rel(.7), face = "bold"),
    panel.grid.major.y = element_line(size = 0.03),
    strip.background = element_blank(),
    axis.line = element_blank(),
    #strip.placement = 'outside',
    panel.background = element_rect(colour = colorspace::lighten("#141438", 0.1),
                                    size = 0.7),
    plot.title.position = "panel",
    legend.position = 'none',
    plot.margin = margin(7, 19, 10, 15),
    legend.key.size = unit(3, "mm")
    
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
               colour = 'grey69',
               size = 0.3) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(y = c(-1.3,10)) + 
  expand_limits(x= c(0, 22)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )

moons_pa +
  inset_element(inset_pa,
                left = 0,
                bottom = 0,
                right = 1,
                top = 1, 
                ignore_tag = TRUE) +
  inset_element(img3,
                left = 0,
                bottom = 0.9,
                right = 0.09,
                top = 1, 
                align_to = 'full') + theme_void()

ggsave(here::here("figures/Figure_S9.png"),
       height = 100, width = 160,
       units = "mm", dpi = 600)
# Figure S10 ----
## a) data ----
## b) plots ----
# Figure S11 ----
## a) data ----
cwm_methods =
  readRDS("output_data/CWM_methods_comparison.RDS") %>%
  mutate(ci_low = predict(lm(bootstrap_CWM~traditional_CWM), 
                          interval = "confidence",
                          level = 0.95)[,2],
         ci_high = predict(lm(bootstrap_CWM~traditional_CWM), 
                           interval = "confidence",
                           level = 0.95)[,3])

cwm_corr =
  readRDS("output_data/CWM_methods_comparison.RDS") %>%
  group_by(trait, moment) %>%
  summarise(corr = sprintf('%.2f',(summary(lm(bootstrap_CWM~traditional_CWM))$r.squared)),
            yint = round(lm(bootstrap_CWM~traditional_CWM)$coefficients[[1]],
                         digits = 2),
            grad = round(lm(bootstrap_CWM~traditional_CWM)$coefficients[[2]],
                         digits = 2))

cwm_CI = 
  readRDS("output_data/CWM_methods_comparison.RDS") %>%
  group_by(trait, moment) %>%
  mutate(ci_low = predict(lm(bootstrap_CWM~traditional_CWM), 
                          interval = "confidence",
                          level = 0.95)[,2],
         ci_high = predict(lm(bootstrap_CWM~traditional_CWM), 
                           interval = "confidence",
                           level = 0.95)[,3]) %>%
  group_by(trait, moment, traditional_CWM) %>%
  summarise(ci_high = mean(ci_high),
            ci_low  = mean(ci_low))

cwm_methods$moment <- factor(cwm_methods$moment,
                             levels = c("mean",
                                        "variance",
                                        "skewness",
                                        "kurtosis"))

cwm_corr$moment <- factor(cwm_corr$moment,
                          levels = c("mean",
                                     "variance",
                                     "skewness",
                                     "kurtosis"))

cwm_CI$moment <- factor(cwm_CI$moment,
                        levels = c("mean",
                                   "variance",
                                   "skewness",
                                   "kurtosis"))
## b) plots ----
cowplot::ggdraw(
  ggplot(cwm_methods) +
    geom_abline(aes(slope = 1, 
                    intercept = 0,
                    linetype = "1:1 line"),
                colour = "black") +
    geom_ribbon(data = cwm_CI,
                aes(x = traditional_CWM,
                    ymin = ci_low,
                    ymax = ci_high),
                alpha = 0.15) +
    geom_abline(data = cwm_corr,
                aes(slope = grad, 
                    intercept = yint,
                    linetype = "Regression slope"),
                colour = "grey69",
                alpha = 0.9) +
    geom_point(aes(x = traditional_CWM,
                   y = bootstrap_CWM),
               colour = "grey69",
               fill = pal_df$c[1],
               shape = 21,
               size = 2,
               alpha = 0.5) +
    geom_point(aes(x = traditional_CWM,
                   y = bootstrap_CWM),
               colour = "grey69",
               fill = NA,
               shape = 21,
               size = 2,
               alpha = 0.5) +
    geom_textbox(data = cwm_corr,
                 aes(x = min(cwm_methods$traditional_CWM),
                     y = max(cwm_methods$bootstrap_CWM),
                     label = paste0(glue::glue("R^2 = {corr} Slope = {grad}"))),
                 hjust = 0,
                 size = 1.6,
                 vjust = 1,
                 width = unit(0.27, "npc"),
                 box.padding = unit(c(2.7, 0.9, 2.3, 2.3), "pt")) +
    facet_grid(rows = vars(trait),
               cols = vars(moment),
               labeller = labeller(
                 trait = traits_parsed,
                 .default = capitalize
               ),
               scale = 'free',
               switch = 'y') +
    labs(x = "Traditional CW estimate",
         y = "Bootstrapped CW estimate") +
    scale_linetype_manual("",
                          values=c("1:1 line" = 2,
                                   "Regression slope" = 1),
                          guide = guide_legend(override.aes = list(colour = c("black","grey69")))) +
    coord_cartesian(clip = "off") +
    theme_moon +
    theme(axis.ticks.length=unit(.5, "mm"),
          axis.text = element_text(size = rel(.55)),
          legend.position = 'bottom',
          legend.text = element_text(size = rel(.7)),
          legend.key.size = unit(3, "mm")))  +
  cowplot::draw_image(
    img1, x = 0.03, y = 0.93, hjust = 0.5, vjust = 0.5,
    width = 0.045
  )

ggsave(here::here("figures/Figure_S11.png"),
       height = 110, width = 180,
       units = "mm", dpi = 600)


# Figure S12 ----
## a) data ----
bs_methods =
  tidy_simdata(readRDS("output_data/bootstrap_sample_size_and_method_sims.RDS")) 

moon_means =   
  bs_methods %>%
  filter(trait_sample_size %in% c(1,4,25,49,100,196,441)) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  filter(boot_sample_size %in% c(50, 100, 200, 400, 800, 1600)) %>%
  group_by(boot_sample_size, method, moment, trait_sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation)),
            avg_error = mean(abs((true_value-estimate)/true_value))*100)


moon_means$moment =
  ordered(moon_means$moment,levels = c("mean",
                                       "variance",
                                       "skewness",
                                       "kurtosis"))
## b) plots ----
plots <- vector('list', 6)
samp_size = c(50, 100, 200, 400, 800, 1600)

for (i in 1:length(samp_size)) {
  
  plots[[i]] = 
    ggplot(moon_means %>%
             filter(boot_sample_size == samp_size[i])) +
    geom_hline(aes(yintercept = 0),
               color = "grey69",
               size = .6) +
    geom_smooth(aes(
      x = trait_sample_size,
      y = avg_error ,
      color = method),
      alpha = 0.5,
      se = FALSE,
      size = 0.5) +
    geom_point(aes(
      x = trait_sample_size,
      y = avg_error,
      color = method
    ),
    size = 1.1,
    alpha = 0.9) +
    geom_moon(aes(
      x = trait_sample_size,
      y = avg_error,
      ratio = percentage,
      fill = method
    ),
    color = "transparent",
    size = 1.3) +
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
         y = "Average percent error (%)") +
    # Theme
    figure_theme +
    theme(
      axis.text = element_text(color = "#5e5e5e", size = rel(.4)),
      axis.title = element_text(color = "#5e5e5e", size = rel(.5)),
      legend.text = element_text(color = "#5e5e5e", size = rel(.5)),
      legend.title = element_text(color = "#5e5e5e", size = rel(.6)),
      strip.text.y = element_text(margin = margin(0, 0, 3, 0),
                                  size = rel(.6), face = "bold",
                                  color = "#5e5e5e"),
      strip.text.x.top = element_text(margin = margin(0, 0, 3, 0),
                                      size = rel(.6),
                                      color = "#5e5e5e", face = "bold"),
      panel.grid.major.y = element_line(size = 0.03,
                                        color = "#5e5e5e"),
      strip.background = element_blank(),
      axis.line = element_blank(),
      panel.background = element_rect(colour = colorspace::darken("#dddddd", 0.1),
                                      size = 0.5),
      plot.title.position = "panel",
      plot.title = element_text(margin = margin(0, 0, 10, 0),
                                size = rel(.5), face = "bold",
                                color = "#5e5e5e"),
      legend.position = 'bottom',
      plot.margin = margin(2, 2, 2, 2),
      legend.key.size = unit(3, "mm"),
      axis.ticks = element_line(size = 0.03)
    ) 
  
}

(plots[[1]] +
    labs(title = "A: Bootstrap sample size: 50") +
    inset_element(img1,
                  left = 0.02,
                  bottom = 0.85,
                  right = 0.1,
                  top = 0.97, 
                  align_to = 'full', 
                  ignore_tag = TRUE) + theme_void() +
    plots[[2]] +
    labs(title = "B: Bootstrap sample size: 100")) /
  (plots[[3]] +
     labs(title = "C: Bootstrap sample size: 200") + 
     plots[[4]] +
     labs(title = "D: Bootstrap sample size: 400"))/
  (plots[[5]] +
     labs(title = "E: Bootstrap sample size: 800") + 
     plots[[6]] +
     labs(title = "F: Bootstrap sample size: 1 600")) +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = 'none')
  ) 

ggsave(here::here("figures/Figure_S12.png"),
       height = 190, width = 180,
       units = "mm", dpi = 600)

# Figure S13 ----
## a) data ----
bs_ci = 
  bs_methods %>%
  filter(trait_sample_size == 9) %>% 
  group_by(method, boot_sample_size, moment) %>%
  summarise(true_value = mean(true_value),
            estimate = mean(estimate),
            ci_high = mean(ci_high),
            ci_low = mean(ci_low)) %>%
  na.omit()
## b) plots ----
ggplot(bs_ci) +
  geom_ribbon(aes(x = boot_sample_size,
                  ymin = ci_low,
                  ymax = ci_high,
                  fill = method),
              alpha = 0.2) +
  geom_line(aes(x = boot_sample_size,
                y = true_value,
                linetype = "True value"),
            colour = 'grey30',
            size = 0.5) +
  geom_bump(aes(x = boot_sample_size,
                y = estimate,
                colour = method,
                linetype = "Mean estimate"),
            size = 0.6, smooth = 2)+
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_linetype_manual(values=c("True value" = 4,
                                 "Mean estimate" = 1),
                        guide = guide_legend(title = "Estimate",
                                             title.position="top",
                                             title.hjust = 0.5,
                                             override.aes = 
                                               list(colour = c("grey69","grey30")))) +
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
  guides(fill = 'none') +
  labs(x = "BS sample size",
       y = "Estimate") +
  # Theme
  theme_moon +
  theme(legend.key.size = unit(5, "mm"),
        axis.text = element_text(size = rel(.55)),
        legend.position = 'bottom') +
  inset_element(img1,
                left = 0.0,
                bottom = 0.89,
                right = 0.07,
                top = 0.99, 
                align_to = 'full', 
                ignore_tag = TRUE) + theme_void()

ggsave(here::here("figures/Figure_S13.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)


# Figure S14 ----
## a) data ----
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
## b) plots ----
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
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(method, measure, sample_size) %>%
  summarise(percentage = as.numeric(sum(hit - 1)/n()),
            deviation = mean(ifelse(abs(estimate) > abs(true_value),
                                    abs(estimate) - abs(true_value),
                                    abs(true_value) - abs(estimate)),
                             na.rm = TRUE),
            avg_error = mean(abs((true_value-estimate)/true_value))*100) %>%
  ungroup() %>%
  mutate(method = ordered(method,levels = c("Traditional CWM",
                                            "Cross-Site CW",
                                            "Site-Specific CW",
                                            "Nonparametric BS")))

moon_plots <-
  ggplot(sim_moon_means %>%
           filter(method != "Traditional CWM") %>%
           filter(sample_size %in% c(1,9,25,49,100,169,441))) +
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
                                                "Nonparametric BS"))) %>%
      group_by(method, measure, sample_size) %>%
      summarise(avg_error = mean(abs((true_value-estimate)/true_value))*100),
    aes(
      x = sample_size,
      y = avg_error ,
      color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.4,
    linetype = 4) +
  geom_point(aes(
    x = sample_size,
    y = avg_error,
    color = method
  ),
  size = 1,
  alpha = 0.9) +
  geom_moon(
    aes(
      x = sample_size,
      y = avg_error,
      ratio = percentage,
      fill = method
    ),
    color = "transparent",
    size = 1.4) +
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
       y = "Average percent error (%)") +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                     limits = c(0, 500)) +
  # Theme
  theme_moon +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_text(size = rel(.5)),
    axis.title = element_text(size = rel(.6)),
    legend.text = element_text(size = rel(.4)),
    legend.title = element_text(size = rel(.6)),
    strip.text.y = element_text(margin = margin(0, 0, 3, 0),
                                size = rel(.6), face = "bold"),
    strip.text.x.top = element_text(margin = margin(0, 0, 3, 0),
                                    size = rel(.6), face = "bold"),
    panel.grid.major.y = element_line(size = 0.03),
    strip.background = element_blank(),
    axis.line = element_blank(),
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
  lims(y = c(-1.3,10)) +
  scale_x_continuous(trans = 'sqrt', breaks = c(1,4,9,16,25, 36,49),
                     limits = c(1, 50)) + 
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
                left = 0.86,
                bottom = 0.57,
                right = 1,
                top = 0.7, 
                align_to = 'full') + theme_void()

ggsave(here::here("figures/Figure_S14.png"),
       height = 150, width = 150,
       units = "mm", dpi = 600)
# Figure S15 ----
## a) data ----
## b) plots ----
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
                   color = my_ranks,
                   linetype = "True value"),
               size = .3) +
    geom_smooth(
      aes(
        x = sample_size,
        y = estimate,
        color = my_ranks,
        linetype = "Estimated value"),
      alpha = 0.5,
      se = FALSE,
      size = 0.4) +
    facet_grid(cols = vars(method),
               rows = vars(measure),
               labeller = labeller(
                 trait = traits_parsed,
                 .default = capitalize
               ),
               switch = 'y',
               scales = 'free') +
    labs(x = "Sample size",
         y = "Value of metric") +
    scale_colour_manual(values=cc) +
    scale_fill_manual(values=cc)  +
    guides(colour = 'none',
           fill = 'none') +
    scale_linetype_manual("Metric type",
                          values = c("True value" = 1,
                                     "Estimated value" = 4),
                          guide = guide_legend(override.aes = list(colour = "grey69"))) +
    scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                       limits = c(0, 500)) +
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
      panel.background = element_rect(colour = colorspace::lighten("#141438", 0.1),
                                      size = 0.3),
      plot.title.position = "panel",
      plot.title = element_text(margin = margin(0, 0, 10, 0),
                                size = rel(.7), face = "bold"),
      legend.position = 'right',
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

ggsave(here::here("figures/Figure_S15.png"),
       height = 180, width = 120,
       units = "mm", dpi = 600)
