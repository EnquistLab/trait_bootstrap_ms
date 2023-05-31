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
## b) plots ----
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
## b) plots ----
# Figure S12 ----
## a) data ----
## b) plots ----
# Figure S13 ----
## a) data ----
## b) plots ----
# Figure S14 ----
## a) data ----
## b) plots ----
# Figure S15 ----
## a) data ----
## b) plots ----