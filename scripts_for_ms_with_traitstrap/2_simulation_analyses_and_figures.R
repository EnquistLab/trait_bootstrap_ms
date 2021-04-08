#analyze and plot simulation results

source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

#### Read Data ####


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


### Accuracy but using absolutes ----

#All traits

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

ggplot(simmeans) + 
  geom_vline(aes(xintercept = 0), 
             color = "grey50",
             size = 1) +
  geom_segment(data = simmeans,
               aes(x = 0, 
                   xend = estimate, 
                   y = method, 
                   yend = method), 
               color = "grey50", 
               size = 0.5) +
  geom_jitter(data = simdata_lollipop,
              aes(x = deviation, 
                  y = method, 
                  fill = method,
                  alpha = hit), 
              color = "grey85", 
              width = 0, height = 0.2, shape = 21) +
  geom_point(data = simmeans,
             aes(x = estimate, 
                 y = method,
                 fill = method,
                 colour = method),
             shape = 23, size = 3.2) + 
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
  scale_alpha_discrete(range = c(0.2, 0.5)) +
  labs(x = "Deviation from true value",
       y = NULL) +
  guides(colour = guide_legend(title = "Method",
                               override.aes = list(shape = 21,
                                                   size = 3,
                                                   order = 1)), 
         fill = guide_legend(title = "Method",
                             override.aes = list(shape = 21,
                                                 size = 3,
                                                 order = 1)), 
         alpha = guide_legend(title = "Value in CI",
                              override.aes = list(shape = 16,
                                                  size = 3,
                                                  order = 2))) +
  theme_lollipop

ggsave(here::here("figures/Lollipops_deviation.png"),
       height = 8, width = 15,
       units = "in", dpi = 300)

### Accuracy Across datasets (creative) ----

#All traits

simmeans = 
  rbind(simdata %>%
          mutate(dataset = rep("Herbs", nrow(.))),
        simdata_frogs %>%
          mutate(dataset = rep("Tadpoles", nrow(.))),
        simdata_panama %>%
          mutate(dataset = rep("Trees", nrow(.))),
        simdata_rats %>%
          mutate(dataset = rep("Rodents", nrow(.)))) %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(dataset, moment) %>%
  mutate(true_val = mean(true_value)) %>%
  # group_by(dataset, moment) %>%
  # mutate(deviation = scale(deviation)) %>%
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
          mutate(dataset = rep("Rodents", nrow(.)))) %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  # group_by(dataset, moment) %>%
  # mutate(deviation = scale(deviation)) %>%
  filter(deviation < 10) %>%
  group_by(dataset, moment, method, sample_size) %>%
  slice_sample(n = 20)

#re-order to match moment 'numbers'

simdata_lollipop$dataset <- factor(simdata_lollipop$dataset,
                                   levels = c("Herbs",
                                              "Tadpoles",
                                              "Trees", 
                                              "Rodents"))

simmeans$dataset <- factor(simmeans$dataset,
                           levels = c("Herbs",
                                      "Tadpoles",
                                      "Trees", 
                                      "Rodents"))

lollipop_all = 
  ggplot(simmeans) + 
  geom_vline(aes(xintercept = 0), 
             color = "grey50",
             size = 1) +
  geom_segment(data = simmeans,
               aes(x = 0, 
                   xend = estimate, 
                   y = method, 
                   yend = method), 
               color = "grey50", 
               size = 0.5) +
  geom_jitter(data = simdata_lollipop,
              aes(x = deviation, 
                  y = method, 
                  fill = method,
                  alpha = hit), 
              color = "white", 
              width = 0, height = 0.2, shape = 21) +
  geom_point(data = simmeans,
             aes(x = estimate, 
                 y = method,
                 fill = method,
                 colour = method),
             shape = 23, size = 3.2) + 
  facet_grid(rows = vars(dataset),
             cols = vars(moment),
             labeller = labeller(
               .default = capitalize
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
  scale_alpha_discrete(guide = guide_legend(title = "Value in CI",
                                            override.aes = list(shape = 16,
                                                                size = 3)),
                       range = c(0.2, 0.5)) +
  labs(
    x = "Deviation from true value",
    y = NULL
  ) +
  #guides(size = 'none') +
  theme_lollipop

ggsave(here::here("figures/Lollipops_Datsets.png"),
       lollipop_all,
       height = 9, width = 15,
       units = "in", dpi = 300)


### Doughnut plots - winners ----

library(ggtext)

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

sim_radar$moment <- factor(sim_radar$moment,
                           levels = c("mean",
                                      "variance",
                                      "skewness",
                                      "kurtosis"))

sim_radar$method <- factor(sim_radar$method,
                           levels = c("Cross-Site CWM",
                                      "Site-Specific CWM",
                                      "Parametric BS", 
                                      "Non-Parametric BS"))

sim_win_text$moment <- factor(sim_win_text$moment,
                              levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))

sim_win_text$method <- factor(sim_win_text$method,
                              levels = c("Cross-Site CWM",
                                         "Site-Specific CWM",
                                         "Parametric BS", 
                                         "Non-Parametric BS"))

doughnut_CO = 
  ggplot(sim_radar) +
  geom_col(aes(
    x = 2,
    y = percentage,
    fill = method
  ),
  colour = 'grey96') +
  xlim(c(0.7, 2.5)) +
  ylim(c(0, 1))  +
  #annotation textboxes
  geom_text(data = sim_win_text,
            aes(x = 1.1,
                y = 0.25,
                colour = method,
                label = glue::glue("{percentage}%")),
            #label = glue::glue("{method} - {percentage}%")),
            #colour = 'grey90',
            hjust = 1,
            show.legend = FALSE,
            size = 4) +
  coord_polar(theta = 'y') +
  facet_grid(rows = vars(trait),
             cols = vars(moment),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y')  + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = pal_df$c,
                      breaks = pal_df$l) +
  # Theme
  theme_doughnut

over_under =
  simdata  %>%
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


over_under$method <- factor(over_under$method,
                            levels = c("Cross-Site CWM",
                                       "Site-Specific CWM",
                                       "Parametric BS", 
                                       "Non-Parametric BS"))

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
               colour = 'grey96',
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
                     width = 0.8,
                     height = 0.87,
                     x = 0.25,
                     y = 0.14)

ggsave(here::here("figures/WinnerDoughnuts.png"),
       height = 10.4, width = 8.95,
       units = "in", dpi = 300)

### Over Under - across winners ----

over_under =
  rbind(simdata %>%
          mutate(dataset = rep("Herbs", nrow(.))),
        simdata_frogs %>%
          mutate(dataset = rep("Tadpoles", nrow(.))),
        simdata_panama %>%
          mutate(dataset = rep("Trees", nrow(.))),
        simdata_rats %>%
          mutate(dataset = rep("Rodents", nrow(.)))) %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(dataset, moment, method, overunder) %>%
  summarise(dev = mean(abs(deviation)),
            tally = n()) %>%
  group_by(dataset, moment, method) %>%
  filter(tally == max(tally)) %>%
  group_by(dataset, moment, overunder) %>%
  mutate(x = dev/max(dev)) %>%
  mutate(x = ifelse(overunder == "under",
                    -1*x,
                    x))

over_under$method <- factor(over_under$method,
                            levels = c("Cross-Site CWM",
                                       "Site-Specific CWM",
                                       "Parametric BS", 
                                       "Non-Parametric BS"))

over_under$dataset <- factor(over_under$dataset,
                             levels = c("Herbs",
                                        "Tadpoles",
                                        "Trees", 
                                        "Rodents"))

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
  facet_grid(rows = vars(dataset),
             cols = vars(moment),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y')  + 
  geom_segment(aes(y = 0,
                   xend = 4.5,
                   x = 0.5, yend = 0),
               colour = 'grey96',
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


### Doughnut plots - across winners ----

simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(moment, sample_size, site, trait) %>%
  count() %>%
  ungroup()%>%
  distinct(moment, trait, n)

group_size = 
  rbind(simdata %>%
          mutate(dataset = rep("Herbs", nrow(.))),
        simdata_frogs %>%
          mutate(dataset = rep("Tadpoles", nrow(.))),
        simdata_panama %>%
          mutate(dataset = rep("Trees", nrow(.))),
        simdata_rats %>%
          mutate(dataset = rep("Rodents", nrow(.)))) %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  distinct(dataset, moment, sample_size, site, trait) %>%
  group_by(dataset, moment) %>%
  count() %>%
  rename(grp_mn = n)

abs(sim_doughnuts_all$deviation)

sim_doughnuts_all = 
  rbind(simdata %>%
          mutate(dataset = rep("Herbs", nrow(.))),
        simdata_frogs %>%
          mutate(dataset = rep("Tadpoles", nrow(.))),
        simdata_panama %>%
          mutate(dataset = rep("Trees", nrow(.))),
        simdata_rats %>%
          mutate(dataset = rep("Rodents", nrow(.)))) %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate))) %>%
  group_by(dataset, moment, sample_size, site, trait) %>%
  filter(hit == 2) %>%
  filter(deviation == min(deviation)) %>%
  group_by(dataset, method, moment) %>%
  count()  %>%
  left_join(.,
            group_size) %>%
  mutate(percentage = n/grp_mn)

sim_win_text =
  sim_doughnuts_all %>%
  group_by(moment, dataset) %>%
  filter(percentage == max(percentage)) %>%
  mutate(percentage = round(percentage*100))

sim_doughnuts_all$moment <- factor(sim_doughnuts_all$moment,
                                   levels = c("mean",
                                              "variance",
                                              "skewness",
                                              "kurtosis"))

sim_doughnuts_all$method <- factor(sim_doughnuts_all$method,
                                   levels = c("Cross-Site CWM",
                                              "Site-Specific CWM",
                                              "Parametric BS", 
                                              "Non-Parametric BS"))

sim_doughnuts_all$dataset <- factor(sim_doughnuts_all$dataset,
                                    levels = c("Herbs",
                                               "Tadpoles",
                                               "Trees", 
                                               "Rodents"))

sim_win_text$moment <- factor(sim_win_text$moment,
                              levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))

sim_win_text$method <- factor(sim_win_text$method,
                              levels = c("Cross-Site CWM",
                                         "Site-Specific CWM",
                                         "Parametric BS", 
                                         "Non-Parametric BS"))

sim_win_text$dataset <- factor(sim_win_text$dataset,
                               levels = c("Herbs",
                                          "Tadpoles",
                                          "Trees", 
                                          "Rodents"))

doughnut = 
  ggplot(sim_doughnuts_all) +
  geom_col(aes(
    x = 2,
    y = percentage,
    fill = method
  ),
  colour = 'grey96') +
  xlim(c(0.7, 2.5)) +
  ylim(c(0, 1)) +
  #annotation textboxes
  geom_text(data = sim_win_text,
            aes(x = 1,
                y = 0.26,
                colour = method,
                label = glue::glue("{percentage}%")),
            #label = glue::glue("{method} - {percentage}%")),
            #colour = 'grey90',
            hjust = 1,
            show.legend = FALSE,
            size = 4) +
  coord_polar(theta = 'y') +
  facet_grid(rows = vars(dataset),
             cols = vars(moment),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y')  + 
  scale_colour_manual(values = pal_df$c,
                      breaks = pal_df$l)  + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  # Theme
  theme_doughnut

cowplot::ggdraw(doughnut) +
  cowplot::draw_image(
    img1, x = 0.09, y = 0.89, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.07
  ) +
  cowplot::draw_image(
    img2, x = 0.1, y = 0.67, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.08
  ) +
  cowplot::draw_image(
    img3, x = 0.1, y = 0.44, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.07
  ) +
  cowplot::draw_image(
    img4, x = 0.1, y = 0.2, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.08
  ) +
  cowplot::draw_plot(inset,
                     width = 0.85,
                     height = 0.89,
                     x = 0.2,
                     y = 0.13)

ggsave(here::here("figures/WinnerDoughnuts_datasets_images.png"),
       height = 10, width = 10.2,
       units = "in", dpi = 300)

### a) BumpPlots ----

library(ggbump)
library(ggfx)

bumps = 
  rbind(simdata %>%
          mutate(dataset = rep("Herbs", nrow(.))),
        simdata_frogs %>%
          mutate(dataset = rep("Tadpoles", nrow(.))),
        simdata_panama %>%
          mutate(dataset = rep("Trees", nrow(.))),
        simdata_rats %>%
          mutate(dataset = rep("Rodents", nrow(.)))) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         deviation = ifelse(abs(estimate) > abs(true_value),
                            abs(estimate) - abs(true_value),
                            abs(true_value) - abs(estimate))) %>%
  # group_by(dataset, moment, sample_size, site, trait) %>%
  filter(hit == 2)  %>%
  group_by(dataset, method, moment, sample_size) %>%
  summarise(n = mean(deviation)) %>%
  group_by(dataset, moment, sample_size) %>%
  mutate(rank = rank(n)) %>%
  mutate(rank = ifelse(is.na(rank),
                       5,
                       rank))

bumps$dataset <- factor(bumps$dataset,
                        levels = c("Herbs",
                                   "Tadpoles",
                                   "Trees", 
                                   "Rodents"))

bumps$moment =
  ordered(bumps$moment,levels = c("mean",
                                  "variance",
                                  "skewness",
                                  "kurtosis"))
sub_bump = 
  ggplot(bumps %>%
           filter(sample_size < 50 &&
                  dataset == "Herbs")) +
  with_blur(
    geom_bump(aes(x = sample_size,
                  y = -rank,
                  colour = method),
              size = 1, smooth = 8),
    sigma = 1) +
  geom_point(aes(x = sample_size,
                 y = -rank,
                 colour = method),
             size = 2) +
  facet_grid(cols = vars(moment),
             rows = vars(dataset),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y') +
  scale_colour_manual(guide = guide_legend(title = "Method"),
                      values = pal_df$c,
                      labels = pal_df$l) +
  labs(x = 'Sample size',
       y = "") +
  scale_x_continuous(trans = 'sqrt', breaks = c(1,4,9,16,25, 36,49),
                     limits = c(1, 50)) +
  scale_y_continuous(breaks = c(-1,-4),
                   labels = c("Best", "Worst")) +
  # Theme
  figure_theme +
  theme(panel.background = element_rect(colour = colorspace::lighten("#141438", 0.1),
                                        size = 1),
        strip.text.y = element_blank(),
        strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                        size = rel(1.7),
                                        colour = "grey65", face = "bold"),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(color = "grey65", size = rel(1)),
        legend.title = element_text(color = "grey65", size = rel(1.5)))

bump =
  ggplot(bumps %>%
           filter(sample_size %in% c(1,9,49,100,196,441))) +
  with_blur(
    geom_bump(aes(x = sample_size,
                  y = rank,
                  colour = method),
              size = 1, smooth = 8),
    sigma = 1) +
  geom_point(aes(x = sample_size,
                 y = rank,
                 colour = method),
             size = 2) +
  facet_grid(cols = vars(moment),
             rows = vars(dataset),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y') +
  scale_colour_manual(guide = guide_legend(title = "Method"),
                      values = pal_df$c,
                      labels = pal_df$l) +
  labs(x = 'Sample size',
       y = "Rank") +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                     limits = c(0, 500)) +
  lims(y = c(4.5,.5)) +
  # Theme
  theme_lollipop

(bump +
    sub_bump) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A',
                  theme = theme(
                    legend.position = 'bottom',
                    plot.background = element_rect(fill = "#141438", colour = NA),
                    panel.background = element_rect(fill = "#141438", colour = NA),
                    text = element_text(family = "Noto", color = "grey65")))

ggsave(here::here("figures/bumps.png"),
       height = 8, width = 18,
       units = "in", dpi = 300)

### b) Lollipop CO ----

lollipop_CO = 
  ggplot(simmeans %>%
           filter(dataset == "Herbs")) + 
  geom_vline(aes(xintercept = 0), 
             color = "grey50",
             size = 1) +
  geom_segment(data = simmeans %>%
                 filter(dataset == "Herbs"),
               aes(x = 0, 
                   xend = estimate, 
                   y = method, 
                   yend = method), 
               color = "grey50", 
               size = 0.5) +
  geom_jitter(data = simdata_lollipop %>%
                filter(dataset == "Herbs"),
              aes(x = deviation, 
                  y = method, 
                  fill = method,
                  alpha = hit), 
              color = "white", 
              width = 0, height = 0.2, shape = 21) +
  geom_point(data = simmeans %>%
               filter(dataset == "Herbs"),
             aes(x = estimate, 
                 y = method,
                 fill = method,
                 colour = method),
             shape = 23, size = 3.2) + 
  facet_grid(rows = vars(dataset),
             cols = vars(moment),
             labeller = labeller(
               .default = capitalize
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
  scale_alpha_discrete(guide = guide_legend(title = "Value in CI",
                                            override.aes = list(shape = 16,
                                                                size = 3)),
                       range = c(0.2, 0.5)) +
  labs(
    x = "Deviation from true value",
    y = NULL
  ) +
  #guides(size = 'none') +
  theme_lollipop

### Fig 2 panel ----

(ggplot(sim_doughnuts_all) +
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
              size = 4) +
    coord_polar(theta = 'y') +
    facet_grid(rows = vars(dataset),
               cols = vars(moment),
               labeller = labeller(
                 trait = traits_parsed,
                 .default = capitalize
               ),
               switch = 'y')  + 
    scale_colour_manual(values = pal_df$c,
                        breaks = pal_df$l)  + 
    scale_fill_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = pal_df$c,
                      breaks = pal_df$l) +
    # Theme
    theme_doughnut +
    theme(
      legend.position = 'none',
      text = element_text(family = "Noto", color = "grey65"))
    ) /
    (sub_bump +
    theme(legend.position = 'none',
          text = element_text(family = "Noto", color = "grey65"),
          strip.text.y = element_blank(),
          axis.title.y = element_text(size = rel(0.9))))/
  lollipop_CO +
  theme(
    strip.text.y = element_blank(),
    axis.title.y = element_text(size = rel(0.9))
  )  +
  plot_annotation(tag_levels = 'A',
                  theme = theme(
                    plot.background = element_rect(fill = "#141438", colour = NA),
                    panel.background = element_rect(fill = "#141438", colour = NA),
                    text = element_text(family = "Noto", color = "grey65", 
                                        face = 'bold'))) +
  plot_layout(heights = c(1, 0.25, 0.25))

ggsave(here::here("figures/Fig2_panel.png"),
       height = 16, width = 13,
       units = "in", dpi = 300)

### Doughnut - Panama by trait ----


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

sim_radar$moment <- factor(sim_radar$moment,
                           levels = c("mean",
                                      "variance",
                                      "skewness",
                                      "kurtosis"))
sim_radar$trait <- factor(sim_radar$trait,
                          levels = c("Area",
                                     "Dry.weight",
                                     "Fresh.weight",
                                     "LDMC",
                                     "LMA",
                                     "LCC",
                                     "LNC",
                                     "N.C"))

sim_radar$method <- factor(sim_radar$method,
                           levels = c("Cross-Site CWM",
                                      "Site-Specific CWM",
                                      "Parametric BS", 
                                      "Non-Parametric BS"))

sim_win_text$moment <- factor(sim_win_text$moment,
                              levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))

sim_win_text$trait <- factor(sim_win_text$trait,
                             levels = c("Area",
                                        "Dry.weight",
                                        "Fresh.weight",
                                        "LDMC",
                                        "LMA",
                                        "LCC",
                                        "LNC",
                                        "N.C"))

sim_win_text$method <- factor(sim_win_text$method,
                              levels = c("Cross-Site CWM",
                                         "Site-Specific CWM",
                                         "Parametric BS", 
                                         "Non-Parametric BS"))

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

over_under$moment <- factor(over_under$moment,
                            levels = c("mean",
                                       "variance",
                                       "skewness",
                                       "kurtosis"))

over_under$trait <- factor(over_under$trait,
                           levels = c("Area",
                                      "Dry.weight",
                                      "Fresh.weight",
                                      "LDMC",
                                      "LMA",
                                      "LCC",
                                      "LNC",
                                      "N.C"))

over_under$method <- factor(over_under$method,
                            levels = c("Cross-Site CWM",
                                       "Site-Specific CWM",
                                       "Parametric BS", 
                                       "Non-Parametric BS"))


doughnut = 
  ggplot(sim_radar) +
  geom_col(aes(
    x = 2,
    y = percentage,
    fill = method
  ),
  colour = 'grey96') +
  xlim(c(0.7, 2.5)) +
  ylim(c(0, 1))  +
  #annotation textboxes
  geom_text(data = sim_win_text,
            aes(x = 1.1,
                y = 0.25,
                colour = method,
                label = glue::glue("{percentage}%")),
            #label = glue::glue("{method} - {percentage}%")),
            #colour = 'grey90',
            hjust = 1,
            show.legend = FALSE,
            size = 4) +
  coord_polar(theta = 'y') +
  facet_grid(rows = vars(trait),
             cols = vars(moment),
             labeller = labeller(
               trait = traits_panama,
               .default = capitalize
             ),
             switch = 'y')  + 
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = pal_df$c,
                      breaks = pal_df$l) +
  # Theme
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14,
                                colour = "grey65"),
    plot.background = element_rect(fill = "#141438",
                                   colour = NA),
    panel.background = element_rect(fill = "#141438",
                                    colour = NA),
    strip.text.x = element_text(margin = margin(0, 0, 10, 0),
                                size = 16, face = "bold",
                                colour = "grey70"),
    strip.text.y.left = element_text(colour = "grey69",
                                     margin = margin(0, 10, 10, 10),
                                     angle = 0,
                                     size = 16),
    legend.text = element_text(colour = "grey65"),
    plot.margin = margin(15, 19, 10, 15)
  )

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
               colour = 'grey96',
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
                     width = 0.75,
                     height = 0.92,
                     x = 0.285,
                     y = 0.09)

ggsave(here::here("figures/WinnerDoughnuts_panama.png"),
       height = 15, width = 9.2,
       units = "in", dpi = 300)
