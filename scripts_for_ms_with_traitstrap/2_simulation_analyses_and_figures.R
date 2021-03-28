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


### Accuracy (creative) ----

#All traits

simmeans = 
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(trait, moment, site) %>%
  mutate(true_val = mean(true_value)) %>%
  group_by(trait, moment, method, site, true_val) %>%
  summarise(estimate = mean(estimate)) %>%
  filter(site == 'Road') %>%
  mutate(facet_lab = paste0(moment,"_",trait))

simdata_lollipop =
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  filter(site == 'Road') %>%
  mutate(facet_lab = paste0(moment,"_",trait)) #%>%
# group_by(trait, moment, method, site, sample_size) %>%
# slice_sample(n = 20)

#re-order to match moment 'numbers'
simmeans$moment <- factor(simmeans$moment,
                          levels = c("mean",
                                     "variance",
                                     "skewness",
                                     "kurtosis"))


simdata_lollipop$moment <- factor(simdata_lollipop$moment,
                                  levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))


#TODO clean labelling

ggplot(simmeans) + 
  geom_vline(aes(xintercept = true_val), 
             color = "grey50",
             size = 1) +
  geom_jitter(data = simdata_lollipop,
              aes(x = estimate, 
                  y = method, 
                  fill = method), 
              color = "grey85", 
              width = 0, height = 0.2, alpha = 0.3, shape = 21) +
  geom_segment(data = simmeans,
               aes(x = true_val, 
                   xend = estimate, 
                   y = method, 
                   yend = method), 
               color = "grey50", 
               size = 0.5) +
  geom_point(data = simmeans,
             aes(x = estimate, 
                 y = method),
             color = "grey50", size = 3) + 
  geom_point(data = simmeans,
             aes(x = estimate, 
                 y = method,
                 color = method), 
             size = 2) +
  facet_wrap(~trait + moment,
             #rows = vars(trait),
             labeller = labeller(
               .default = capitalize,
               trait = traits_parsed,
               .multi_line = FALSE
             ),
             ncol = 4,
             scales = "free_x",
             strip.position = 'top') +
  scale_fill_manual(guide = guide_legend(title = "Method"),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method"),
                      values = pal_df$c,
                      breaks = pal_df$l) +
  scale_size(guide = guide_legend(title = "Sample Size"),
             range = c(0.5, 2)) +
  labs(
    x = "Estimated Value",
    y = NULL
  ) +
  guides(size = 'none') +
  figure_theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.background = element_rect(fill = "#141438",
                                       colour = NA),
        legend.background = element_rect(fill = "#141438",
                                         colour = NA),
        panel.background = element_rect(fill = "#141438",
                                        colour = 'grey69'),
        strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                    size = 14, face = "bold",
                                    colour = "grey65"),
        strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                        size = 12, face = "bold",
                                        colour = "grey65"),
        panel.grid.major.y = element_line(size = 0.05,
                                          colour = "grey65"),
        legend.key = element_blank(),
        legend.text = element_text(colour = "grey65"),
        axis.title = element_text(colour = "grey65"),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.title = element_text(colour = "grey65"),
        legend.position = 'bottom')

ggsave(here::here("figures/Lollipops_All.png"),
       height = 9, width = 13,
       units = "in", dpi = 300)




### Accuracy but using absolutes ----

#All traits

simmeans = 
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(trait, moment, method) %>%
  summarise(estimate = mean(deviation))
  mutate(facet_lab = paste0(moment,"_",trait))

simdata_lollipop =
  simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(trait, moment, method, sample_size) %>%
  slice_sample(n = 20)

#re-order to match moment 'numbers'
simmeans$moment <- factor(simmeans$moment,
                          levels = c("mean",
                                     "variance",
                                     "skewness",
                                     "kurtosis"))


simdata_lollipop$moment <- factor(simdata_lollipop$moment,
                                  levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))


#TODO clean labelling

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
                  size = as.factor(sample_size),
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
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         override.aes = list(shape = 21)),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           override.aes = list(shape = 21)),
                      values = colorspace::darken(pal_df$c, 0.5),
                      breaks = pal_df$l) +
  scale_size_discrete(guide = guide_legend(title = "Sample Size"),
                      range = c(1, 2.4)) +
  scale_alpha_discrete(guide = guide_legend(title = "Value in CI",
                                            override.aes = list(shape = 16)),
                       range = c(0.2, 0.5)) +
  labs(
    x = "Deviation from true value",
    y = NULL
  ) +
  #guides(size = 'none') +
  figure_theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.background = element_rect(fill = "#141438",
                                       colour = NA),
        legend.background = element_rect(fill = "#141438",
                                         colour = NA),
        panel.background = element_rect(fill = "#141438",
                                        colour = 'grey69'),
        strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                    size = 14, face = "bold",
                                    colour = "grey65"),
        strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                        size = 12, face = "bold",
                                        colour = "grey65"),
        panel.grid.major.y = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(colour = "grey65"),
        axis.title = element_text(colour = "grey65"),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.title = element_text(colour = "grey65"),
        legend.position = 'bottom')

ggsave(here::here("figures/Lollipops_deviation.png"),
       height = 8, width = 13,
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
simmeans$moment <- factor(simmeans$moment,
                          levels = c("mean",
                                     "variance",
                                     "skewness",
                                     "kurtosis"))

simdata_lollipop$moment <- factor(simdata_lollipop$moment,
                                  levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))

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

#TODO clean labelling

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
                  size = as.factor(sample_size),
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
  scale_size_discrete(guide = guide_legend(title = "Sample Size"),
                      range = c(1, 2.4)) +
  scale_alpha_discrete(guide = guide_legend(title = "Value in CI",
                                            override.aes = list(shape = 16)),
                       range = c(0.2, 0.5)) +
  labs(
    x = "Deviation from true value",
    y = NULL
  ) +
  #guides(size = 'none') +
  figure_theme +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        plot.background = element_rect(fill = "#141438",
                                       colour = NA),
        legend.background = element_rect(fill = "#141438",
                                         colour = NA),
        panel.background = element_rect(fill = "#141438",
                                        colour = 'grey69'),
        strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                    size = 14, face = "bold",
                                    colour = "grey65"),
        strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                        size = 12, face = "bold",
                                        colour = "grey65"),
        panel.grid.major.y = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(colour = "grey65"),
        axis.title = element_text(colour = "grey65"),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.title = element_text(colour = "grey65"),
        legend.position = 'bottom')


ggsave(here::here("figures/Lollipops_Datsets.png"),
       lollipop_all,
       height = 8.5, width = 13,
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
  theme_void() +
  theme(
    legend.position = "right",
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
    plot.margin = margin(15, 15, 10, 15)
  )

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
  geom_col(aes(x = x,
               y = method,
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
  geom_segment(aes(x = 0,
                   yend = 4.5,
                   y = 0.5, xend = 0),
               colour = 'grey96',
               size = 0.7) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(x = c(-5,5)) + 
  expand_limits(y= c(-9, 11)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )


cowplot::ggdraw(doughnut_CO) +
  cowplot::draw_plot(inset,
                     width = 0.74,
                     height = 0.925,
                     x = 0.185,
                     y = 0.085)

ggsave(here::here("figures/WinnerDoughnuts.png"),
       height = 9.8, width = 10.4,
       units = "in", dpi = 300)


### Doughnut plots - across winners ----

simdata %>%
  filter(sample_size < 26 &
           sample_size > 8) %>%
  group_by(moment, sample_size, site, trait) %>%
  count()

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
                                         #nrow = 1,
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    breaks = pal_df$l) +
  # Theme
  theme_void() +
  theme(
    legend.position = "right",
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
                                     size = 12,
                                     vjust = 0),
    legend.text = element_text(colour = "grey65")
  )

img1 = png::readPNG("images/Colorado.png")
img2 = png::readPNG("images/Frogs.png")
img3 = png::readPNG("images/Panama.png")
img4 = png::readPNG("images/AZ.png")


  cowplot::ggdraw(doughnut) +
  cowplot::draw_image(
    img1, x = 0.08, y = 0.89, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.06
  ) +
  cowplot::draw_image(
    img2, x = 0.08, y = 0.64, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.07
  ) +
  cowplot::draw_image(
    img3, x = 0.08, y = 0.41, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.06
  ) +
  cowplot::draw_image(
    img4, x = 0.08, y = 0.145, hjust = 1, vjust = 1, halign = 1, valign = 1,
    width = 0.07
  ) +
  cowplot::draw_plot(inset,
                     width = 0.75,
                     height = 0.95,
                     x = 0.18,
                     y = 0.1)

ggsave(here::here("figures/WinnerDoughnuts_datasets_images.png"),
       height = 8.3, width = 10.4,
       units = "in", dpi = 300)

### Fig 2 panel ----

####table####

library(gt)

library(cowplot)

datasets = tibble(
  Dataset = factor(rep(c("CO", "PAN", "FROG", "AZ"),6),
                   levels = c("AZ","FROG","PAN","CO")),
  label = c(c("Colorado<br>Plants", "Texas<br>Tadpoles", "Panama<br>Trees", "Arizona<br>Rodents"),
            c(50, 122, 17, 1),
            c(5, 20, 24, 1),
            c(5, 8, 1, 1),
            c(2064, 322, 4341, 992),
            c("", 
              "Messier, McGill,<br> & Lechowicz, 2010", 
              "Ernest, Yenni,<br>Allington, & <br>Bledsoe, 2020", 
              "Rasmussen, <br>Rudolf, 2016")),
  x_spp = c(rep("Dataset", 4),
            rep("Species", 4),
            rep("Sites", 4),
            rep("Traits", 4),
            rep("Individuals", 4),
            rep("Citation", 4)),
  x = c(rep(1, 4),
        rep(1.7, 4),
        rep(2.2, 4),
        rep(2.7, 4),
        rep(3.2, 4),
        rep(4, 4)))

datasets$x_spp = factor(datasets$x_spp,
                        levels = c(
                          "Dataset",
                          "Number of species",
                          "Number of sites",
                          "Number of traits",
                          "Number of individuals",
                          "Citation"
                        ))

p = 
  ggplot(datasets) +
  geom_richtext(aes(x = x,
                    y = Dataset,
                    label = label),
                family = "Noto",
                colour = "grey69",
                fill = NA,
                label.color = NA) +
  theme_void() +
  scale_x_continuous(position = "top",
                     breaks = c(1, 1.7, 2.2, 2.7, 3.2, 4),
                     labels = c("Dataset",
                                "Species",
                                "Sites",
                                "Traits",
                                "Individuals",
                                "Citation")) + 
  expand_limits(x = c(0.5, 4.7)) +
  coord_cartesian(clip = 'off') +
  theme(axis.text.x = element_text(size = 12),
        legend.text = element_text(colour = "grey65"),
        text = element_text(family = "Noto", color = "grey65"))


pimage <- axis_canvas(p, axis = 'y') + 
  draw_image(img1, y = 3.5, scale = 0.5) +
  draw_image(img2, y = 2.5, scale = 0.5) +
  draw_image(img3, y = 1.5, scale = 0.5) +
  draw_image(img4, y = 0.5, scale = 0.5)

ggdraw(insert_yaxis_grob(p, pimage, position = "left"))

inset = 
  ggplot(over_under) +
  geom_col(aes(x = x,
               y = method,
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
  geom_segment(aes(x = 0,
                   yend = 4.5,
                   y = 0.5, xend = 0),
               colour = 'grey96',
               size = 0.7) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(x = c(-8,1)) + 
  expand_limits(y= c(-12, 8)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )


(ggdraw(insert_yaxis_grob(p, pimage, position = "left")) + doughnut +
    theme(legend.position = 'none',
          text = element_text(family = "Noto", color = "grey65")))/
  lollipop_all +
  theme(
    legend.position = 'right',
    strip.text.x = element_text(margin = margin(0, 0, 10, 0),
                                size = 16, face = "bold",
                                colour = "grey70"),
    strip.text.y.left = element_text(colour = "grey69",
                                     margin = margin(0, 10, 10, 10),
                                     angle = 0,
                                     size = 12,
                                     vjust = 0),
    legend.text = element_text(colour = "grey65")
  ) +
  plot_layout(guides = 'collect',
              widths = c(1, 1)) +
  plot_annotation(tag_levels = 'A',
                  theme = theme(
    plot.background = element_rect(fill = "#141438", colour = NA),
    panel.background = element_rect(fill = "#141438", colour = NA),
    text = element_text(family = "Noto", color = "grey65"))) 

ggsave(here::here("figures/Fig2_panel.png"),
       height = 12, width = 19,
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
  geom_col(aes(x = x,
               y = method,
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
  geom_segment(aes(x = 0,
                   yend = 4.5,
                   y = 0.5, xend = 0),
               colour = 'grey96',
               size = 0.7) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(x = c(-5,5)) + 
  expand_limits(y= c(-9, 11)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )

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
    legend.position = "right",
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
    legend.text = element_text(colour = "grey65")
  )

inset = 
  ggplot(over_under) +
  geom_col(aes(x = x,
               y = method,
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
  geom_segment(aes(x = 0,
                   yend = 4.5,
                   y = 0.5, xend = 0),
               colour = 'grey96',
               size = 0.7) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(x = c(-5,5)) + 
  expand_limits(y= c(-9, 11)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )


cowplot::ggdraw(doughnut) +
  cowplot::draw_plot(inset,
                     width = 0.7,
                     height = 0.95,
                     x = 0.24,
                     y = 0.05)

ggsave(here::here("figures/WinnerDoughnuts_panama.png"),
       height = 15, width = 10.4,
       units = "in", dpi = 300)
