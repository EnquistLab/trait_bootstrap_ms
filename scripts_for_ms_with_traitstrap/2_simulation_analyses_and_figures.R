#analyze and plot simulation results

#load script that determines plotting aesthetics
source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

#### 1 Read Data ####


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

simdata_plankton <- 
  tidy_simdata(readRDS("output_data/simulation_results_phyto_subset_scaled.RDS"))


#### 2 Summarise Data ####

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

#### 3 Figure 4 ####
### a) Panel A ----

# organise data for plotting

simdata  %>%
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
          mutate(dataset = rep("Rodents", nrow(.))),
        simdata_plankton %>%
          mutate(dataset = rep("Plankton", nrow(.))))  %>%
  distinct(dataset, moment, sample_size, site, trait) %>%
  group_by(dataset, moment) %>%
  count() %>%
  rename(grp_mn = n)

sim_doughnuts_all =
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

sim_doughnuts_all$dataset <- factor(sim_doughnuts_all$dataset,
                                    levels = c("Herbs",
                                               "Tadpoles",
                                               "Trees",
                                               "Rodents",
                                               "Plankton"))

sim_win_text$dataset <- factor(sim_win_text$dataset,
                               levels = c("Herbs",
                                          "Tadpoles",
                                          "Trees",
                                          "Rodents",
                                          "Plankton"))

### b) Panel B ----

# organise data

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
# create plot

sub_bump =
  ggplot(bumps %>%
           filter(sample_size < 50 &
                    dataset == "Herbs")) +
  with_blur(
    geom_bump(aes(x = sample_size,
                  y = -rank,
                  colour = method),
              size = 0.7, smooth = 8),
    sigma = 1) +
  geom_point(aes(x = sample_size,
                 y = -rank,
                 colour = method),
             size = 1.5) +
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
                     labels = c("Best", "Worst"),
                     limits = c(-4.25, -0.75)) +
  # Theme
  figure_theme +
  theme(panel.background = element_rect(colour = colorspace::darken("#dddddd", 0.1),
                                        linewidth = 0.6),
        strip.text.y = element_blank(),
        strip.text.x.top = element_text(margin = margin(0, 0, 5, 0),
                                        size = rel(1), face = "bold"),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        strip.placement = 'outside',
        axis.ticks.y = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.5)))


### c) Panel C ----

# organise data

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

#plot

lollipop_CO =
  ggplot(simmeans %>%
           filter(dataset == "Herbs")) +
  geom_vline(aes(xintercept = 0),
             color = "grey69",
             size = 0.6) +
  geom_segment(data = simmeans %>%
                 filter(dataset == "Herbs"),
               aes(x = 0,
                   xend = estimate,
                   y = method,
                   yend = method),
               color = "grey69",
               size = 0.3) +
  geom_point(data = simdata_lollipop %>%
               filter(dataset == "Herbs"),
             aes(x = deviation,
                 y = method,
                 fill = method,
                 alpha = hit),
             color = colorspace::lighten("#5e5e5e", 0.3),
             size = 1, stroke = 0.2,
             position = position_jitternormal(sd_x = 0, sd_y = 0.1), 
             shape = 21) +
  geom_point(data = simmeans %>%
               filter(dataset == "Herbs"),
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
                       range = c(0.5, 0.9)) +
  labs(
    x = "Deviation from true value",
    y = NULL
  ) +
  #guides(size = 'none') +
  theme_lollipop


### d) Combine panels ----

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
             fontface = 'bold',
             size = 5) +
   coord_polar(theta = 'y') +
   facet_grid(rows = vars(dataset),
              cols = vars(moment),
              labeller = labeller(
                trait = traits_parsed,
                .default = capitalize
              ),
              switch = 'y')  +
   scale_colour_manual(values = colorspace::darken(pal_df$c, 0.2),
                       breaks = pal_df$l)  +
   scale_fill_manual(guide = guide_legend(title = "Method",
                                          title.position="top",
                                          title.hjust = 0.5),
                     values = pal_df$c,
                     breaks = pal_df$l) +
   labs(tag = "A") +
   # Theme
   theme_doughnut +
   theme(strip.text.y.left = element_text(margin = margin(0, -1, 5, 0),
                                          size = rel(0.8), vjust = 0,
                                          angle = 0),
         strip.text.x.top = element_text(size = rel(1.3)),
         legend.position = 'top',
         legend.background = element_rect(colour = colorspace::darken("#dddddd", 0.1),
                                          size = 0.4),
         legend.margin = margin(3, 5, 3, 5),
         plot.margin = margin(5, 0, 7, 0)) +
   inset_element(img5,
                 left = 0.02,
                 bottom = 0.05,
                 right = 0.1,
                 top = 0.14, 
                 align_to = 'full', 
                 ignore_tag = TRUE) + theme_void() +
   inset_element(img4,
                 left = 0.02,
                 bottom = 0.23,
                 right = 0.1,
                 top = 0.32, 
                 align_to = 'full', 
                 ignore_tag = TRUE) + theme_void() +
   inset_element(img3,
                 left = 0.03,
                 bottom = 0.42,
                 right = 0.1,
                 top = 0.5, 
                 align_to = 'full', 
                 ignore_tag = TRUE) + theme_void() +
   inset_element(img2,
                 left = 0.02,
                 bottom = 0.6,
                 right = 0.1,
                 top = 0.67, 
                 align_to = 'full', 
                 ignore_tag = TRUE) + theme_void() +
   inset_element(img1,
                 left = 0.02,
                 bottom = 0.78,
                 right = 0.1,
                 top = 0.85, 
                 align_to = 'full', 
                 ignore_tag = TRUE) + theme_void()) /
  (sub_bump +
     labs(tag = "B") +
     theme(legend.position = 'none',
           strip.text.x.top = element_blank(),
           strip.text.y = element_blank(),
           axis.title.y = element_text(size = rel(0.9)),
           plot.margin = margin(0, 2, 0, 0)) +
     inset_element(img1,
                   left = 0.0,
                   bottom = 0.4,
                   right = 0.1,
                   top = 0.7,
                   align_to = 'full', 
                   ignore_tag = TRUE) + theme_void())/
  (lollipop_CO +
     labs(tag = "C") +
     theme(
       strip.text.y.left = element_blank(),
       strip.text.x.top = element_blank(),
       axis.title.y = element_text(size = rel(0.9)),
       legend.position = 'none',
       plot.margin = margin(0, 2, 0, 0)
     ) +
     inset_element(img1,
                   left = 0.0,
                   bottom = 0.4,
                   right = 0.1,
                   top = 0.7, 
                   align_to = 'full', 
                   ignore_tag = TRUE) + theme_void())  +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    text = element_text(face = 'bold'))) +
  plot_layout(heights = c(1, 0.15, 0.15))

ggsave(here::here("figures/Figure_4.png"),
       height = 310, width = 180,
       units = "mm", dpi = 600)


#### Other ####

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

ggsave(here::here("figures/Figure_SI_4.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)

### Accuracy Across datasets (creative) ----

#All traits

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


ggsave(here::here("figures/Figure_SI_3.png"),
       height = 115, width = 180,
       units = "mm", dpi = 600)
ggsave(here::here("figures/pdf/Figure_SI_3.pdf"),
       height = 115, width = 180,
       units = "mm", dpi = 600)


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

ggsave(here::here("figures/Figure_SI_6.png"),
       height = 180, width = 147,
       units = "mm", dpi = 600)

### Doughnut plots - across winners ----


### a) BumpPlots ----

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
        strip.placement = 'outside',
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

ggsave(here::here("figures/Figure_SI_2.png"),
       height = 130, width = 180,
       units = "mm", dpi = 600)


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

ggsave(here::here("figures/Figure_SI_5.png"),
       height = 314, width = 180,
       units = "mm", dpi = 600)
