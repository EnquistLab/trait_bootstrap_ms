# Admin ----

#load script that determines plotting aesthetics
source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")
library(patchwork)

# Figure 4 ----
## a) read data ----
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
## b) Panel A ----

# organise data for plotting
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

#plot

doughnut_plot = 
  ggplot(sim_doughnuts_all) +
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
                    breaks = pal_df$l)

## c) Panel B ----

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
                                        size = 0.6),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none',
        strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.9)),
        plot.margin = margin(0, 2, 0, 0))


## d) Panel C ----

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
  # Theme
  figure_theme +
  theme(panel.background = element_rect(colour = colorspace::darken("#dddddd", 0.1),
                                        size = 0.6),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'none',
        strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.9)),
        plot.margin = margin(0, 2, 0, 0))


## e) Combine panels ----

(doughnut_plot +
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
     inset_element(img1,
                   left = 0,
                   bottom = 0.4,
                   right = 0.1,
                   top = 0.7,
                   align_to = 'full', 
                   ignore_tag = TRUE) + theme_void())/
  (lollipop_CO +
     labs(tag = "C") +
     inset_element(img1,
                   left = 0,
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

# Figure 5 ----
## a) Read Data ----

herbs =
  tidy_simdata(readRDS("output_data/simulation_results.RDS"))  %>%
  filter(sample_size == "9") %>%
  group_by(site, trait, moment, method) %>%
  slice_head(n = 1)

herbs_corr =
  herbs %>%
  group_by(moment, method) %>%
  summarise(corr = sprintf('%.2f',(summary(lm(estimate~true_value))$r.squared)),
            yint = round(lm(estimate~true_value)$coefficients[[1]],
                         digits = 2),
            grad = round(lm(estimate~true_value)$coefficients[[2]],
                         digits = 2))

herbs_lims =
  herbs %>%
  group_by(moment, method) %>%
  summarise(x_min = min(true_value),
            y_max = max(estimate)) %>%
  ungroup() %>%
  mutate(x_min = min(x_min)) %>%
  group_by(moment) %>%
  mutate(y_max = max(y_max))


## b) plot ----

ggplot(herbs) +
  geom_abline(aes(slope = 1,
                  intercept = 0,
                  linetype = "1:1 line"),
              colour = "black") +
  geom_abline(data = herbs_corr,
              aes(slope = grad, 
                  intercept = yint,
                  linetype = "Regression slope"),
              colour = "grey69",
              alpha = 0.9) +
  geom_point(aes(x = true_value,
                 y = estimate,
                 colour = method),
             alpha = 0.7,
             shape = 16) +
  geom_textbox(data = herbs_corr,
               aes(x = herbs_lims$x_min,
                   y = herbs_lims$y_max,
                   label = paste0(glue::glue("R^2 = {corr} Slope = {grad}"))),
               hjust = 0,
               size = 1.6,
               vjust = 1,
               width = unit(0.27, "npc"),
               box.padding = unit(c(2.7, 0.9, 2.3, 1.5), "pt")) +
  facet_grid(cols = vars(method),
             rows = vars(moment),
             labeller = labeller(
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free')  +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = pal_df$c,
                      breaks = pal_df$l) +
  scale_linetype_manual(values=c("1:1 line" = 2,
                                 "Regression slope" = 1),
                        guide = guide_legend(title = "Slope",
                                             title.position="top",
                                             override.aes = list(colour = c("black","grey69")))) +
  labs(x = "True value",
       y = "Estimated value") +
  figure_theme +
  theme(axis.ticks.length=unit(.5, "mm"),
        axis.text = element_text(size = rel(.55)),
        legend.position = 'bottom',
        legend.text = element_text(size = rel(.7)),
        legend.key.size = unit(3, "mm"),
        strip.text.y.right = element_text(margin = margin(0, 0, 10, 0),
                                          size = rel(.9), face = "bold"),
        strip.text.x.top = element_text(margin = margin(0, 0, 5, 0),
                                        size = rel(.9),
                                        face = "bold"),
        panel.grid.major.y = element_line(size = 0.05),
        strip.background = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(colour = colorspace::darken("#dddddd", 0.1),
                                        size = 0.6),
        plot.title.position = "panel",
        plot.title = element_text(margin = margin(0, 0, 10, 0),
                                  size = 15, face = "bold")) +
  inset_element(img1,
                left = 0.01,
                bottom = 0.89,
                right = 0.08,
                top = 1, 
                align_to = 'full') + theme_void()

ggsave(here::here("figures/Figure_5.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)

# Figure 6 ----
## a) read in data  ----

colorado_percent <- readRDS("output_data/Colorado_percent_community_sims.RDS")

colorado_percent$method = fct_recode(colorado_percent$method, `Cross-Site CW` = "Cross-Site CWM", 
                                     `Site-Specific CW` = "Site-Specific CWM",
                                     `Nonparametric BS` = 'Non-Parametric BS')

panama_percent <- readRDS("output_data/Panama_percent_community_sims.RDS")

panama_percent$method = fct_recode(panama_percent$method, `Cross-Site CW` = "Cross-Site CWM", 
                                   `Site-Specific CW` = "Site-Specific CWM",
                                   `Nonparametric BS` = 'Non-Parametric BS')

rodent_percent <- readRDS("output_data/Rodent_percent_community_sims.RDS")

rodent_percent$method = fct_recode(rodent_percent$method, `Cross-Site CW` = "Cross-Site CWM", 
                                   `Site-Specific CW` = "Site-Specific CWM",
                                   `Nonparametric BS` = 'Non-Parametric BS')


rodent_percent$method <- ordered(rodent_percent$method,
                                 levels = c("Cross-Site CW",
                                            "Site-Specific CW",
                                            "Parametric BS",
                                            "Nonparametric BS"))

colorado_percent$method <- ordered(colorado_percent$method,
                                   levels = c("Cross-Site CW","Site-Specific CW","Parametric BS","Nonparametric BS"))
panama_percent$method <- ordered(panama_percent$method,
                                 levels = c("Cross-Site CW","Site-Specific CW","Parametric BS","Nonparametric BS"))

colorado_percent$moment =
  ordered(colorado_percent$moment,levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))

rodent_percent$moment =
  ordered(rodent_percent$moment,levels = c("mean",
                                           "variance",
                                           "skewness",
                                           "kurtosis"))

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
  filter(sample_size %in% c(1,9,49,100,196,441)) %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  ungroup() %>%
  group_by(method, moment, sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation)),
            avg_error = mean(abs((true_value-estimate)/true_value))*100)

sim_biased_moon_means =
  simdata_biased %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(method, moment, sample_size) %>%
  #calculate proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation)),
            avg_error = mean(abs((true_value-estimate)/true_value))*100)

## b) inset plots  ----

moons_mini <-
  ggplot(sim_moon_means) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 0.3) +
  geom_smooth(data = sim_biased_moon_means,
              aes(
                x = sample_size,
                y = avg_error,
                color = method,
                linetype = "Size-biased"),
              se = FALSE,
              size = 0.4) +
  geom_smooth(aes(
    x = sample_size,
    y = avg_error ,
    color = method,
    linetype = "Random"),
    alpha = 0.5,
    se = FALSE,
    size = 0.2) +
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
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.1),
                      labels = pal_df$l) +
  scale_linetype_manual("Sampling",
                        values=c("Size-biased" = 4,
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
       y = "Average percent error (%)") +
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
  )

inset_mini <-
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
             switch = 'y') + 
  geom_segment(aes(y = 0,
                   xend = 7.5,
                   x = 0.5, yend = 0),
               colour = 'grey69',
               size = 0.2) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(y = c(-8,6)) + 
  expand_limits(x= c(-32, 7)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )



moons_AZ <-
  ggplot(sim_moon_means %>%
           filter(sample_size %in% c(1,9,25,49,100,169))) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 0.3) +
  geom_smooth(data = sim_biased_moon_means,
              aes(
                x = sample_size,
                y = avg_error,
                color = method,
                linetype = "Biased"),
              se = FALSE,
              size = 0.4) +
  geom_smooth(aes(
    x = sample_size,
    y = avg_error ,
    color = method,
    linetype = "Random"),
    alpha = 0.5,
    se = FALSE,
    size = 0.2) +
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
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.1),
                      labels = pal_df$l) +
  scale_linetype_manual("Sampling",
                        values=c("Size-biased" = 4,
                                 "Random" = 1),
                        guide = guide_legend(override.aes = list(colour = "grey69"))) +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200),
                     limits = c(0, 200)) +
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
  )

inset_AZ <-
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
             switch = 'y') + 
  geom_segment(aes(y = 0,
                   xend = 7.5,
                   x = 0.5, yend = 0),
               colour = 'grey69',
               size = 0.2) +
  scale_fill_manual(values = pal_df$c,
                    breaks = pal_df$l) +
  lims(y = c(-8,6)) + 
  expand_limits(x= c(-32, 7)) +
  # Theme
  theme_void() +
  theme(
    strip.text = element_blank()
  )


data <- vector('list', 2)
data[[1]] = colorado_percent
data[[2]] = rodent_percent
inset_plots <- vector('list', 2)
moon_plots <- vector('list', 2)

for (i in 1:2) {
  
  dat = data[[i]]
  
  overunders = 
    dat %>%
    filter(estimate != is.na(estimate)) %>%
    mutate(pct_abd_sampled = round(pct_abd_sampled, digits = -1),
           overunder = ifelse(true_value <= estimate,
                              "over",
                              "under"),
           deviation = ifelse(abs(estimate) > abs(true_value),
                              abs(estimate) - abs(true_value),
                              abs(true_value) - abs(estimate))) %>%
    group_by(moment, method, pct_abd_sampled, overunder) %>%
    summarise(dev = mean(abs(deviation), na.rm = TRUE),
              tally = n()) %>%
    group_by(moment, method, pct_abd_sampled) %>%
    filter(tally == max(tally)) %>%
    group_by(moment, method) %>%
    mutate(x = dev/max(dev)) %>%
    mutate(x = ifelse(overunder == "under",
                      -1*x,
                      x))
  
  sim_moon_means =
    dat %>%
    filter(estimate != is.na(estimate)) %>%
    #if true value falls in estimate's CI
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
  
  sim_moon_means$moment =
    ordered(sim_moon_means$moment,levels = c("mean",
                                             "variance",
                                             "skewness",
                                             "kurtosis"))
  
  overunders$moment =
    ordered(overunders$moment,levels = c("mean",
                                         "variance",
                                         "skewness",
                                         "kurtosis"))
  
  moon_plots[[i]] <-
    ggplot(sim_moon_means) +
    geom_hline(aes(yintercept = 0),
               color = "grey50",
               size = .3) +
    geom_smooth(
      data = dat %>%
        filter(estimate != is.na(estimate)) %>%
        mutate(deviation = ifelse(abs(estimate) > abs(true_value),
                                  abs(estimate) - abs(true_value),
                                  abs(true_value) - abs(estimate))) %>%
        group_by(method, moment, pct_abd_sampled) %>%
        mutate(avg_error = mean(abs((true_value-estimate)/true_value))*100),
      aes(
        x = pct_abd_sampled,
        y = avg_error,
        color = method),
      alpha = 0.5,
      se = FALSE,
      size = 0.4,
      linetype = 4) +
    geom_point(aes(
      x = pct_abd_sampled,
      y = avg_error,
      color = method
    ),
    size = 1,
    alpha = 0.9) +
    geom_moon(aes(
      x = pct_abd_sampled,
      y = avg_error,
      ratio = percentage,
      fill = method
    ),
    color = "transparent",
    size = 1.2) +
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
    )
  
  inset_plots[[i]] =
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
                     xend = 7.5,
                     x = 0.5, yend = 0),
                 colour = 'grey69',
                 size = 0.2) +
    scale_fill_manual(values = pal_df$c,
                      breaks = pal_df$l) +
    lims(y = c(-1.3,10)) + 
    expand_limits(x= c(0, 23)) +
    # Theme
    theme_void() +
    theme(
      strip.text = element_blank()
    )
  
}

## c) panels  ----

layout <- '
AB
CC
'

((moons_mini +
    labs(tag = "A") +
    theme(legend.position = 'right',
          plot.tag = element_text(size = rel(0.7), 
                                  face = "bold")) +
    inset_element(inset_mini,
                  left = 0,
                  bottom = 0,
                  right = 1,
                  top = 1, 
                  ignore_tag = TRUE) +
    inset_element(img1,
                  left = 0.05,
                  bottom = 0.9,
                  right = 0.1,
                  top = 1, 
                  align_to = 'full') + theme_void()) +
    (moons_AZ +
       labs(tag = "B") +
       theme(legend.position = 'right',
             plot.tag = element_text(size = rel(0.7), 
                                     face = "bold")) +
       inset_element(inset_AZ,
                     left = 0,
                     bottom = 0,
                     right = 1,
                     top = 1, 
                     ignore_tag = TRUE) +
       inset_element(img4,
                     left = 0.05,
                     bottom = 0.89,
                     right = 0.11,
                     top = 1, 
                     align_to = 'full') + theme_void())) +
  ((moon_plots[[1]] +
      labs(tag = "C") +
      theme(legend.position = 'none',
            plot.tag = element_text(size = rel(0.7), 
                                    face = "bold")) +
      inset_element(inset_plots[[1]],
                    left = 0,
                    bottom = 0,
                    right = 1,
                    top = 1, 
                    ignore_tag = TRUE) +
      inset_element(img1,
                    left = 0.05,
                    bottom = 0.9,
                    right = 0.1,
                    top = 1, 
                    align_to = 'full') + theme_void()) +
     (moon_plots[[2]]  +
        labs(tag = "D") +
        theme(legend.position = 'none',
              plot.tag = element_text(size = rel(0.7), 
                                      face = "bold")) +
        inset_element(inset_plots[[2]],
                      left = 0,
                      bottom = 0,
                      right = 1,
                      top = 1,
                      ignore_tag = TRUE) +
        inset_element(img4,
                      left = 0.05,
                      bottom = 0.89,
                      right = 0.11,
                      top = 1, 
                      align_to = 'full') + theme_void())) +
  plot_layout(guides = 'collect',
              # heights = c(1, 0.7),
              design = layout) +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = 'right')) +
  inset_element(moon_legend,
                left = 0.87,
                bottom = 0.43,
                right = 1,
                top = 0.7, 
                align_to = 'full') + theme_void()

ggsave(here::here("figures/Figure_6.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)
# Figure 7 ----