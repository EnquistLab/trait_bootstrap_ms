#analyze and plot simulation results


##############################################

library(tidyverse)
source(source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R"))

#read in data
colorado_percent <- readRDS("output_data/Colorado_percent_community_sims.RDS")

panama_percent <- readRDS("output_data/Panama_percent_community_sims.RDS")

colorado_percent$method <- ordered(colorado_percent$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametric BS","Non-Parametric BS"))
panama_percent$method <- ordered(panama_percent$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametric BS","Non-Parametric BS"))

### Moon plots - accuracy of moments - 'global' ----

library(gggibbous)

#### All traits combined

sim_moon_means_colorado =
  colorado_percent %>%
  #if true value falls in estimate's CI
  filter(estimate != is.na(estimate)) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         pct_abd_sampled = round(pct_abd_sampled, digits = -1)) %>%
  group_by(method, moment, pct_abd_sampled) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/sum(hit),
            deviation = mean(ifelse(estimate > true_value,
                                    estimate - true_value,
                                    true_value - estimate),
                             na.rm = TRUE))

sim_moon_means_panama =
  panama_percent %>%
  #if true value falls in estimate's CI
  filter(estimate != is.na(estimate)) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1),
         pct_abd_sampled = round(pct_abd_sampled, digits = -1)) %>%
  group_by(method, moment, pct_abd_sampled) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/sum(hit),
            deviation = mean(ifelse(estimate > true_value,
                                    estimate - true_value,
                                    true_value - estimate),
                             na.rm = TRUE))


sim_moon_means_panama$moment =
  ordered(sim_moon_means_panama$moment,levels = c("mean","variance","skewness","kurtosis"))

sim_moon_means_colorado$moment =
  ordered(sim_moon_means_colorado$moment,levels = c("mean",
                                                    "variance",
                                                    "skewness",
                                                    "kurtosis"))

moons <-
  ggplot(sim_moon_means_colorado) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_hline(data = sim_moon_means %>%
               filter(sample_size == 9),
             aes(yintercept = deviation),
             color = "grey50",
             size = 0.5,
             linetype = 4) +
  geom_smooth(aes(
    x = pct_abd_sampled,
    y = deviation ,
    color = method,
    linetype = "Colorado"),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  geom_smooth(
    data = sim_moon_means_panama,
    aes(
      x = pct_abd_sampled,
      y = deviation ,
      color = method,
      linetype = "Panama"),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  ggblur::geom_point_blur(
    aes(
      x = pct_abd_sampled,
      y = deviation,
      color = method
    ),
    color = "transparent",
    size = 3) +
  geom_point(aes(
    x = pct_abd_sampled,
    y = deviation,
    color = method
  ),
  size = 3,
  alpha = 0.9) +
  geom_moon(aes(
    x = pct_abd_sampled,
    y = deviation,
    ratio = percentage,
    #right = right,
    fill = method
  ),
  color = "transparent",
  size = 3) +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           #nrow = 1,
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.5),
                      labels = pal_df$l) +  
  scale_linetype_manual("Sampling",
                        values=c("Panama" = 2,
                                 "Colorado" = 1),
                        guide = guide_legend(override.aes = list(colour = "grey69"))) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Percent abundance sampled",
       y = "Average deviation from true moment") +
  #draw_key_moon(data.frame(x = 1:5, y = 0, ratio = 0:4 * 0.25))
  # Theme
  figure_theme +
  theme(
    legend.position = 'right',
    legend.title = element_text(size = 14, colour = "grey65"),
    strip.text.y = element_text(margin = margin(0, 0, 10, 0),
                                size = 14, face = "bold",
                                colour = "grey65"),
    strip.text.x.top = element_text(margin = margin(0, 0, 10, 0),
                                    size = 14, face = "bold",
                                    colour = "grey65"),
    panel.grid.major.y = element_line(size = 0.05,
                                      colour = "grey65"),
    legend.key = element_blank(),
    legend.text = element_text(colour = "grey65"),
    axis.title = element_text(colour = "grey65"),
    strip.background = element_blank(),
    axis.line = element_blank(),
    strip.placement = 'outside'
  )

cowplot::ggdraw(moons) +
  cowplot::draw_plot(ggplot(data.frame(y = c(1,1.5,2,2.5),
                                       x = 0, ratio = 1:4 * 0.25),
                            aes(x = x, y = y)) +
                       geom_moon(aes(ratio = ratio), size = 5, fill = "grey69", colour = "grey69") +
                       geom_text(aes(x = x + 0.6,
                                     label = paste0(ratio*100,"%")),
                                 size = 2.6,
                                 colour = "grey65",
                                 family = "Noto") +
                       coord_fixed() +
                       ggtitle("Uuum") +
                       lims(y = c(0.5, 2.7), x = c(-1, 1.4)) +
                       theme_void() +
                       theme(plot.title = element_text(hjust = 0.5),
                             text = element_text(colour = "grey65",
                                                 family = "Noto")),
                     .80, .11,
                     0.2, .23)

ggsave(here::here("figures/moons_pct_abund.png"),
       height = 7.4, width = 12.5,
       units = "in", dpi = 300)

