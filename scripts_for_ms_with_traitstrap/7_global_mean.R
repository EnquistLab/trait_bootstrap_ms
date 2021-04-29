
source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

library(e1071)

## Load data ----

global <- rbind(readRDS("output_data/local_intra_rmbl_moments_2010.rds") %>%
                  mutate(dataset = "Local BS",
                         level = "BS"),
                readRDS("output_data/local_mean_rmbl_moments_2010.rds") %>%
                  mutate(dataset = "Local CW",
                         level = "CW"),
                readRDS("output_data/BIEN_mean_rmbl_moments_2010.rds") %>%
                  mutate(dataset = "Global CW",
                         level = "CW"),
                readRDS("output_data/BIEN_intra_rmbl_moments_2010.rds") %>%
                  mutate(dataset = "Global BS",
                         level = "BS")) %>%
  pivot_longer(cols = c('mean', 'var', 'skew', 'kurt'),
               names_to = 'moment',
               values_to = 'estimate') %>%
  pivot_longer(cols = contains('ci_high'),
               names_to = 'ci_high_moment',
               values_to = 'ci_high') %>%
  mutate(ci_high_moment = str_to_lower(str_extract(ci_high_moment,'[[:alpha:]]*$'))) %>%
  filter(ci_high_moment == moment) %>%
  pivot_longer(cols = contains('ci_low'),
               names_to = 'ci_low_moment',
               values_to = 'ci_low') %>%
  mutate(ci_low_moment = str_to_lower(str_extract(ci_low_moment,'[[:alpha:]]*$'))) %>%
  filter(ci_low_moment == moment) %>%
  mutate(moment = case_when(moment == 'var' ~ 'variance',
                            moment == 'kurt' ~ 'kurtosis',
                            moment == 'skew' ~ 'skewness',
                            TRUE ~ moment)) %>%
  select(-c(global, ci_low_moment, ci_high_moment)) %>%
  left_join(.,
            readRDS("data/elevations.RDS")) %>%
  mutate(mean_elev = round(mean_elev),
         moment = ordered(moment,levels = c("mean",
                                            "variance",
                                            "skewness",
                                            "kurtosis")),
         traits = case_when(traits == "leaf_area_per_leaf_dry_mass" ~ "SLA",
                            traits == "whole_plant_height" ~ "height",
                            TRUE ~ traits)) %>%
  rename(trait = traits) %>%
  filter(trait %in% c("SLA", "height")) %>%
  left_join(.,
            ##HERE##
            readRDS(file = "data/all_traits_unscaled_RMBL.rds") %>%
              #pick desired traits
              select(site, SLA_m2_kg, height) %>%
              rename(SLA = SLA_m2_kg) %>%
              pivot_longer(cols = c(SLA, height),
                           names_to = 'trait',
                           values_to = 'true_value') %>%
              mutate(true_value = ifelse(trait == "SLA",
                                         true_value/100,
                                         true_value)) %>%
              group_by(site, trait) %>%
              summarise(mean = mean(true_value, na.rm = TRUE),
                        variance = var(true_value, na.rm = TRUE),
                        kurtosis = kurtosis(true_value, na.rm = TRUE),
                        skewness = skewness(true_value, na.rm = TRUE)) %>%
              pivot_longer(cols = c(mean, variance, kurtosis, skewness),
                           names_to = 'moment',
                           values_to = 'true_value') %>%
              mutate(moment = ordered(moment,levels = c("mean",
                                                        "variance",
                                                        "skewness",
                                                        "kurtosis"))))


###Line plots ----

global_plots <- vector('list', 2)
trait_select = c("SLA","height")

for (i in 1:2) {
  
  global_plots[[i]] <- 
    ggplot(global %>%
             filter(trait == trait_select[i])) +
    geom_line(aes(y = true_value,
                  x = mean_elev),
              linetype = 2,
              colour = "grey69",
              size = 0.4) +
    geom_ribbon(aes(ymin = ci_low,
                    ymax = ci_high,
                    x = mean_elev,
                    fill = dataset),
                alpha = 0.2) +
    geom_line(aes(y = estimate,
                  x = mean_elev,
                  colour = dataset),
              size = 0.3,
              alpha = 0.6,
              show.legend = FALSE) +
    geom_point(aes(y = estimate,
                   x = mean_elev,
                   colour = dataset,
                   shape = level),
               size = 1.3,
               alpha = 0.6) +
    facet_grid(rows = vars(moment),
               cols = vars(trait),
               scales = 'free',
               labeller = labeller(
                 .default = capitalize
               ),
               switch = 'y') +
    coord_cartesian(clip = "off") +
    scale_colour_manual(guide = guide_legend(title = "Data source",
                                             title.position = "top",
                                             title.hjust = 0.5),
                        values = c("#4BA698", "#61F2DC", "#2934A6", "#4958F2"))  +
    scale_fill_manual(guide = guide_legend(title = "Data source",
                                           title.position = "top",
                                           title.hjust = 0.5),
                      values = c("#4BA698", "#61F2DC", "#2934A6", "#4958F2")) +
    guides(colour = guide_legend(override.aes = list(shape = c(16, 17, 16, 17),
                                                     alpha = c(1,1,1,1),
                                                     size = c(1.7,1.7,1.7,1.7)),
                                 title = "Method",
                                 title.position="top",
                                 title.hjust = 0.5),
           shape = 'none',
           fill = 'none') +
    scale_x_continuous(breaks = c(2484, 2527, 3379, 3162, 2815),
                       limits = c(2483, 3380)) +
    labs(x = "Elevation",
         y = "Estimate") +
    theme_moon +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_text(size = rel(.3)),
      axis.title = element_text(size = rel(.5)),
      legend.text = element_text(size = rel(.6)),
      legend.title = element_text(size = rel(.5)),
      strip.text.y = element_text(margin = margin(0, 0, 3, 0),
                                  size = rel(.7), face = "bold"),
      strip.text.x.top = element_text(margin = margin(0, 0, 3, 0),
                                      size = rel(.7), face = "bold"),
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
      plot.margin = margin(2, 2, 2, 2),
      legend.key.size = unit(5, "mm"),
      axis.ticks.length=unit(0.25, "mm")
    )
  
}

global_plots[[1]] +
  global_plots[[2]] +
  theme(
    strip.text.y = element_blank(),
    axis.title.y = element_blank()) +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = 'bottom'))

ggsave(here::here("figures/global_mean.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)

