
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
              # mutate(true_value = ifelse(trait == "SLA",
              #                            true_value/100,
              #                            true_value)) %>%
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




####

global = 
  readRDS("output_data/global_vs_local_summary.rds") %>%
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
                            TRUE ~ moment),
         method = case_when(method == 'global cwm' ~ 'Cross-Site CW',
                            method == 'site-specic CWM' ~ 'Site-Specific CW',
                            method == 'nonparametric bs' ~ 'Non-Parametric BS',
                            method == 'parametric bs' ~ 'Parametric BS',
                            TRUE ~ method)) %>%
  pivot_longer(cols = contains('true'),
               names_to = 'true_moment',
               values_to = 'true_value') %>%
  mutate(true_moment = str_to_lower(str_extract(true_moment,'[[:alpha:]]*$'))) %>%
  filter(true_moment == moment) %>%
  select(-c(true_moment)) %>%
  select(-c(global, ci_low_moment, ci_high_moment)) %>%
  mutate(method = ordered(method,levels = c("Cross-Site CW",
                                            "Site-Specific CW",
                                            "Parametric BS",
                                            "Non-Parametric BS"))) %>%
  left_join(.,
            readRDS("data/elevations.RDS")) %>%
  distinct(trait_source, method, site, trait, moment, n, estimate,
           ci_high, ci_low, true_value, mean_elev) %>%
  mutate(moment = ordered(moment,levels = c("mean",
                                            "variance",
                                            "skewness",
                                            "kurtosis")))



####


ggplot(global) +
  geom_line(aes(x = mean_elev,
                y = true_value),
            linetype = 2,
            colour = "grey69") +
  # geom_ribbon(aes(x = mean_elev,
  #                 ymin = ci_low,
  #                 ymax = ci_high,
  #                 fill = method,
  #                 group = trait_source),
  #             alpha = 0.2) +
  geom_line(aes(x = mean_elev,
                y = estimate,
                colour = method, 
                linetype = trait_source),
            size = 1.3) +
  facet_grid(rows = vars(moment),
             cols = vars(trait),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = pal_df$c,
                      labels = pal_df$l) +
  theme_moon +
  inset_element(img1,
                left = 0.0,
                bottom = 0.93,
                right = 0.07,
                top = 0.99, 
                align_to = 'full', 
                ignore_tag = TRUE) + theme_void()

ggsave(here::here("figures/global_mean.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)