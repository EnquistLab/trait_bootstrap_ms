##GLOBAL VS LOCAL DATA##

source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

## Load data ----

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
            linetype = 4,
            colour = "grey69",
            size = 0.9) +
  geom_ribbon(data = global %>%
                filter(trait_source == "global"),
              aes(x = mean_elev,
                  ymin = ci_low,
                  ymax = ci_high,
                  fill = method),
              alpha = 0.1)+
  geom_ribbon(data = global %>%
                filter(trait_source == "local"),
              aes(x = mean_elev,
                  ymin = ci_low,
                  ymax = ci_high,
                  fill = method),
              alpha = 0.1) +
  geom_line(aes(x = mean_elev,
                y = estimate,
                colour = method, 
                linetype = trait_source),
            size = 0.6,
            alpha = 0.7) +
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
  scale_linetype_manual("Data source",
                        values=c(2, 1),
                        labels = c("Global", "Local"),
                        guide = guide_legend(override.aes = list(colour = "grey69"))) +
  theme_moon +
  inset_element(img1,
                left = 0.0,
                bottom = 0.9,
                right = 0.09,
                top = 0.99, 
                align_to = 'full', 
                ignore_tag = TRUE) + theme_void()

ggsave(here::here("figures/global_mean.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)
