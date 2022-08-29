# Plotting relative rank (somehow)

source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

#### Read Data ####

#read in data
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
                            TRUE ~ method)) %>%
  filter(true_measure == measure) %>%
  select(-c(true_measure, ci_low_measure, ci_high_measure)) %>%
  mutate(method = ordered(method,levels = c("Cross-Site CW",
                                            "Site-Specific CW",
                                            "Parametric BS",
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
         moment = ordered(measure,
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
                            TRUE ~ method)) %>%
  filter(true_measure == measure) %>%
  select(-c(true_measure, ci_low_measure, ci_high_measure)) %>%
  mutate(method = ordered(method,levels = c("Cross-Site CW",
                                            "Site-Specific CW",
                                            "Parametric BS",
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
                          levels = c('avg_uniqueness', 'avg_distinctiveness')))

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


#### 1:1 style plotting ####
# plotting true val vs estimated val

ggplot(herbs %>%
         filter(sample_size == 9)) +
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
               box.padding = unit(c(2.7, 0.9, 2.3, 2.3), "pt")) +
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
                        guide = guide_legend(title = " ",
                                             title.position="top",
                                             override.aes = list(colour = c("black","grey69")))) +
  labs(x = "True value",
       y = "Estimated value") +
  theme_moon +
  theme(axis.ticks.length=unit(.5, "mm"),
        axis.text = element_text(size = rel(.55)),
        legend.position = 'bottom',
        legend.text = element_text(size = rel(.7)),
        legend.key.size = unit(3, "mm")) +
  inset_element(img1,
                left = 0.01,
                bottom = 0.89,
                right = 0.08,
                top = 1, 
                align_to = 'full') + theme_void()

ggsave(here::here("figures/Rel_rank.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)
# ggsave(here::here("figures/pdf/Rel_rank.pdf"),
#        height = 120, width = 180,
#        units = "mm", dpi = 600)
