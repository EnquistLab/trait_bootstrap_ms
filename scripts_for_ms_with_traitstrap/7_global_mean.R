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
                            method == 'nonparametric bs' ~ 'Nonparametric BS',
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
                                            "Nonparametric BS"))) %>%
  left_join(.,
            readRDS("data/elevations.RDS")) %>%
  distinct(trait_source, method, site, trait, moment, n, estimate,
           ci_high, ci_low, true_value, mean_elev) %>%
  mutate(moment = ordered(moment,levels = c("mean",
                                            "variance",
                                            "skewness",
                                            "kurtosis"))) %>%
  filter(method %in% c("Cross-Site CW",
                       "Nonparametric BS"))



####

trait_select = unique(global$trait)
global_plots = vector('list', 2)

for (i in 1:length(trait_select)) {
  
  global_plots[[i]] = 
    ggplot(global %>%
             filter(trait == trait_select[i])) +
    geom_line(aes(x = mean_elev,
                  y = true_value,
                  linetype = "True value"),
              colour = "grey30",
              size = 0.8) +
    geom_ribbon(data = global %>%
                  filter(trait == trait_select[i]) %>%
                  filter(trait_source == "global"),
                aes(x = mean_elev,
                    ymin = ci_low,
                    ymax = ci_high,
                    fill = method),
                alpha = 0.15,
                show.legend = FALSE)+
    geom_ribbon(data = global %>%
                  filter(trait == trait_select[i]) %>%
                  filter(trait_source == "local"),
                aes(x = mean_elev,
                    ymin = ci_low,
                    ymax = ci_high,
                    fill = method),
                alpha = 0.15,
                show.legend = FALSE) +
    geom_line(aes(x = mean_elev,
                  y = estimate,
                  colour = method, 
                  linetype = trait_source),
              size = 0.65) +
    facet_grid(rows = vars(moment),
               cols = vars(trait_source),
               labeller = labeller(
                 trait = traits_parsed,
                 .default = capitalize
               ),
               switch = 'y',
               scales = 'free') +
    scale_fill_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = c(pal_df$c[1], pal_df$c[4]),
                      labels = c(pal_df$l[1], pal_df$l[4])) +
    scale_colour_manual(guide = guide_legend(title = "Method",
                                             title.position="top",
                                             title.hjust = 0.5),
                        values = c(pal_df$c[1], pal_df$c[4]),
                        labels = c(pal_df$l[1], pal_df$l[4])) +
    scale_linetype_manual(values=c(2, 1, 4),
                          labels = c("Global", "Local", "True value"),
                          guide = guide_legend(title = "Data source",
                                               title.position="top",
                                               title.hjust = 0.5,
                                               override.aes = list(colour = c("grey69",
                                                                              "grey69",
                                                                              "grey30"),
                                                                   size = 0.5))) +
    labs(x = "Elevation (m)",
         y = "Estimate") +
    theme_moon +
    theme(
      legend.position = 'bottom',
      legend.key.size = unit(7, "mm"),
      axis.title = element_text(size = rel(.7)),
      plot.title = element_text(hjust = 0.5,
                                size = rel(0.8)),
      axis.text = element_text(size = rel(.5)),
      plot.margin = margin(7, 5, 5, 5),
      legend.margin = margin(0, 2, 1, 0)
    )
}



global_plots[[1]] +
  labs(title = "Height")  +
  inset_element(img1,
                left = 0.0,
                bottom = 0.9,
                right = 0.09,
                top = 0.99, 
                align_to = 'full', 
                ignore_tag = TRUE) + theme_void() +
  global_plots[[2]] +
    labs(title = "LMA") +
  theme(axis.title.y = element_blank(),
        strip.text.y = element_blank()) +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = 'bottom'))

ggsave(here::here("figures/Figure_4.png"),
       height = 150, width = 180,
       units = "mm", dpi = 600)
ggsave(here::here("figures/pdf/Figure_4.pdf"),
       height = 150, width = 180,
       units = "mm", dpi = 600)


# Vertical

(global_plots[[1]] +
  labs(title = "Height") +
    theme(legend.key.size = unit(5, "mm")) +
  inset_element(img1,
                left = 0.0,
                bottom = 0.88,
                right = 0.12,
                top = 0.99, 
                align_to = 'full', 
                ignore_tag = TRUE) + theme_void()) /
  (global_plots[[2]] +
  labs(title = "LMA")) +
  theme(legend.key.size = unit(5, "mm")) +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.position = 'bottom'))

ggsave(here::here("figures/Figure_4_vert.png"),
       height = 230, width = 125,
       units = "mm", dpi = 600)
ggsave(here::here("figures/pdf/Figure_4_vert.pdf"),
       height = 230, width = 125,
       units = "mm", dpi = 600)
