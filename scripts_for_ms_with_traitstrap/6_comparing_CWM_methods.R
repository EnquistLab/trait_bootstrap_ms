#analyze and plot simulation results

source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

library(ggtext)

#### Read Data ####


#read in data
cwm_methods =
  readRDS("output_data/CWM_methods_comparison.RDS")

cwm_corr =
  readRDS("output_data/CWM_methods_comparison.RDS") %>%
  group_by(trait, moment) %>%
  summarise(corr = sprintf('%.2f',(summary(lm(bootstrap_CWM~traditional_CWM))$r.squared)))

cwm_methods$moment <- factor(cwm_methods$moment,
                             levels = c("mean",
                                        "variance",
                                        "skewness",
                                        "kurtosis"))

cwm_corr$moment <- factor(cwm_corr$moment,
                             levels = c("mean",
                                        "variance",
                                        "skewness",
                                        "kurtosis"))

ggplot(cwm_methods) +
  geom_abline(aes(slope = 1, 
                  intercept = 0),
              colour = "grey69") +
  geom_point(aes(x = log10(abs(traditional_CWM)),
                 y = log10(abs(bootstrap_CWM))),
             colour = "grey69",
             fill = pal_df$c[1],
             shape = 21,
             size = 2) +
  geom_textbox(data = cwm_corr,
               aes(x = min(log10(abs(cwm_methods$traditional_CWM))),
                   y = max(log10(abs(cwm_methods$bootstrap_CWM))),
                   label = glue::glue("R^2 = {corr}")),
               hjust = 0,
               vjust = 1,
               width = unit(0.27, "npc"),
               family = "Noto") +
  facet_grid(rows = vars(trait),
             cols = vars(moment),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y') +
  labs(x = "Traditional CWM; log-transformed",
       y = "Bootstrapped CWM; log-transformed") +
  figure_theme +
  theme(axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
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
        legend.title = element_text(colour = "grey65"),
        legend.position = 'bottom')

ggsave(here::here("figures/CWM_comparison.png"),
       height = 9, width = 13,
       units = "in", dpi = 300)
