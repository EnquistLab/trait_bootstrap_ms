#Distribution figures - time


###########################################
#Load stuff in

library(BIEN)
library(e1071)
library(traitstrap)
library(devtools)
library(tidyr)
library(dplyr)
library(ggplot2)

source("r_functions/parametric_bs.R")
source("r_functions/draw_traits_tidy.R")
source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")

##TODO
simdata_plankton <- 
  tidy_simdata(readRDS("output_data/simulation_results_phyto_subset_scaled.RDS"))

plankton_means =
  simdata_plankton %>%
  filter(sample_size == 9) %>%
  group_by(method, moment, site) %>%
  summarise(deviation = mean(estimate))

cowplot::ggdraw(
  ggplot(simdata_plankton %>%
           filter(sample_size == 9)) +
    geom_smooth(aes(x = as.numeric(site),
                    y = true_value,
                    group = method),
                colour = 'grey69',
                se = FALSE,
                size = 0.5) +
    geom_smooth(aes(
      x = as.numeric(site),
      y = estimate ,
      color = method),
      alpha = 0.5,
      se = FALSE,
      size = 0.9) +
    coord_cartesian(clip = 'off') +
    scale_fill_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::darken(pal_df$c, amount = 0.2),
                      labels = pal_df$l) +
    scale_colour_manual(guide = guide_legend(title = "Method",
                                             title.position="top"),
                        values = colorspace::lighten(pal_df$c, amount = 0.2),
                        labels = pal_df$l) +
    facet_grid(rows = vars(moment),
               cols = vars(method),
               labeller = labeller(
                 trait = traits_parsed,
                 .default = capitalize
               ),
               switch = 'y',
               scales = 'free') +
    labs(x = "Time",
         y = "Estimate") +
    # Theme
    theme_moon +
    theme(legend.key.size = unit(5, "mm"),
          axis.text = element_text(size = rel(.55)))
)+
  cowplot::draw_image(
    img5, x = 0.04, y = 0.93, hjust = 0.5, vjust = 0.5,
    width = 0.06
  )

ggsave(here::here("figures/plankton_time.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)
