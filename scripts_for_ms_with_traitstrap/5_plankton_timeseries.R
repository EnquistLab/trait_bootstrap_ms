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
source("r_functions/tidy_simdata.R")

##TODO
simdata_plankton <- 
  tidy_simdata(readRDS("output_data/simulation_results_phyto_subset_scaled.RDS"))

plankton_means =
  simdata_plankton %>%
  filter(sample_size == 9) %>%
  group_by(method, moment, site) %>%
  summarise(estimate = mean(estimate),
            ci_low = mean(ci_low),
            ci_high = mean(ci_high),
            true_value = mean(true_value))

cowplot::ggdraw(
  ggplot(plankton_means) +
    geom_bump(aes(x = as.numeric(site),
                  y = true_value,
                  group = method,
                  linetype = "True value"),
              colour = 'grey30',
              size = 0.4, smooth = 3) +
    geom_ribbon(aes(
      x = as.numeric(site),
      ymin = ci_low,
      ymax = ci_high,
      fill = method
    ),
    alpha = 0.2) +
    geom_bump(aes(
      x = as.numeric(site),
      y = estimate ,
      color = method,
      linetype = "Mean estimate"),
      alpha = 0.7,
      size = 0.5, smooth = 3) +
    coord_cartesian(clip = 'off') +
    scale_fill_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
                      values = colorspace::darken(pal_df$c, amount = 0.2),
                      labels = pal_df$l) +
    scale_colour_manual(guide = guide_legend(title = "Method",
                                             title.position="top",
                                             title.hjust = 0.5),
                        values = colorspace::lighten(pal_df$c, amount = 0.2),
                        labels = pal_df$l) +
    scale_linetype_manual(values=c("True value" = 4,
                                   "Mean estimate" = 1),
                          guide = guide_legend(title = "Estimate",
                                               title.position="top",
                                               title.hjust = 0.5,
                                               override.aes = 
                                                 list(colour = c("grey69","grey30")))) +
    facet_grid(rows = vars(moment),
               cols = vars(method),
               labeller = labeller(
                 trait = traits_parsed,
                 .default = capitalize
               ),
               switch = 'y',
               scales = 'free') +
    labs(x = "Day of year",
         y = "Estimate") +
    guides(fill = 'none') +
    # Theme
    theme_moon +
    theme(legend.key.size = unit(5, "mm"),
          axis.text = element_text(size = rel(.55)),
          legend.position = 'bottom')
)+
  cowplot::draw_image(
    img5, x = 0.03, y = 0.94, hjust = 0.5, vjust = 0.5,
    width = 0.06
  )

ggsave(here::here("figures/Figure_5.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)
ggsave(here::here("figures/pdf/Figure_5.pdf"),
       height = 120, width = 180,
       units = "mm", dpi = 600)

# Wave plots ----

plankton_dist = 
  readRDS("output_data/raw_plankton_distributions.RDS")



ggplot(plankton_dist %>%
         mutate(month = month(as.Date(day_of_year, format = "%j", origin = "1.1.2014"),
                              label = TRUE))) +
  stat_density_ridges(aes(x = value,
                          y = month),
                      colour = NA,
                      alpha = 0.4,
                      fill = "#f3b155",
                      scale = 2.5) +
  xlim(c(-2,7)) +
  theme_void()

# ggsave(here::here("figures/plankton_wave.png"),
#        height = 240, width = 180,
#        units = "mm", dpi = 600)
