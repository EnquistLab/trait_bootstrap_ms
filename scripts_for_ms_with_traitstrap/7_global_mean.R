
source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

library(e1071)

## Load data ----

global <- rbind(readRDS("output_data/local_intra_rmbl_moments_2010.rds") %>%
                  mutate(dataset = "Local intraspecific",
                         level = "local"),
                readRDS("output_data/local_mean_rmbl_moments_2010.rds") %>%
                  mutate(dataset = "Local community",
                         level = "local"),
                readRDS("output_data/BIEN_mean_rmbl_moments_2010.rds") %>%
                  mutate(dataset = "Global mean",
                         level = "global"),
                readRDS("output_data/BIEN_intra_rmbl_moments_2010.rds") %>%
                  mutate(dataset = "Global intraspecific",
                         level = "global")) %>%
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


##############################################################  

ggplot(global %>%
         filter(trait == "SLA")) +
  geom_line(aes(y = true_value,
                x = mean_elev),
            linetype = 2,
            colour = "grey69",
            size = 0.2) +
  geom_ribbon(aes(ymin = ci_low,
                  ymax = ci_high,
                  x = mean_elev,
                  fill = dataset),
              alpha = 0.2) +
  geom_line(aes(y = estimate,
                x = mean_elev,
                colour = dataset),
            size = 0.3) +
  geom_point(aes(y = estimate,
                 x = mean_elev,
                 colour = dataset,
                 shape = level),
             size = 1) +
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
  guides(colour = guide_legend(override.aes = list(shape = c(16, 16, 17, 17)),
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
    legend.text = element_text(size = rel(.4)),
    legend.title = element_text(size = rel(.5)),
    strip.text.y = element_text(margin = margin(0, 0, 3, 0),
                                size = rel(.5), face = "bold"),
    strip.text.x.top = element_text(margin = margin(0, 0, 3, 0),
                                    size = rel(.5), face = "bold"),
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
    legend.key.size = unit(3, "mm"),
    axis.ticks.length=unit(0.25, "mm")
  ) +
  #height
  ggplot(global %>%
           filter(trait == "height")) +
  geom_line(aes(y = true_value,
                x = mean_elev),
            linetype = 2,
            colour = "grey69",
            size = 0.2) +
  geom_ribbon(aes(ymin = ci_low,
                  ymax = ci_high,
                  x = mean_elev,
                  fill = dataset),
              alpha = 0.2) +
  geom_line(aes(y = estimate,
                x = mean_elev,
                colour = dataset),
            size = 0.3) +
  geom_point(aes(y = estimate,
                 x = mean_elev,
                 colour = dataset,
                 shape = level),
             size = 1) +
  facet_grid(rows = vars(moment),
             cols = vars(trait),
             scales = 'free',
             labeller = labeller(
               trait = traits_parsed,
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
  guides(colour = guide_legend(override.aes = list(shape = c(16, 16, 17, 17)),
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
    axis.title.x = element_text(size = rel(.5)),
    legend.text = element_text(size = rel(.4)),
    legend.title = element_text(size = rel(.5)),
    strip.text.x.top = element_text(margin = margin(0, 0, 3, 0),
                                    size = rel(.5), face = "bold"),
    panel.grid.major.y = element_line(size = 0.03),
    strip.background = element_blank(),
    axis.line = element_blank(),
    strip.placement = 'outside',
    panel.background = element_rect(colour = colorspace::lighten("#141438", 0.1),
                                    size = 0.3),
    plot.title.position = "panel",
    plot.title = element_text(margin = margin(0, 0, 10, 0),
                              size = rel(.7), face = "bold"),
    plot.margin = margin(2, 2, 2, 2),
    legend.key.size = unit(3, "mm"),
    axis.ticks.length=unit(0.25, "mm"),
    legend.position = 'bottom',
    strip.text.y = element_blank(),
    axis.title.y = element_blank()) +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = 'bottom'))

ggsave(here::here("figures/global_mean.png"),
       height = 120, width =180,
       units = "mm", dpi = 600)

#################################################        Mean
RegressionPlot_MeanRMBL_elev <- 
  moments  %>%
  ggplot(aes(x = elevation_m, y = mean, color = origen)) +
  geom_ribbon(aes(ymin = ci_low_mean, ymax = ci_high_mean, fill= factor(origen)), 
              linetype = 0,  alpha = 0.1,  show.legend = T) +
  geom_line(size = 0.8, show.legend = F)+
  geom_point(aes(fill = origen),
             shape = 21, size = 3, color="grey", alpha = 0.20, show.legend = T) +
  labs(x = " ", y = "Mean", colour = "") +
  facet_wrap( ~ traits, scales = "free_y") + 
  guides(color = FALSE) +
  
  theme(legend.position="right") +
  theme(axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 0, size = 8),
        axis.line = element_line(color = "grey"),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size =.1, color="grey"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'grey'),
        
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key=element_blank(),
        
        #strip.text.x = element_blank(), # remove everything (text included) from the facets
        strip.background = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold"))

RegressionPlot_MeanRMBL_elev

################################################# Variance

RegressionPlot_VarRMBL_elev <- moments  %>%
  ggplot(aes(x = elevation_m, y = var, color = origen)) +
  geom_ribbon(aes(ymin = ci_low_var, ymax = ci_high_var, fill= factor(origen)), 
              linetype = 0,  alpha = 0.1,  show.legend = T) +
  geom_line(size = 0.8, show.legend = F)+
  geom_point(aes(fill = origen),
             shape = 21, size = 3, color="grey", alpha = 0.20, show.legend = T) +
  labs(x = " ", y = "Variance", colour = "") +
  facet_wrap( ~ traits, scales = "free_y") + 
  guides(color = FALSE) +
  theme(axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 0, size = 8),
        axis.line = element_line(color = "grey"),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size =.1, color="grey"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'grey'),
        
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key=element_blank(),
        
        strip.background = element_blank(),
        strip.text.x = element_blank())

RegressionPlot_VarRMBL_elev 

################################################# Skewness

RegressionPlot_SkewRMBL_elev <- moments  %>%
  ggplot(aes(x = elevation_m, y = skew, color = origen)) +
  geom_ribbon(aes(ymin = ci_low_skew, ymax = ci_high_skew, fill= factor(origen)), 
              linetype = 0,  alpha = 0.1,  show.legend = T) +
  geom_line(size = 0.8, show.legend = F)+
  geom_point(aes(fill = origen),
             shape = 21, size = 3, color="grey", alpha = 0.20, show.legend = T) +
  labs(x = " ", y = "Skewness", colour = "") +
  facet_wrap( ~ traits, scales = "free_y") + 
  guides(color = FALSE) +
  theme(axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 0, size = 8),
        axis.line = element_line(color = "grey"),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size =.1, color="grey"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'grey'),
        
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key=element_blank(),
        
        strip.background = element_blank(),
        strip.text.x = element_blank())

RegressionPlot_SkewRMBL_elev 


################################################# Kurtosis

RegressionPlot_KurtRMBL_elev <- moments  %>%
  ggplot(aes(x = elevation_m, y = kurt, color = origen)) +
  geom_ribbon(aes(ymin = ci_low_kurt, ymax = ci_high_kurt, fill= factor(origen)), 
              linetype = 0,  alpha = 0.1,  show.legend = T) +
  geom_line(size = 0.8, show.legend = F)+
  geom_point(aes(fill = origen),
             shape = 21, size = 3, color="grey", alpha = 0.20, show.legend = T) +
  labs(x = "Elevation (m)", y = "Kurtosis", colour = "") +
  facet_wrap( ~ traits, scales = "free_y") + 
  guides(color = FALSE) +
  theme(axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 0, size = 8),
        axis.line = element_line(color = "grey"),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line( size =.1, color="grey"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'grey'),
        
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key = element_blank(),
        
        strip.background = element_blank(),
        strip.text.x = element_blank())

RegressionPlot_KurtRMBL_elev 

################################################################################

Maitner.plot <- ggarrange(RegressionPlot_MeanRMBL_elev, RegressionPlot_VarRMBL_elev, RegressionPlot_SkewRMBL_elev, RegressionPlot_KurtRMBL_elev,
                          ncol=1,
                          nrow=4, common.legend = TRUE, legend = "right")
## common.legend = TRUE) ### hacer la legenda
Maitner.plot

ggsave(Maitner.plot, file = "Maitner.plot.png", device = "png",
       width = 7, height = 9, dpi = 300)

