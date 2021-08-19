# Plotting relative rank (somehow)

source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

#### Read Data ####

#read in data
herbs =
  tidy_simdata(readRDS("output_data/simulation_results.RDS"))


#### 1:1 style plotting ####
# plotting true val vs estimated val

ggplot(herbs %>%
         #filter(trait == "LMA_mg_mm2") %>%
         filter(sample_size == 9)) +
  geom_abline(aes(slope = 1,
                  intercept = 0),
              colour = "grey69",
              alpha = 0.9,
              linetype = 2) +
  geom_point(aes(x = true_value,
                 y = estimate,
                 colour = method),
             alpha = 0.7) +
  facet_grid(cols = vars(method),
             rows = vars(moment),
             labeller = labeller(
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free')  +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5,
                                           override.aes = list(shape = 21)),
                      values = pal_df$c,
                      breaks = pal_df$l) +
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
#        height = 100, width = 180,
#        units = "mm", dpi = 600)
