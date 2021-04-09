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

cowplot::ggdraw(
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
  scale_x_continuous(breaks = c(-1,0,1)) +
  theme_moon
  ) +
  cowplot::draw_image(
    img1, x = 0.03, y = 0.93, hjust = 0.5, vjust = 0.5,
    width = 0.045
  )

ggsave(here::here("figures/CWM_comparison.png"),
       height = 9, width = 14,
       units = "in", dpi = 300)

#### Bootstrap sample sizes ####

bs_methods =
  tidy_simdata(readRDS("output_data/bootstrap_sample_size_and_method_sims.RDS")) 

moon_means =   
  bs_methods %>%
  filter(trait_sample_size %in% c(1,4,25,49,100,196,441)) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  filter(boot_sample_size %in% c(200, 400, 800, 1600)) %>%
  group_by(boot_sample_size, method, moment, trait_sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation)))


moon_means$moment =
  ordered(moon_means$moment,levels = c("mean",
                                       "variance",
                                       "skewness",
                                       "kurtosis"))

#BSS + 200

p200 =
ggplot(moon_means %>%
         filter(boot_sample_size == 200)) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_smooth(aes(
    x = trait_sample_size,
    y = deviation ,
    color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  geom_point(aes(
    x = trait_sample_size,
    y = deviation,
    color = method
  ),
  size = 5,
  alpha = 0.9) +
  geom_moon(aes(
    x = trait_sample_size,
    y = deviation,
    ratio = percentage,
    fill = method
  ),
  color = "transparent",
  size = 5) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                     limits = c(0, 500)) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Trait sample size",
       y = "Average deviation from true moment",
       title = "A: Bootstrap sample size = 200") +
  #draw_key_moon(data.frame(x = 1:5, y = 0, ratio = 0:4 * 0.25))
  # Theme
  theme_moon

#BSS = 400

p400 =
  ggplot(moon_means %>%
           filter(boot_sample_size == 400)) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_smooth(aes(
    x = trait_sample_size,
    y = deviation ,
    color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  geom_point(aes(
    x = trait_sample_size,
    y = deviation,
    color = method
  ),
  size = 5,
  alpha = 0.9) +
  geom_moon(aes(
    x = trait_sample_size,
    y = deviation,
    ratio = percentage,
    fill = method
  ),
  color = "transparent",
  size = 5) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                     limits = c(0, 500)) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Trait sample size",
       y = "Average deviation from true moment",
       title = "B: Bootstrap sample size = 400") +
  #draw_key_moon(data.frame(x = 1:5, y = 0, ratio = 0:4 * 0.25))
  # Theme
  theme_moon

#BSS = 800

p800 =
  ggplot(moon_means %>%
           filter(boot_sample_size == 800)) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_smooth(aes(
    x = trait_sample_size,
    y = deviation ,
    color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  geom_point(aes(
    x = trait_sample_size,
    y = deviation,
    color = method
  ),
  size = 5,
  alpha = 0.9) +
  geom_moon(aes(
    x = trait_sample_size,
    y = deviation,
    ratio = percentage,
    fill = method
  ),
  color = "transparent",
  size = 5) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                     limits = c(0, 500)) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Trait sample size",
       y = "Average deviation from true moment",
       title = "C: Bootstrap sample size = 800") +
  theme_moon

#BSS = 1600

p1600 =
  ggplot(moon_means %>%
           filter(boot_sample_size == 1600)) +
  geom_hline(aes(yintercept = 0),
             color = "grey50",
             size = 1.5) +
  geom_smooth(aes(
    x = trait_sample_size,
    y = deviation ,
    color = method),
    alpha = 0.5,
    se = FALSE,
    size = 0.8) +
  geom_point(aes(
    x = trait_sample_size,
    y = deviation,
    color = method
  ),
  size = 5,
  alpha = 0.9) +
  geom_moon(aes(
    x = trait_sample_size,
    y = deviation,
    ratio = percentage,
    fill = method
  ),
  color = "transparent",
  size = 5) +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top"),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top"),
                      values = colorspace::lighten(pal_df$c, amount = 0.6),
                      labels = pal_df$l) +
  scale_x_continuous(trans = 'sqrt', breaks = c(0,10,50,100,200,500),
                     limits = c(0, 500)) +
  facet_grid(rows = vars(moment),
             cols = vars(method),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y',
             scales = 'free') +
  labs(x = "Trait sample size",
       y = "Average deviation from true moment",
       title = "D: Bootstrap sample size = 1 600") +
  # Theme
  theme_moon

(p200 + p400)/
  (p800 + p1600) +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
    plot.background = element_rect(fill = "#141438", colour = NA),
    panel.background = element_rect(fill = "#141438", colour = NA))) 

ggsave(here::here("figures/bs_samplesize.png"),
       height = 15.5, width = 25,
       units = "in", dpi = 300)
