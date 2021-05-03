#analyze and plot simulation results

source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")
#load function to clean data
source("r_functions/tidy_simdata.R")

library(ggtext)

#### Read Data ####


#read in data
cwm_methods =
  readRDS("output_data/CWM_methods_comparison.RDS") %>%
  mutate(ci_low = predict(lm(bootstrap_CWM~traditional_CWM), 
                          interval = "confidence",
                          level = 0.95)[,2],
         ci_high = predict(lm(bootstrap_CWM~traditional_CWM), 
                           interval = "confidence",
                           level = 0.95)[,3])

cwm_corr =
  readRDS("output_data/CWM_methods_comparison.RDS") %>%
  group_by(trait, moment) %>%
  summarise(corr = sprintf('%.2f',(summary(lm(bootstrap_CWM~traditional_CWM))$r.squared)),
            yint = round(lm(bootstrap_CWM~traditional_CWM)$coefficients[[1]],
                         digits = 2),
            grad = round(lm(bootstrap_CWM~traditional_CWM)$coefficients[[2]],
                         digits = 2))

cwm_CI = 
  readRDS("output_data/CWM_methods_comparison.RDS") %>%
  group_by(trait, moment) %>%
  mutate(ci_low = predict(lm(bootstrap_CWM~traditional_CWM), 
                          interval = "confidence",
                          level = 0.95)[,2],
         ci_high = predict(lm(bootstrap_CWM~traditional_CWM), 
                           interval = "confidence",
                           level = 0.95)[,3]) %>%
  group_by(trait, moment, traditional_CWM) %>%
  summarise(ci_high = mean(ci_high),
            ci_low  = mean(ci_low))

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

cwm_CI$moment <- factor(cwm_CI$moment,
                        levels = c("mean",
                                   "variance",
                                   "skewness",
                                   "kurtosis"))

#### Traditional CWM ####

cowplot::ggdraw(
  ggplot(cwm_methods) +
    geom_abline(aes(slope = 1, 
                    intercept = 0,
                    linetype = "1:1 line"),
                colour = "black") +
    geom_ribbon(data = cwm_CI,
                aes(x = traditional_CWM,
                    ymin = ci_low,
                    ymax = ci_high),
                alpha = 0.15) +
    geom_abline(data = cwm_corr,
                aes(slope = grad, 
                    intercept = yint,
                    linetype = "Regression slope"),
                colour = "grey69",
                alpha = 0.9) +
    geom_point(aes(x = traditional_CWM,
                   y = bootstrap_CWM),
               colour = "grey69",
               fill = pal_df$c[1],
               shape = 21,
               size = 2,
               alpha = 0.5) +
    geom_point(aes(x = traditional_CWM,
                   y = bootstrap_CWM),
               colour = "grey69",
               fill = NA,
               shape = 21,
               size = 2,
               alpha = 0.5) +
    geom_textbox(data = cwm_corr,
                 aes(x = min(cwm_methods$traditional_CWM),
                     y = max(cwm_methods$bootstrap_CWM),
                     label = paste0(glue::glue("R^2 = {corr} Slope = {grad}"))),
                 hjust = 0,
                 size = 1.6,
                 vjust = 1,
                 width = unit(0.27, "npc"),
                 box.padding = unit(c(2.7, 0.9, 2.3, 2.3), "pt")) +
    # facet_wrap(vars(trait, moment),
    #            labeller = labeller(
    #              trait = traits_parsed,
    #              .default = capitalize
    #            ),
    #            scale = 'free',
    #            switch = 'y') +
    facet_grid(rows = vars(trait),
               cols = vars(moment),
               labeller = labeller(
                 trait = traits_parsed,
                 .default = capitalize
               ),
               scale = 'free',
               switch = 'y') +
    labs(x = "Traditional CW estimate",
         y = "Bootstrapped CW estimate") +
    scale_linetype_manual("",
                          values=c("1:1 line" = 2,
                                   "Regression slope" = 1),
                          guide = guide_legend(override.aes = list(colour = c("black","grey69")))) +
    coord_cartesian(clip = "off") +
    theme_moon +
    theme(axis.ticks.length=unit(.5, "mm"),
          axis.text = element_text(size = rel(.55)),
          legend.position = 'bottom',
          legend.text = element_text(size = rel(.7)),
          legend.key.size = unit(3, "mm")))  +
  cowplot::draw_image(
    img1, x = 0.03, y = 0.93, hjust = 0.5, vjust = 0.5,
    width = 0.045
  )

ggsave(here::here("figures/Figure_SI_9.png"),
       height = 110, width = 180,
       units = "mm", dpi = 600)
ggsave(here::here("figures/pdf/Figure_SI_9.pdf"),
       height = 110, width = 180,
       units = "mm", dpi = 600)

#### Bootstrap sample sizes ####

bs_methods =
  tidy_simdata(readRDS("output_data/bootstrap_sample_size_and_method_sims.RDS")) 

moon_means =   
  bs_methods %>%
  filter(trait_sample_size %in% c(1,4,25,49,100,196,441)) %>%
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  filter(boot_sample_size %in% c(50, 100, 200, 400, 800, 1600)) %>%
  group_by(boot_sample_size, method, moment, trait_sample_size) %>%
  #calcualte proportion of 'hits' per trait, methods, moment
  summarise(percentage = sum(hit - 1)/n(),
            deviation = mean(abs(deviation)))


moon_means$moment =
  ordered(moon_means$moment,levels = c("mean",
                                       "variance",
                                       "skewness",
                                       "kurtosis"))

plots <- vector('list', 6)
samp_size = c(50, 100, 200, 400, 800, 1600)

for (i in 1:length(samp_size)) {
  
  plots[[i]] = 
    ggplot(moon_means %>%
             filter(boot_sample_size == samp_size[i])) +
    geom_hline(aes(yintercept = 0),
               color = "grey69",
               size = .6) +
    geom_smooth(aes(
      x = trait_sample_size,
      y = deviation ,
      color = method),
      alpha = 0.5,
      se = FALSE,
      size = 0.5) +
    geom_point(aes(
      x = trait_sample_size,
      y = deviation,
      color = method
    ),
    size = 1.1,
    alpha = 0.9) +
    geom_moon(aes(
      x = trait_sample_size,
      y = deviation,
      ratio = percentage,
      fill = method
    ),
    color = "transparent",
    size = 1.3) +
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
    facet_grid(rows = vars(moment),
               cols = vars(method),
               labeller = labeller(
                 trait = traits_parsed,
                 .default = capitalize
               ),
               switch = 'y',
               scales = 'free') +
    labs(x = "Sample Size",
         y = "Average deviation from true moment") +
    # Theme
    figure_theme +
    theme(
      axis.text = element_text(color = "#5e5e5e", size = rel(.4)),
      axis.title = element_text(color = "#5e5e5e", size = rel(.5)),
      legend.text = element_text(color = "#5e5e5e", size = rel(.5)),
      legend.title = element_text(color = "#5e5e5e", size = rel(.6)),
      strip.text.y = element_text(margin = margin(0, 0, 3, 0),
                                  size = rel(.6), face = "bold",
                                  color = "#5e5e5e"),
      strip.text.x.top = element_text(margin = margin(0, 0, 3, 0),
                                      size = rel(.6),
                                      color = "#5e5e5e", face = "bold"),
      panel.grid.major.y = element_line(size = 0.03,
                                        color = "#5e5e5e"),
      strip.background = element_blank(),
      axis.line = element_blank(),
      strip.placement = 'outside',
      panel.background = element_rect(colour = colorspace::darken("#dddddd", 0.1),
                                      size = 0.5),
      plot.title.position = "panel",
      plot.title = element_text(margin = margin(0, 0, 10, 0),
                                size = rel(.5), face = "bold",
                                color = "#5e5e5e"),
      legend.position = 'bottom',
      plot.margin = margin(2, 2, 2, 2),
      legend.key.size = unit(3, "mm"),
      axis.ticks = element_line(size = 0.03)
    ) 
  
}

(plots[[1]] +
    labs(title = "A: Bootstrap sample size: 50") +
    inset_element(img1,
                  left = 0.02,
                  bottom = 0.85,
                  right = 0.1,
                  top = 0.97, 
                  align_to = 'full', 
                  ignore_tag = TRUE) + theme_void() +
    plots[[2]] +
    labs(title = "B: Bootstrap sample size: 100")) /
  (plots[[3]] +
     labs(title = "C: Bootstrap sample size: 200") + 
     plots[[4]] +
     labs(title = "D: Bootstrap sample size: 400"))/
  (plots[[5]] +
     labs(title = "E: Bootstrap sample size: 800") + 
     plots[[6]] +
     labs(title = "F: Bootstrap sample size: 1 600")) +
  plot_layout(guides = 'collect') +
  plot_annotation(theme = theme(
                    plot.background = element_rect(fill = "white", colour = NA),
                    panel.background = element_rect(fill = "white", colour = NA),
                    legend.position = 'none')
  ) 

ggsave(here::here("figures/Figure_SI_10.png"),
       height = 190, width = 180,
       units = "mm", dpi = 600)
ggsave(here::here("figures/pdf/Figure_SI_10.pdf"),
       height = 190, width = 180,
       units = "mm", dpi = 600)

#### Bootstrap sample sizes with CI ####

bs_ci = 
  bs_methods %>%
  filter(trait_sample_size == 9) %>% 
  group_by(method, boot_sample_size, moment) %>%
  summarise(true_value = mean(true_value),
            estimate = mean(estimate),
            ci_high = mean(ci_high),
            ci_low = mean(ci_low)) %>%
  na.omit()

ggplot(bs_ci) +
  geom_ribbon(aes(x = boot_sample_size,
                  ymin = ci_low,
                  ymax = ci_high,
                  fill = method),
              alpha = 0.2) +
  geom_line(aes(x = boot_sample_size,
                y = true_value,
                linetype = "True value"),
            colour = 'grey30',
            size = 0.5) +
  geom_bump(aes(x = boot_sample_size,
                y = estimate,
                colour = method,
                linetype = "Mean estimate"),
            size = 0.6, smooth = 2)+
  coord_cartesian(clip = 'off') +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = colorspace::darken(pal_df$c, amount = 0.2),
                    labels = pal_df$l) +
  scale_linetype_manual(values=c("True value" = 4,
                                 "Mean estimate" = 1),
                        guide = guide_legend(title = "Estimate",
                                             title.position="top",
                                             title.hjust = 0.5,
                                             override.aes = 
                                               list(colour = c("grey69","grey30")))) +
  scale_colour_manual(guide = guide_legend(title = "Method",
                                           title.position="top",
                                           title.hjust = 0.5),
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
  guides(fill = 'none') +
  labs(x = "BS sample size",
       y = "Estimate") +
  # Theme
  theme_moon +
  theme(legend.key.size = unit(5, "mm"),
        axis.text = element_text(size = rel(.55)),
        legend.position = 'bottom') +
  inset_element(img1,
                left = 0.0,
                bottom = 0.89,
                right = 0.07,
                top = 0.99, 
                align_to = 'full', 
                ignore_tag = TRUE) + theme_void()

ggsave(here::here("figures/Figure_SI_11.png"),
       height = 120, width = 180,
       units = "mm", dpi = 600)
ggsave(here::here("figures/pdf/Figure_SI_11.pdf"),
       height = 120, width = 180,
       units = "mm", dpi = 600)
