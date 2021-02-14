#analyze and plot simulation results


##############################################


#read in data
simdata <- readRDS("output_data/simulation_results.RDS")

#need to further tidy up data (add a column for moment)

  #could probably do this with pipes, but I'll go oldschool and cut down the googling

sim_mean <- simdata[c("site","trait","method","sample_size","n",
                       "mean","ci_low_mean","ci_high_mean","true_mean")]
sim_mean$moment <- "mean"
colnames(sim_mean)[grep(pattern = "ci_low",x = colnames(sim_mean))]<-"ci_low"
colnames(sim_mean)[grep(pattern = "ci_high",x = colnames(sim_mean))]<-"ci_high"
colnames(sim_mean)[grep(pattern = "true",x = colnames(sim_mean))]<-"true_value"
colnames(sim_mean)[grep(pattern = "mean",x = colnames(sim_mean))]<-"estimate"

sim_var <- simdata[c("site","trait","method","sample_size","n",
                      "var","ci_low_var","ci_high_var","true_variance")]
sim_var$moment <- "variance"
colnames(sim_var)[grep(pattern = "ci_low",x = colnames(sim_var))]<-"ci_low"
colnames(sim_var)[grep(pattern = "ci_high",x = colnames(sim_var))]<-"ci_high"
colnames(sim_var)[grep(pattern = "true",x = colnames(sim_var))]<-"true_value"
colnames(sim_var)[grep(pattern = "var",x = colnames(sim_var))]<-"estimate"


sim_skew <- simdata[c("site","trait","method","sample_size","n",
                      "skew","ci_low_skew","ci_high_skew","true_skewness")]
sim_skew$moment <- "skewness"
colnames(sim_skew)[grep(pattern = "ci_low",x = colnames(sim_skew))]<-"ci_low"
colnames(sim_skew)[grep(pattern = "ci_high",x = colnames(sim_skew))]<-"ci_high"
colnames(sim_skew)[grep(pattern = "true",x = colnames(sim_skew))]<-"true_value"
colnames(sim_skew)[grep(pattern = "skew",x = colnames(sim_skew))]<-"estimate"



sim_kurt <- simdata[c("site","trait","method","sample_size","n",
                      "kurt","ci_low_kurt","ci_high_Kurt","true_kurtosis")]
sim_kurt$moment <- "kurtosis"
colnames(sim_kurt)[grep(pattern = "ci_low",x = colnames(sim_kurt))]<-"ci_low"
colnames(sim_kurt)[grep(pattern = "ci_high",x = colnames(sim_kurt))]<-"ci_high"
colnames(sim_kurt)[grep(pattern = "true",x = colnames(sim_kurt))]<-"true_value"
colnames(sim_kurt)[grep(pattern = "kurt",x = colnames(sim_kurt))]<-"estimate"

simdata <- rbind(sim_mean,sim_var,sim_skew,sim_kurt)
rm(sim_mean,sim_var,sim_skew,sim_kurt)

#Rename methods for plotting
unique(simdata$method)
simdata$method[which(simdata$method=="global cwm")] <- "Cross-Site CWM"
simdata$method[which(simdata$method=="site-specic CWM")] <- "Site-Specific CWM"
simdata$method[which(simdata$method=="nonparametric bs")] <- "Nonparametrics BS"
simdata$method[which(simdata$method=="parametric bs")] <- "Parametrics BS"

simdata$method <- ordered(simdata$method,levels = c("Cross-Site CWM","Site-Specific CWM","Parametrics BS","Nonparametrics BS"))

library(ggplot2)


##############################################################
#Accuracy vs sample size

ggplot(data = simdata[which(simdata$moment=="mean"),], mapping = aes(y=(estimate-true_value)/true_value,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Mean")


ggplot(data = simdata[which(simdata$moment=="variance"),], mapping = aes(y=(estimate-true_value)/true_value,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Variance")

ggplot(data = simdata[which(simdata$moment=="skewness"),], mapping = aes(y=(estimate-true_value)/true_value,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Skewness")

ggplot(data = simdata[which(simdata$moment=="kurtosis"),], mapping = aes(y=(estimate-true_value)/true_value,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Kurtosis")


############################################################

source("r_functions/sumarize_sims.R")
sim_summary <- summarize_simulations(simulation_result = simdata)
colnames(sim_summary)

ggplot(data = sim_summary[which(sim_summary$moment=="mean"),], mapping = aes(y=pct_in_CI,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Mean")


ggplot(data = sim_summary[which(sim_summary$moment=="variance"),], mapping = aes(y=pct_in_CI,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Variance")

ggplot(data = sim_summary[which(sim_summary$moment=="skewness"),], mapping = aes(y=pct_in_CI,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Skewness")

ggplot(data = sim_summary[which(sim_summary$moment=="kurtosis"),], mapping = aes(y=pct_in_CI,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Kurtosis")


######

ggplot(data = sim_summary[which(sim_summary$moment=="mean"),], 
       mapping = aes(y=fractional_difference,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Mean")


ggplot(data = sim_summary[which(sim_summary$moment=="variance"),], 
       mapping = aes(y=fractional_difference,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Variance")

ggplot(data = sim_summary[which(sim_summary$moment=="skewness"),], 
       mapping = aes(y=fractional_difference,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Skewness")

ggplot(data = sim_summary[which(sim_summary$moment=="kurtosis"),], 
       mapping = aes(y=fractional_difference,x = sample_size^.5,color=method))+
  geom_abline(intercept=0,slope = 0)+geom_point()+geom_smooth()+
  facet_grid(rows = site~trait,scales = "free")+ggtitle("Kurtosis")


##### Balloon plots - accuracy of moments - 'global'

library(ggforce)
library(prismatic)

f = 0.7 #shape of baloons

colors <- c("Cross-Site CWM" = "#492259",
            "Site-Specific CWM" = "#D95284",
            "Non-parametric BS" = "#546FBF",
            "Parametric BS" = "#62F4B6",
            "True" = "#D98032")

# Join colors with categories
pal_df <- data.frame(c = unname(colors)[1:4], l = names(colors)[1:4])

#create balloons
simdata_shapes =
  simdata %>%
  #if true value falls in estimate's CI
  mutate(hit = ifelse(ci_low <= true_value & true_value <= ci_high,
                      2,
                      1)) %>%
  group_by(trait, method, moment) %>%
  #calcualte proportion of 'hits' per trait, methos, moment
  summarise(percentage = sum(hit - 1)/sum(hit)) %>%
  #filter(trait == 'height') %>%
  group_by(moment, trait) %>%
  mutate(category_nr = row_number()) %>% 
  ungroup()%>%
  rowwise() %>% 
  mutate(
    # Calculate points on circle for the "balloons", we need 4 x-y pairs for geom_bspline_closed
    # pi / length(pal_df$l) - creates the flower design by subdividing cricle
    x = list(c(0,
               f * percentage * sin(category_nr * 2 * pi / length(pal_df$l) - pi/4),
               percentage * sin(category_nr * 2 * pi / length(pal_df$l)), # real percentage for main "radius"
               f * percentage * sin(category_nr * 2 * pi / length(pal_df$l) + pi/5),
               0
    )),
    y = list(c(0,
               f * percentage * cos(category_nr * 2 * pi / length(pal_df$l) - pi/5),
               percentage * cos(category_nr * 2 * pi / length(pal_df$l)), # real percentage for main "radius"
               f * percentage * cos(category_nr * 2 * pi / 4 + pi/length(pal_df$l)),
               0
    ))
  ) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(moment, trait), names_from = category_nr, values_from = c(x, y)) %>%
  unnest(x_1:y_4)

#re-order to match moment 'numbers'
simdata_shapes$moment <- factor(simdata_shapes$moment, 
                                levels = c("mean",
                                           "variance",
                                           "skewness",
                                           "kurtosis"))
#rename traits
traits_parsed <- c(
  biomass_per_ind = "Biomass",
  dry_mass_mg = "Dry mass",
  height = "Height",
  leaf_area_mm2 = "Leaf area",
  LMA_mg_mm2 = "LMA"
)

#To capitalise labels
capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

# Plot
ggplot(simdata_shapes %>%
         mutate(combo = paste(moment, trait))) + 
  geom_point(aes(0, 0), size = 0.01, colour = "grey30")  +  # Make a "center"
  # Plot a "balloon" for every category
  geom_bspline_closed(aes(x_1, y_1, group = combo, fill = pal[1]), alpha = 0.7) +
  geom_bspline_closed(aes(x_2, y_2, group = combo, fill = pal[2]), alpha = 0.7) +
  geom_bspline_closed(aes(x_3, y_3, group = combo, fill = pal[3]), alpha = 0.7) +
  geom_bspline_closed(aes(x_4, y_4, group = combo, fill = pal[4]), alpha = 0.7) +
  scale_fill_identity(guide = guide_legend(title = "Method", 
                                           #nrow = 1, 
                                           override.aes = list(alpha = 0.7, shape = 2, size = 8),
                                           title.position="top", 
                                           title.hjust = 0.5), 
                      breaks = pal_df$c, 
                      labels = pal_df$l) +
  coord_fixed(ratio = 1) +
  facet_grid(col = vars(moment),
             row = vars(trait),
             labeller = labeller(
               trait = traits_parsed,
               .default = capitalize
             ),
             switch = 'y') +
  # Theme
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "grey97", 
                                   colour = NA),
    panel.background = element_rect(fill = "grey97", 
                                    colour = NA),
    strip.text.x = element_text(colour = "grey20",
                                margin = margin(0, 0, 10, 0),
                                size = 16),
    strip.text.y.left = element_text(colour = "grey20",
                                     margin = margin(0, 10, 10, 10),
                                     angle = 0,
                                     size = 16)
  )

ggsave(here::here("figures/CIballoons.png"), 
       height = 10, width = 7.6, 
       units = "in", dpi = 600)
