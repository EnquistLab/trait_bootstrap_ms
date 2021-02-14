#Distribution figures


###########################################
#Load stuff in

library(BIEN)
library(moments)
library(traitstrap)
library(devtools)
library(tidyr)
library(dplyr)
library(ggplot2)

source("r_functions/parametric_bs.R")
source("r_functions/draw_traits_tidy.R")
source("scripts_for_ms_with_traitstrap/plotting_aesthetics.R")


#Code commented out to keep track of origin of trait data
#traits<-read.csv("C:/Users/Brian/Desktop/current_projects/RMBL_traits/Trait_Data/rmbl_trait_data_master.csv")
#atraits<-traits[which(traits$source=="Amanda"),]
#rm(traits)
#Remove columns that are empty, no need to bother with them
#atraits <- atraits[,which(!is.na(atraits[1,]))]
#atraits$taxon<-gsub(pattern = "Artemesia", replacement = "Artemisia",x = atraits$taxon)#correct name error
#Prune trait data to needed columns
#atraits$SLA<-as.numeric(as.character(atraits$SLA))
#atraits<-atraits[c("site","taxon","leaf_area_mm2","dry_mass_mg","SLA_m2_kg","height","biomass_per_ind" )]
#saveRDS(object = atraits,file = "data/all_traits_unscaled_RMBL.rds")

atraits <- readRDS(file = "data/all_traits_unscaled_RMBL.rds")

#Removed 7% of records with any missing data
message(paste(round(nrow(na.omit(atraits))/nrow(atraits)*100),"percent complete data. Pruning incomplete rows."))
atraits <- na.omit(atraits)

#correct unit error
atraits$dry_mass_mg[which(atraits$dry_mass_mg==7000)] <- 7

#Add LMA and remove SLA
atraits$LMA_mg_mm2 <- atraits$dry_mass_mg/atraits$leaf_area_mm2
atraits <- atraits[grep(pattern = "SLA",x = colnames(atraits),invert = T)]

#Add individual ID column and covert to long form
atraits$ID <- 1:nrow(atraits)


#Convert to tidy/skinny/long form
atraits <- gather(data = atraits,key = "trait","value",3:7)

#Get community data
atraits %>% group_by(taxon,site) %>% summarise(across(ID,~(length(unique(.x))),.names = "abundance"),.groups="drop") -> community

#log transform
#ggplot(data = atraits,mapping = aes(x=value))+geom_histogram()+facet_wrap(trait~site,scales = "free")
atraits$value <- log10(atraits$value)
#ggplot(data = atraits,mapping = aes(x=value))+geom_histogram()+facet_wrap(trait~site,scales = "free")



#######################################################

#Make distribution plots

source("r_functions/get_distributions.R")
sample_size <- 10 #this is the number of leaf samples to take
n_distributions <- 1
sample_size_dist <- 200 #number of samples to draw from the distribution
elev<-read_rds(path = "data/elevations.RDS")



#First simulate a draw of the relevant sample size
traits_nt <- draw_traits_tidy(tidy_traits = atraits,sample_size =  sample_size)

#Get species mean traits
species_means_nt <- samples_to_means(tidy_traits = traits_nt,level = "taxon")
species_means_nt$site <- "ALL" #Note: adding a "dummy" site here so traitstrap will use global data but still return site-level moments

#Get species x site mean traits
species_site_means_nt <- samples_to_means(tidy_traits = traits_nt,level = "taxon_by_site")

#Trait imputation for distributions
imputed_full <-
  trait_impute(comm = community,
               traits = traits_nt,
               scale_hierarchy = "site",
               global = T,
               taxon_col = "taxon",
               trait_col = "trait",
               value_col = "value",
               abundance_col = "abundance")

#Trait imputation for species mean
imputed_species_mean <-
  trait_impute(comm = community,
               traits = species_means_nt,
               scale_hierarchy = "site",
               global = T,
               taxon_col = "taxon",
               trait_col = "trait",
               value_col = "value",
               abundance_col = "abundance")

#Trait imputation for species x site mean
imputed_species_x_site_mean <-
  trait_impute(comm = community,
               traits = species_site_means_nt,
               scale_hierarchy = "site",
               global = T,
               taxon_col = "taxon",
               trait_col = "trait",
               value_col = "value",
               abundance_col = "abundance")

#Get distributions

cwm_dist <- get_distributions(imputed_traits = imputed_species_mean,nrep = n_distributions,sample_size = sample_size_dist)
cwm_site_dist <- get_distributions(imputed_traits = imputed_species_x_site_mean,nrep = n_distributions,sample_size = sample_size_dist)
pbs_dist <- get_distributions_parametric(imputed_traits = imputed_full,distribution_type = "normal",nrep = n_distributions,samples_per_abundance = 4,sample_size = sample_size_dist)
np_dist <- get_distributions(imputed_traits = imputed_full,nrep = n_distributions,sample_size = sample_size_dist)





np_dist$method <- "Non-parametric BS"
pbs_dist$method <- "Parametric BS"
cwm_dist$method <- "Cross-Site CWM"
cwm_site_dist$method <- "Site-Specific CWM"

all_dists <- rbind(as_tibble(np_dist),as_tibble(pbs_dist),as_tibble(cwm_dist),as_tibble(cwm_site_dist))


atraits_dist <- atraits
colnames(atraits_dist)
atraits_dist$n <-"1"
atraits_dist$global <-"global"
atraits_dist$method <-"True"
atraits_dist <- atraits_dist[colnames(all_dists)]

all_dists <- rbind(as.data.frame(all_dists),as.data.frame(atraits_dist))

all_dists <- merge(x = all_dists,y = elev)
all_dists$mean_elev <-round(all_dists$mean_elev)
rm(atraits_dist)





ggplot(data = all_dists[which(all_dists$site=="Road"),],
       mapping = aes(x=value,color=method,fill=method,alpha=0.5)) +
  geom_density() +
  facet_wrap(~trait,scales = "free")

ggplot(data = all_dists[which(all_dists$trait=="height"),],
       mapping = aes(x=value,color=method,fill=method,alpha=0.5)) +
  geom_density() +
  facet_wrap(~site,scales = "free_y")

library(ggridges)

colors <- c("Cross-Site CWM" = "#492259",
            "Site-Specific CWM" = "#D95284",
            "True" = "#D98032",
            "Non-parametric BS" = "#546FBF",
            "Parametric BS" = "#62F4B6")



ggplot(all_dists[which(all_dists$trait=="height"),],
       aes(x = value,
           y = as.factor(round(mean_elev)),
           color = method,
           fill = method)) +
  geom_density_ridges(rel_min_height = 0.005,alpha=0.5) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.1, 0)) +
  theme_ridges() +
  scale_color_manual(values = colors) +
  scale_fill_manual(values=colors)

ggplot(all_dists[which(all_dists$trait=="biomass_per_ind"),], aes(x = value, y = as.factor(round(mean_elev)),color=method,fill=method)) +
  geom_density_ridges(rel_min_height = 0.005,alpha=0.5) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.1, 0)) +
  theme_ridges()

ggplot(all_dists[which(all_dists$trait=="dry_mass_mg"),], aes(x = value, y = as.factor(round(mean_elev)),color=method,fill=method)) +
  geom_density_ridges(rel_min_height = 0.005,alpha=0.5) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.1, 0)) +
  theme_ridges()

ggplot(all_dists[which(all_dists$trait=="leaf_area_mm2"),], aes(x = value, y = as.factor(paste(round(mean_elev),"m")),color=method,fill=method)) +
  geom_density_ridges(rel_min_height = 0.005,alpha=0.5) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.1, 0)) +
  theme_ridges()+xlab("Leaf Area")+ylab(NULL)

ggplot(all_dists[which(all_dists$trait=="LMA_mg_mm2"),], aes(x = value, y = as.factor(round(mean_elev)),color=method,fill=method)) +
  geom_density_ridges2(rel_min_height = 0.005,alpha=0.5) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.1, 0)) +
  theme_ridges()


ggplot(all_dists[which(all_dists$trait=="leaf_area_mm2"),],
       aes(x = value,
           y = factor(paste(mean_elev,"m")),
           fill=method,
           color=method,
           linetype = method,
           alpha= method)) +
  geom_density_ridges(rel_min_height = 0.005) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.1, 0)) +
  theme_ridges() +
  labs(x = "Leaf Area",
       y = NULL) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values=colors) +
  scale_linetype_manual(values=c(1,1,1,1,2)) +
  scale_alpha_manual(values=c(0.5,0.5,0.5,0.5,0.7)) +
  guides(alpha = 'none')

##Tanya's zone

### Joy (Ridge) Plots ----

all_dists$method <- factor(all_dists$method,
                           levels = c("True",
                                      "Cross-Site CWM",
                                      "Site-Specific CWM",
                                      "Non-parametric BS",
                                      "Parametric BS"))

ggplot() +
  scale_fill_manual(guide = guide_legend(title = "Method",
                                         #nrow = 1,
                                         override.aes = list(alpha = 0.7, shape = 2, size = 8),
                                         title.position="top",
                                         title.hjust = 0.5),
                    values = pal_df$c,
                    labels = pal_df$l) +
  stat_density_ridges(data = all_dists %>%
                        filter(trait == "leaf_area_mm2" &
                                 method == "True"),
                      aes(x = value,
                          y = factor(paste(mean_elev,"m"))),
                      rel_min_height = 0.003,
                      colour = NA,
                      scale = 1,
                      fill = unname(colors)[5],
                      alpha = 0.3) +
  stat_density_ridges(data = all_dists %>%
                        filter(trait == "leaf_area_mm2" &
                                 method != "True"),
                      aes(x = value,
                          y = factor(paste(mean_elev,"m")),
                          fill = method),
                      rel_min_height = 0.003,
                      colour = NA,
                      scale = 1,
                      alpha = 0.7) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.1, 0)) +
  theme_classic() +
  labs(x = "Leaf Area",
       y = NULL) +
  guides(alpha = 'none')  +
  figure_theme

ggsave(here::here("figures/densityestimate_joy.png"),
       height = 8.3, width = 15,
       units = "in", dpi = 600)


### Halfeye plots ----

library(tidybayes)
ggplot() +
  stat_interval(data = all_dists %>%
                  filter(trait == "leaf_area_mm2" &
                           method == "True"),
                aes(y = value, 
                    x = factor(paste(mean_elev,"m"))),
                .width = c(.1, .25, .5, .75, 1), 
                height = 5, show.legend = F) +
  rcartocolor::scale_color_carto_d(palette = "RedOr") + 
  stat_halfeye(data = all_dists %>%
                 filter(trait == "leaf_area_mm2" &
                          method == "True"),
               aes(y = value, 
                   x = factor(paste(mean_elev,"m"))), 
               .width = 0, fill = "#D98032", alpha = 0.2, height = 0.7, point_color = NA) + 
  stat_halfeye(data = all_dists %>%
                 filter(trait == "leaf_area_mm2" &
                          method != "True"),
               aes(y = value, 
                   x = factor(paste(mean_elev,"m")),
                   group = method,
                   fill = method), 
               .width = 0, 
               alpha = 0.7, height = 0.7,
               point_color = NA) +
  scale_fill_manual(values  = pal_df$c,
                    labels = pal_df$l) +
  scale_discrete_manual("point_color",
                        values  = pal_df$c,
                        labels = pal_df$l) +
  coord_flip() +
  labs(x = "", y = "Leaf Area") +
  theme_classic()  +
  figure_theme

ggsave(here::here("figures/densityestimates.png"),
       height = 8.3, width = 15,
       units = "in", dpi = 600)

#For some reason grouping by elevation will place the plots on a nice elevation scale, but breaks the coloring.  Not sure if there is a fix for this


###################################################################################
###################################################################################

#Figure showing bootstrapped distributions vs true



np_dist_3x <- get_distributions(imputed_traits = imputed_full,nrep = 3,sample_size = sample_size_dist)
np_dist_3x <- merge(np_dist_3x,elev)
np_dist_3x$method <- "Non-parametric BS"
np_dist_3x$n <- paste("Replicate",np_dist_3x$n)

atraits_dist <- atraits
colnames(atraits_dist)
atraits_dist$n <-"TRUE"
atraits_dist$global <-"global"
atraits_dist$method <-"True"
atraits_dist<-merge(atraits_dist,elev)
atraits_dist <- atraits_dist[colnames(np_dist_3x)]
np_dist_3x <- rbind(as.data.frame(np_dist_3x),as.data.frame(atraits_dist))
np_dist_3x$mean_elev <-round(np_dist_3x$mean_elev)


ggplot(data = np_dist_3x[which(np_dist_3x$site=="Almont" & np_dist_3x$trait=="leaf_area_mm2"),],aes(x=value,fill=n))+
  geom_density(alpha=0.5)+
  scale_color_manual(group.colors)
np_dist_3x$n



unique(np_dist_3x$n)
