#Load packages and data


###########################################
#install_github("richardjtelford/traitstrap")

library(BIEN)
library(e1071)
library(traitstrap)
library(devtools)
library(tidyr)
library(dplyr)
library(ggplot2)

#source("r_functions/parametric_bs.R")
source("r_functions/draw_traits_tidy.R")
source("r_functions/sim_percent_sampling.R")
source("r_functions/sim_sample_size.R")
source("r_functions/sim_sample_size_abd_biased.R")


#####################################################################################################################

# Colorado data



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
#elevs <- na.omit(unique(traits[c('site',"elev")])) %>%group_by(site) %>%summarise(mean_elev=mean(elev))
#elevs <- elevs[which(elevs$site %in% atraits$site),]
#saveRDS(object = elevs,file = "data/elevations.RDS")


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

  #log transform
  #ggplot(data = atraits,mapping = aes(x=value))+geom_histogram()+facet_wrap(trait~site,scales = "free")
  atraits$value <- log10(atraits$value)
  #ggplot(data = atraits,mapping = aes(x=value))+geom_histogram()+facet_wrap(trait~site,scales = "free")
  
  
  
#Scale and center traits  
  atraits <- atraits %>%
    group_by(trait) %>% 
    mutate(value = as.numeric(scale(value))) %>% 
    ungroup() # %>% group_by(trait) %>% summarize(mean=mean(value),sd=sd(value))

#Get community data
  community <- atraits %>% 
    group_by(taxon,site) %>%
    summarise(across(ID,~(length(unique(.x))),
                     .names = "abundance"),
              .groups="drop")



  
#Run pct cover sims
  co_pct_sims <- sim_percent_sampling(traits = atraits,
                                      community = community,
                                      nreps = 10,
                                      nsamples = 9,
                                      n_reps_boot = 200)

#save output
  saveRDS(object = co_pct_sims,file = "output_data/Colorado_percent_community_sims.RDS")  
  rm(co_pct_sims)
  

#Run sample size sims
  output_co <- sim_sample_size(tidy_traits = atraits,
                               community = community,
                               n_to_sample = 
                                 (1:ceiling(x = max(community$abundance)^.5))^2,
                               n_reps_trait = 10,
                               n_reps_boot = 200,
                               seed = 2005,
                               prob = NULL)
  #save output
    saveRDS(object = output_co,file = "output_data/simulation_results.RDS")

#Run sample size sims with bias
  output_co_biased <-
    sim_sample_size(tidy_traits = atraits,
                    community = community,
                    prob = 0.75,
                    n_to_sample =
                      (1:ceiling(x = max(community$abundance)^.5))^2,
                    n_reps_trait = 10,
                    n_reps_boot = 200,
                    seed = 2005)
  #save output
  saveRDS(object = output_co_biased,
          file = "output_data/simulation_results_biased.RDS")


#Figure out maximum number of individuals per site  
community %>%
  group_by(site) %>%
  summarise(total_abd = sum(abundance)) %>%
  summarize(max(total_abd))


    
  
#Run abundance_biased sampling  
  # Note: the bias needs to be low enough, and the sample size high enough, that bootstrapping of mean traits is unlikely 
  # to produce a sample of identical values (which throws errors due to NAs in higher moments)
  
  output_co_abd_common_bias <- 
    sim_sample_size_abundance_biased(tidy_traits = atraits,
                                     community = community,
                                     n_to_sample = (6:ceiling(x = 917^.5))^2,
                                     n_reps_trait = 10,
                                     n_reps_boot = 200,
                                     seed = 2005,
                                     distribution_type = "normal",
                                     min_n_in_sample = 1,
                                     abd_bias = "common",
                                     weight_exponent = 1.5)
  
  output_co_abd_rare_bias <- 
    sim_sample_size_abundance_biased(tidy_traits = atraits,
                                     community = community,
                                     n_to_sample = (6:ceiling(x = 917^.5))^2,
                                     n_reps_trait = 10,
                                     n_reps_boot = 200,
                                     seed = 2005,
                                     distribution_type = "normal",
                                     min_n_in_sample = 1,
                                     abd_bias = "rare",
                                     weight_exponent = 1.5)
  
  
  output_co_abd_no_bias <- 
    sim_sample_size_abundance_biased(tidy_traits = atraits,
                                     community = community,
                                     n_to_sample = (6:ceiling(x = 917^.5))^2,
                                     n_reps_trait = 10,
                                     n_reps_boot = 200,
                                     seed = 2005,
                                     distribution_type = "normal",
                                     min_n_in_sample = 1,
                                     abd_bias = "random",
                                     weight_exponent = 1.5)  
    

  #add code here to combine abundance outputs into a single dataframe
      #- add a column for bias type
  
  output_co_abundunce_bias <-
  rbind(data.frame(sampling_bias = "rare",output_co_abd_rare_bias),
        data.frame(sampling_bias = "none",output_co_abd_no_bias),
        data.frame(sampling_bias = "common",output_co_abd_common_bias))
  
  
  #save output
  saveRDS(object = output_co_abundunce_bias,
          file = "output_data/simulation_results_abundance_biased.RDS")
  
  
  
#############################################################################

# Messier Panama Data

library(xlsx)
panama <- read.xlsx(file = "data/Julies_Panama_data.xlsx", 1)

#Rename a few columns so the data plays well with the simulation code
colnames(panama)[which(colnames(panama)=="Site")] <- "region"
colnames(panama)[which(colnames(panama)=="Plot")] <- "site"
colnames(panama)[which(colnames(panama)=="Species")] <- "taxon"
colnames(panama)[which(colnames(panama)=="Id")] <- "ID"

#Convert to tidy/skinny/long form
panama_traits <- gather(data=panama, key = "trait", "value", 12:20)
panama_traits$value <- as.numeric(panama_traits$value)

#which traits have the most NAs?  Toss those and then any rows with NA values
panama_traits %>%
  group_by(trait) %>%
  summarise(across(value,~length(which(is.na(.x)))))
panama_traits <- panama_traits[which(panama_traits$trait!="SPAD.average"),]
panama_traits <- panama_traits[which(!is.na(panama_traits$value)),]

#check scales, log tf if necessary
ggplot(data = panama_traits,aes(x=value))+
  geom_histogram()+facet_wrap(~trait,scales = "free")

ggplot(data = panama_traits,aes(x=log(value)))+
  geom_histogram()+facet_wrap(~trait,scales = "free")

panama_traits %>% group_by(trait) %>% 
  summarise(ks_untf = ks.test(x = .data$value,
                              y = "pnorm")$statistic,
            ks_tf =ks.test(x = log10(.data$value),
                             y = "pnorm")$statistic,
            sw_untf = shapiro.test(x = .data$value)$statistic,
            sw_tf =shapiro.test(x = log10(.data$value))$statistic)

#Log tfing seems to substantially improve some, and make relatively minor changes to others,
#so I'll log tf everything
panama_traits$value <- log10(panama_traits$value)

#Scale and center traits  
panama_traits <- panama_traits %>%
  group_by(trait) %>% 
  mutate(value = as.numeric(scale(value))) %>% 
  ungroup() 
  # %>% group_by(trait) %>% summarize(mean = mean(value), sd = sd(value))


#Convert to trait vs community
panama_community <- panama_traits %>%
  group_by(region,site,taxon) %>%
  summarise(across(Tree,~(length(unique(.x))),
                   .names = "abundance"),
            .groups="drop")

#Since N individuals != N samples, create a separate file containing sample sizes
panama_samples <- panama_traits %>%
  group_by(region,site,taxon) %>%
  summarise(across(ID,~(length(unique(.x))),
                   .names = "abundance"),
            .groups="drop")

#Run pct cover sims
  panama_pct_sims <- sim_percent_sampling(traits = panama_traits,
                                          community = panama_community,
                                          nreps = 10,
                                          nsamples = 9,
                                          n_reps_boot = 200)
  
  saveRDS(object = panama_pct_sims,
          file = "output_data/Panama_percent_community_sims.RDS")  

#Run sample size sims
  output_pa <-
    sim_sample_size(tidy_traits = panama_traits,
                    n_to_sample =
                      (1:ceiling(x = max(panama_samples$abundance)^.5))^2,
                    community = panama_community,
                    n_reps_trait = 10,
                    n_reps_boot = 200,
                    seed = 2005)
  
  saveRDS(object = output_pa,
          file = "output_data/panama_simulation_results.RDS")
###############################################################################

#Portal rodents data
  
library(portalr)

portal_data <- load_rodent_data("repo")

portal_species <- portal_data$species_table
portal_traits <- portal_data$rodent_data

portal_species <- portal_species[c("species","scientificname","taxa")]
portal_traits <- merge(x = portal_traits,y = portal_species,all.x = T)

#Toss non-rodents
portal_traits <- portal_traits[which(portal_traits$taxa=="Rodent"),]

#Toss rows with NA for wgt
portal_traits <- portal_traits[which(!is.na(portal_traits$wgt)),]

#Cut down to the year with the most records
portal_traits <-
  portal_traits[which(portal_traits$year ==
                        as.numeric(names(which.max(table(portal_traits$year))))),]

#Log tf weight
portal_traits$wgt <- log10(portal_traits$wgt)

#Reformat to match simulation inputs (site, taxon, id,trait,value)
portal_traits <- portal_traits[c("plot","scientificname","wgt")]
colnames(portal_traits) <- c("site","taxon","value")
portal_traits$trait <- "log10_weight"
portal_traits$ID <- 1:nrow(portal_traits)

portal_traits <- portal_traits %>%
  group_by(trait) %>% 
  mutate(value = as.numeric(scale(value))) %>% 
  ungroup() 
# %>% group_by(trait) %>% summarize(mean = mean(value), sd = sd(value))



#Make community data
portal_community <- portal_traits %>%
  group_by(taxon,site) %>%
  summarise(across(ID,~(length(unique(.x))),
                   .names = "abundance"),
            .groups="drop") 

#Make sure site is a character to keep code happy
portal_traits$site <- as.character(portal_traits$site)
portal_community$site <- as.character(portal_community$site)

#Run sample size sims
output_rodents <-
  sim_sample_size(tidy_traits = portal_traits,
                  community = portal_community,
                  n_to_sample =
                    (1:ceiling(x = max(portal_community$abundance)^.5))^2,
                  n_reps_trait = 10,
                  n_reps_boot = 200,
                  seed = 2005,
                  prob = NULL
                  )
                                  
saveRDS(object = output_rodents,
        file = "output_data/simulation_results_rodents.RDS")


#Run pct cover sims
rodent_pct_sims <- sim_percent_sampling(traits = portal_traits,
                                        community = portal_community,
                                        nreps = 10,
                                        nsamples = 9,
                                        n_reps_boot = 200)


saveRDS(object = rodent_pct_sims,
        file = "output_data/Rodent_percent_community_sims.RDS")

#Run size-biased simulations

output_rodents_biased <-
  sim_sample_size(tidy_traits = as.data.frame(portal_traits),
                  community = as.data.frame(portal_community),
                  prob = 0.75,
                  n_to_sample =
                    (1:ceiling(x = max(portal_community$abundance)^.5))^2,
                  n_reps_trait = 10,
                  n_reps_boot = 200,
                  seed = 2005,
                  distribution_type = "normal",
                  min_n_in_sample = 1,
                  focal_trait = "log10_weight",
                  large_biased = FALSE) #setting large bias = FALSE here reverses sorting order (smaller individuals will be selected)

#save output
saveRDS(object = output_rodents_biased,
        file = "output_data/simulation_results_rodents_biased.RDS")




#Run abundance_biased sampling  

#Figure out maximum number of individuals per site  
portal_community %>%
  group_by(site) %>%
  summarise(total_abd = sum(abundance)) %>%
  summarize(max(total_abd))


# Note: the bias needs to be low enough, and the sample size high enough, that bootstrapping of mean traits is unlikely 
# to produce a sample of identical values (which throws errors due to NAs in higher moments)

output_rodents_abd_common_bias <- 
  sim_sample_size_abundance_biased(tidy_traits = portal_traits,
                                   community = portal_community,
                                   n_to_sample = (2:ceiling(x = 277^.5))^2,
                                   n_reps_trait = 10,
                                   n_reps_boot = 200,
                                   seed = 2005,
                                   distribution_type = "normal",
                                   min_n_in_sample = 1,
                                   abd_bias = "common",
                                   weight_exponent = 1.5)

output_rodents_abd_rare_bias <- 
  sim_sample_size_abundance_biased(tidy_traits = portal_traits,
                                   community = portal_community,
                                   n_to_sample = (2:ceiling(x = 277^.5))^2,
                                   n_reps_trait = 10,
                                   n_reps_boot = 200,
                                   seed = 2005,
                                   distribution_type = "normal",
                                   min_n_in_sample = 1,
                                   abd_bias = "rare",
                                   weight_exponent = 1.5)


output_rodents_abd_no_bias <- 
  sim_sample_size_abundance_biased(tidy_traits = portal_traits,
                                   community = portal_community,
                                   n_to_sample = (2:ceiling(x = 277^.5))^2,
                                   n_reps_trait = 10,
                                   n_reps_boot = 200,
                                   seed = 2005,
                                   distribution_type = "normal",
                                   min_n_in_sample = 1,
                                   abd_bias = "random",
                                   weight_exponent = 1.5)  


#add code here to combine abundance outputs into a single dataframe
#- add a column for bias type

output_rodents_abundunce_bias <-
  rbind(data.frame(sampling_bias = "rare", output_rodents_abd_rare_bias),
        data.frame(sampling_bias = "none", output_rodents_abd_no_bias),
        data.frame(sampling_bias = "common", output_rodents_abd_common_bias))


#save output
saveRDS(object = output_rodents_abundunce_bias,
        file = "output_data/simulation_results_rodents_abundance_biased.RDS")


###############################################################################

#Treefrog tadpoles

#Load data
treefrogs <- read.xlsx(file = "data/pond_critters/TreefrogTadpoles.xlsx",2)
treefrogs <- treefrogs[c("Tank","Body.Length..mm."  )]
treefrog_md <- read.xlsx(file = "data/pond_critters/TreefrogTadpoles.xlsx",1)
treefrog_md <- treefrog_md[,1:4]
treefrogs <- merge(treefrogs,treefrog_md,all.x = T)
rm(treefrog_md)
treatments <- unique(treefrogs[c("Timing","Synchrony")])
treatments$treatment_numeric <- 1:nrow(treatments)
treefrogs <- merge(treefrogs,treatments)
rm(treatments)

ggplot(data = treefrogs,aes(x=Body.Length..mm.))+geom_histogram()+facet_wrap(facets = "treatment_numeric")

ggplot(data = treefrogs,aes(x=log10(Body.Length..mm.)))+geom_histogram()+facet_wrap(facets = "treatment_numeric")

#Reformat to match necessary input for sims

treefrogs <- treefrogs %>% mutate(
                     site = "south_campus",
                     taxon = as.character(treatment_numeric),
                     trait = "body_length_mm",
                     value = log10(Body.Length..mm.),
                     ID = 1:nrow(treefrogs))

treefrogs <- treefrogs[colnames(atraits)]

treefrogs <- treefrogs %>%
  group_by(trait) %>% 
  mutate(value = as.numeric(scale(value))) %>% 
  ungroup() 
# %>% group_by(trait) %>% summarize(mean = mean(value), sd = sd(value))


treefrog_community <- treefrogs %>% 
  group_by(taxon,site) %>%
  summarise(across(ID,~(length(unique(.x))),
                   .names = "abundance"),
            .groups="drop")

treefrog_output <- sim_sample_size(tidy_traits = treefrogs,
                                   community = treefrog_community,
                                   n_to_sample = 
                                     (1:ceiling(x = max(
                                       treefrog_community$abundance)^.5)
                                      )^2,
                                   n_reps_trait = 10,
                                   n_reps_boot = 200,
                                   seed = 2005)


ggplot(data = treefrog_output,
       mapping = aes(y=mean-true_mean,x = sample_size))+
  geom_point()+
  facet_grid(method~site)

ggplot(data = treefrog_output,
       mapping = aes(y=var-true_variance,x = sample_size))+
  geom_point()+
  facet_grid(method~site)

ggplot(data = treefrog_output,
       mapping = aes(y=skew-true_skewness,x = sample_size))+
  geom_point()+
  facet_grid(method~site)

ggplot(data = treefrog_output,
       mapping = aes(y=kurt-true_kurtosis,x = sample_size))+
  geom_point()+
  facet_grid(method~site)


treefrog_output %>% 
  filter(sample_size <= 25) %>% 
  group_by(method) %>% 
  summarise(mean_kurt_dist = mean(kurt - true_kurtosis))
  #seems to give reasonable performance


saveRDS(object = treefrog_output,
        file = "output_data/simulation_results_treefrogs.RDS")

##########################################################################################


#Phytoplankton

#2019
phyto <- read.table(file = "data/plankton/zooplankton_2019.csv",
                    sep = ";",
                    header = T)

#convert area into numeric
phyto$area <- noquote(phyto$area)
phyto$area <- gsub(pattern = ",",
                   replacement = ".",
                   x = phyto$area)
phyto$area <- as.numeric(phyto$area)

#convert aspect ratio into numeric
phyto$aspect_ratio <- noquote(phyto$aspect_ratio)
phyto$aspect_ratio <- gsub(pattern = ",",
                           replacement = ".",
                           x = phyto$aspect_ratio)
phyto$aspect_ratio <- as.numeric(phyto$aspect_ratio)

#convert eccentricity into numeric
phyto$eccentricity <- noquote(phyto$eccentricity)
phyto$eccentricity <- gsub(pattern = ",",
                           replacement = ".",
                           x = phyto$eccentricity)
phyto$eccentricity <- as.numeric(phyto$eccentricity)

#convert solidity into numeric
phyto$solidity <- noquote(phyto$solidity)
phyto$solidity <- gsub(pattern = ",",
                       replacement = ".",
                       x = phyto$solidity)
phyto$solidity <- as.numeric(phyto$solidity)


#convert date and add additonal format options for plotting
phyto$date <-
  unlist(lapply(X = phyto$timestamp,FUN = function(x) {
    strsplit(x = x,split = " ")[[1]][1]
  }
  ))

phyto$date <- as.Date(phyto$date)
phyto$day_of_year <- as.numeric(phyto$date) - as.numeric(as.Date("2019-01-01"))


#taxon = prediction
#day of year = site
#traits 
#size
#area
#shape
#eccentricity,
#solidity


#prune dataset to save space
phyto <- 
  phyto[c("area",
          "aspect_ratio",
          "eccentricity",
          "solidity",
          "prediction",
          "day_of_year",
          "date"
  )]

#Toss anything with NA's
phyto <- na.omit(phyto)

#Toss unassigned taxa

phyto <- 
  phyto[which(!phyto$prediction %in% c("Unclassified","unknown","dirt") ),]

#check for trait correlations
cor(phyto[,1:4])
colnames(phyto)

#toss aspect ratio since it is strongly correlated with eccentricity
phyto <- phyto[which(colnames(phyto) != "aspect_ratio")]


#Reformat to match simulation input expectations (site, taxon, id,trait,value)

  phyto$ID <- 1:nrow(phyto) #Add ID field to link traits when we tidy things up
  phyto$area <- log10(phyto$area)
  
  hist(phyto$area)#good enough - fit with normal
  hist(phyto$eccentricity) #good candidate for beta fit
  hist(phyto$solidity) #good candidate for beta fit
  
  
  phyto <-
  pivot_longer(data = phyto,
               cols = 1:3,
               names_to = "trait",
               values_to = "value")%>% 
    mutate(taxon = prediction,
           site = as.character(day_of_year)) %>%
    select(-prediction)


#Make community data
phyto_community <- phyto %>%
  group_by(taxon,site) %>%
  summarise(across(ID,~(length(unique(.x))),
                   .names = "abundance"),
            .groups="drop")

#Check that abundances match the number of individuals measured
sum(phyto_community$abundance) == length(unique(phyto$ID))


source("r_functions/sim_sample_size.R")

phyto_scaled <-
phyto %>%
  group_by(trait) %>% 
  mutate(value = as.numeric(scale(value))) %>% 
  ungroup() %>% filter(trait == "area")

#Check that abundances match the number of individuals measured
sum(phyto_community$abundance) == length(unique(phyto_scaled$ID))


#Make subsets of the data to make things faster

phyto_scaled_subset <-
phyto_scaled %>% 
  filter(day_of_year %in% unique(phyto$day_of_year)[seq(1, length(unique(phyto$day_of_year)), 7)])

phyto_community_subset <-
phyto_community %>%
  filter(site %in% unique(phyto_scaled_subset$site))

length(unique(phyto_scaled_subset$day_of_year)) == length(unique(phyto_community_subset$site))


#Run sample size simulations that are scaled for the ms and subsetted to be faster
output_phyto_scaled_subset <-
  sim_sample_size(tidy_traits = phyto_scaled_subset,
                  community = phyto_community_subset,
                  n_to_sample =
                    (1:ceiling(x = max(phyto_community_subset$abundance)^.5))^2,
                    #c(1:21,38,55)^2,
                  n_reps_trait = 10,
                  n_reps_boot = 200,
                  seed = 2005,
                  prob = NULL,
                  distribution_type = "normal",
                  min_n_in_sample = 1 )

saveRDS(object = output_phyto_scaled_subset,
        file = "output_data/simulation_results_phyto_subset_scaled.RDS")

colnames(phyto_scaled_subset)

#calculate mean number of individuals per day
phyto_scaled_subset %>% 
  group_by(day_of_year)%>%
  summarise(n=n())%>%colMeans()


#Run sample size simulations to show it can be done
# output_phyto_unscaled <-
#   sim_sample_size(tidy_traits = phyto,
#                   community = phyto_community,
#                   n_to_sample =
#                     (2:ceiling(x = max(phyto_community$abundance)^.5))^2,
#                   n_reps_trait = 10,
#                   n_reps_boot = 200,
#                   seed = 2005,
#                   prob = NULL,
#                   distribution_type = list(area = "normal",
#                                            eccentricity = "beta",
#                                            solidity = "beta"),
#                   min_n_in_sample = 2 #need to set this to 2 for beta fitting
#   )
# 
# saveRDS(object = output_phyto_unscaled,
#         file = "output_data/unscaled_simulation_results_phyto.RDS")


#Run sample size simulations that are scaled for the ms
#get output distributions for Tanya

plankton_sample <-
draw_traits_tidy(tidy_traits = phyto_scaled,
                 sample_size = 9)


imputed_plankton <-
trait_impute(comm = phyto_community,
             traits = plankton_sample,
             scale_hierarchy = "site",
             global = TRUE,
             taxon_col = "taxon",
             trait_col = "trait",
             value_col = "value",
             abundance_col = "abundance",
             min_n_in_sample = 1)

plankton_dist <-
trait_np_bootstrap(imputed_traits = imputed_plankton,
                   nrep = 1,
                   sample_size = 200,
                   raw = TRUE)

saveRDS(object = plankton_dist,
         file = "output_data/raw_plankton_distributions.RDS")

###############################################################################

# Phytoplankton with multiple traits
    
    
    #2019
    phyto <- read.table(file = "data/plankton/zooplankton_2019.csv",
                        sep = ";",
                        header = T)
    
    #convert area into numeric
    phyto$area <- noquote(phyto$area)
    phyto$area <- gsub(pattern = ",",
                       replacement = ".",
                       x = phyto$area)
    phyto$area <- as.numeric(phyto$area)
    
    #convert aspect ratio into numeric
    phyto$aspect_ratio <- noquote(phyto$aspect_ratio)
    phyto$aspect_ratio <- gsub(pattern = ",",
                               replacement = ".",
                               x = phyto$aspect_ratio)
    phyto$aspect_ratio <- as.numeric(phyto$aspect_ratio)
    
    #convert eccentricity into numeric
    phyto$eccentricity <- noquote(phyto$eccentricity)
    phyto$eccentricity <- gsub(pattern = ",",
                               replacement = ".",
                               x = phyto$eccentricity)
    phyto$eccentricity <- as.numeric(phyto$eccentricity)
    
    #convert solidity into numeric
    phyto$solidity <- noquote(phyto$solidity)
    phyto$solidity <- gsub(pattern = ",",
                           replacement = ".",
                           x = phyto$solidity)
    phyto$solidity <- as.numeric(phyto$solidity)
    
    
    #convert date and add additonal format options for plotting
    phyto$date <-
      unlist(lapply(X = phyto$timestamp,FUN = function(x) {
        strsplit(x = x,split = " ")[[1]][1]
      }
      ))
    
    phyto$date <- as.Date(phyto$date)
    phyto$day_of_year <- as.numeric(phyto$date) - as.numeric(as.Date("2019-01-01"))
    
    
    #taxon = prediction
    #day of year = site
    #traits 
    #size
    #area
    #shape
    #eccentricity,
    #solidity
    
    
    #prune dataset to save space
    phyto <- 
      phyto[c("area",
              "aspect_ratio",
              "eccentricity",
              "solidity",
              "prediction",
              "day_of_year",
              "date"
      )]
    
    #Toss anything with NA's
    phyto <- na.omit(phyto)
    
    #Toss unassigned taxa
    
    phyto <- 
      phyto[which(!phyto$prediction %in% c("Unclassified","unknown","dirt") ),]
    
    #check for trait correlations
    cor(phyto[,1:4])
    colnames(phyto)
    
    #toss aspect ratio since it is strongly correlated with eccentricity
    phyto <- phyto[which(colnames(phyto) != "aspect_ratio")]
    
    
    #Reformat to match simulation input expectations (site, taxon, id,trait,value)
    
    phyto$ID <- 1:nrow(phyto) #Add ID field to link traits when we tidy things up
    phyto$area <- log10(phyto$area)
    
    phyto <-
      pivot_longer(data = phyto,
                   cols = 1:3,
                   names_to = "trait",
                   values_to = "value")%>% 
      mutate(taxon = prediction,
             site = as.character(day_of_year)) %>%
      select(-prediction)
    
    
  #Make community data
    phyto_community <- phyto %>%
      group_by(taxon,site) %>%
      summarise(across(ID,~(length(unique(.x))),
                       .names = "abundance"),
                .groups="drop")
    
    #Check that abundances match the number of individuals measured
    sum(phyto_community$abundance) == length(unique(phyto$ID))
    
    
  source("r_functions/sim_sample_size.R")
    
    phyto_scaled <-
      phyto %>%
      group_by(trait) %>% 
      mutate(value = as.numeric(scale(value))) %>% 
      ungroup()
    
    #Check that abundances match the number of individuals measured
    sum(phyto_community$abundance) == length(unique(phyto_scaled$ID))
    
    
    #Make subsets of the data to make things faster
    
    phyto_scaled_subset <-
      phyto_scaled %>% 
      filter(day_of_year %in% unique(phyto$day_of_year)[seq(1, length(unique(phyto$day_of_year)), 7)])
    
    phyto_community_subset <-
      phyto_community %>%
      filter(site %in% unique(phyto_scaled_subset$site))
    
    length(unique(phyto_scaled_subset$day_of_year)) == length(unique(phyto_community_subset$site))


    plankton_sample <-
      draw_traits_tidy(tidy_traits = phyto_scaled_subset,
                       sample_size = 9)


###############################################################################

#Traitstrap CWM vs traditional CWM

#Convert all of the trait to means
colorado_means <- samples_to_means(tidy_traits = atraits,
                                 level = "taxon")

colorado_means$site <- "ALL" #Note: adding a "dummy" site here so traitstrap will use global data but still return site-level moments

imputed_species_mean <- 
  trait_impute(comm = community,
               traits = colorado_means,
               scale_hierarchy = "site",
               global = T,
               taxon_col = "taxon",
               trait_col = "trait",
               value_col = "value",
               abundance_col = "abundance",
               min_n_leaves = 1)

library(Weighted.Desc.Stat)
source("r_functions/weighted.moments.R")

cwm_estimates <- community_weighted_moments(imputed_traits = imputed_species_mean)
bootstrap_cwm <- trait_np_bootstrap(imputed_traits = imputed_species_mean,
                                    nrep = 200,
                                    sample_size = 200)

bootstrap_cwm <- trait_summarise_boot_moments(bootstrap_moments = bootstrap_cwm)

cwm_estimates <- cwm_estimates[c("site","trait","mean","variance","skewness","kurtosis")]
cwm_estimates <- cwm_estimates %>% 
  pivot_longer(cols = c(mean,variance,skewness,kurtosis),
               names_to = "moment",
               values_to = "traditional_CWM")

bootstrap_cwm <- bootstrap_cwm %>% ungroup() %>% 
  mutate(variance = var, skewness = skew, kurtosis = kurt, method = "bootstrap CWM")

bootstrap_cwm <- bootstrap_cwm %>% 
  pivot_longer(cols = c(mean,variance,skewness,kurtosis),
               names_to = "moment",
               values_to = "bootstrap_CWM")

cwm_comparison <- merge(cwm_estimates,bootstrap_cwm)

cwm_comparison <- cwm_comparison %>% 
  select(c(site,trait,moment,traditional_CWM, bootstrap_CWM))

ggplot(data = cwm_comparison,aes(x= traditional_CWM,y=bootstrap_CWM))+
  geom_point()+
  facet_wrap(moment ~ trait, scales = "free")+
  geom_abline(slope = 1,intercept = 0)

cor.test(cwm_comparison$traditional_CWM,cwm_comparison$bootstrap_CWM)

cwm_comparison %>% group_by(moment) %>% 
  summarise(correlation =
              cor.test(.data$traditional_CWM, .data$bootstrap_CWM)$estimate,
            pval = cor.test(.data$traditional_CWM, .data$bootstrap_CWM)$p.value)



saveRDS(object = cwm_comparison, file = "output_data/CWM_methods_comparison.RDS")


###############################################################################

#Bootstrap sample size and method

#Sample size (of bootstrap sample) vs method and moment

#Should also incorporate the Older version of parametric bs

source("r_functions/sim_boot_size.R")

boot_sample_output <-
  sim_boot_size(tidy_traits = atraits,
                community = community,
                n_to_sample = (1:22)^2,
                n_reps_trait = 10,
                n_reps_boot = 200,
                boot_sample_size = c(50, 100, 200, 400, 800, 1600, 3200),
                seed = 2005)

saveRDS(object = boot_sample_output,
        file = "output_data/bootstrap_sample_size_and_method_sims.RDS")

boot_sample_output <-
  readRDS("output_data/bootstrap_sample_size_and_method_sims.RDS")

############################################################################


#Multidimensional

source("r_functions/sim_sample_size_multidimensional.R")

#we'll plot bootstrapped vs true


#Run sample size sims
  md_output_co <- sim_sample_size_multidimensional(tidy_traits = atraits,
                               community = community,
                               n_to_sample = 
                                 (1:ceiling(x = max(community$abundance)^.5))^2,
                                 #(1:10)^2, #truncating to make things faster
                               n_reps_trait = 10,
                               n_reps_boot = 40, #using 40 rather than 200 because its slow
                               seed = 2005,
                               prob = NULL,
                               FRic = FALSE, #throws errors at low sampling.
                               FDiv = FALSE,
                               tempoutRDS = "output_data/temp_multidimensional.RDS",
                               continue = TRUE)
  
# 
#   saveRDS(object = md_output_co,
#           file = "output_data/multidimensional_bootstrap_sample_size_and_method_sims.RDS")
  
  md_output_co <-
    readRDS("output_data/multidimensional_bootstrap_sample_size_and_method_sims.RDS")

ggplot(data = md_output_co)+
  geom_point(mapping = aes(x=sample_size, y = FEve,col=site))+
  geom_errorbar(mapping = aes(x= sample_size,
                              ymin = ci_low_FEve,
                              ymax = ci_high_FEve,
                              col = site))+
  geom_hline(mapping = aes(yintercept=true_FEve,col=site))+
  facet_wrap(~method)

ggplot(data = md_output_co)+
  geom_point(mapping = aes(x=sample_size, y = FDis,col=site))+
  geom_hline(mapping = aes(yintercept=true_FDis, col = site))+
  facet_wrap(~method)

colnames(md_output_co)

ggplot(data = md_output_co)+
  geom_point(mapping = aes(x=sample_size, y = RaoQ,col=site))+
  geom_hline(mapping = aes(yintercept=true_RaoQ, col = site))+
  facet_wrap(~method)


ggplot(data = md_output_co)+
  geom_point(mapping = aes(x=sample_size, y = FEve, col=site))+
  geom_errorbar(mapping = aes(x= sample_size,
                              ymin = ci_low_FEve,
                              ymax = ci_high_FEve,
                              col = site))+
  geom_hline(mapping = aes(yintercept=true_FEve,col=site))+
  facet_wrap(~method)



########################################

#multidimensional rarity

source("r_functions/sim_sample_size_multidimensional_rarity.R")


#Run sample size sims
md_output_rarity_co <- sim_sample_size_multidimensional_rarity(tidy_traits = atraits,
                                                 community = community,
                                                 n_to_sample = 
                                                  #(1:ceiling(x = max(community$abundance)^.5))^2,
                                                 (1:10)^2, #truncating to make things faster
                                                 n_reps_trait = 10,
                                                 n_reps_boot = 40, #using 40 rather than 200 because its slow
                                                 seed = 2005,
                                                 prob = NULL,
                                                 tempoutRDS = "output_data/temp_multidimensional_rarity.RDS")


# saveRDS(object = md_output_rarity_co,
#         file = "output_data/multidimensional_bootstrap_sample_size_and_method_rarity_sims.RDS")
# 
md_output_rarity_co <-
  readRDS("output_data/multidimensional_bootstrap_sample_size_and_method_rarity_sims.RDS")


md_output_rarity_co$true_mean_distinctiveness

ggplot(data = md_output_rarity_co)+
  geom_point(mapping = aes(x = sample_size,
                           y = avg_uniqueness,
                           col = site))+
  geom_errorbar(mapping = aes(x = sample_size,
                              ymin = ci_low_avg_uniqueness,
                              ymax = ci_high_avg_uniqueness,
                              col = site))+
  geom_hline(mapping = aes(yintercept = true_mean_uniqueness,
                           col = site))+
  facet_wrap(~method)



ggplot(data = md_output_rarity_co)+
  geom_point(mapping = aes(x = sample_size,
                           y = avg_distinctiveness,
                           col = site))+
  geom_errorbar(mapping = aes(x = sample_size,
                              ymin = ci_low_avg_distinctiveness,
                              ymax = ci_high_avg_distinctiveness,
                              col = site))+
  geom_hline(mapping = aes(yintercept = true_mean_distinctiveness,
                           col = site))+
  facet_wrap(~method)



ggplot(data = md_output_rarity_co)+
  geom_point(mapping = aes(x = sample_size,
                           y = avg_uniqueness,
                           col = site))+
  geom_errorbar(mapping = aes(x = sample_size,
                              ymin = ci_low_avg_uniqueness,
                              ymax = ci_high_avg_uniqueness,
                              col = site))+
  geom_hline(mapping = aes(yintercept = true_mean_uniqueness,
                           col = site))+
  facet_wrap(~method)





