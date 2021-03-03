#Load packages and data


###########################################
library(BIEN)
library(moments)
library(traitstrap)
library(devtools)
library(tidyr)
library(dplyr)
library(ggplot2)

#source("r_functions/parametric_bs.R")
source("r_functions/draw_traits_tidy.R")
source("r_functions/sim_percent_sampling.R")
source("r_functions/sim_sample_size.R")


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

#Get community data
  atraits %>% group_by(taxon,site) %>% summarise(across(ID,~(length(unique(.x))),.names = "abundance"),.groups="drop") -> community

#log transform
    #ggplot(data = atraits,mapping = aes(x=value))+geom_histogram()+facet_wrap(trait~site,scales = "free")
  atraits$value <- log10(atraits$value)
    #ggplot(data = atraits,mapping = aes(x=value))+geom_histogram()+facet_wrap(trait~site,scales = "free")


  
#Run pct cover sims
  co_pct_sims <- sim_percent_sampling(traits = atraits,
                                      community = community,
                                      nreps = 10,
                                      nsamples = 10,
                                      n_reps_boot = 200)

#save output
  saveRDS(object = co_pct_sims,file = "output_data/Colorado_percent_community_sims.RDS")  
  rm(co_pct_sims)
  

#Run sample size sims
  output_co <- sim_sample_size(tidy_traits = atraits)
  #save output
    saveRDS(object = output_co,file = "output_data/simulation_results.RDS")

#Run sample size sims with bias
  output_co_biased <- sim_sample_size(tidy_traits = atraits,
                                      prob = 0.75)
  #save output
  saveRDS(object = output_co_biased,file = "output_data/simulation_results_biased.RDS")
  

#############################################################################

# Messier Panama Data

library(xlsx)
panama <- read.xlsx(file = "data/Julies_Panama_data.xlsx",1)

#Rename a few columns so the data plays well with the simulation code
colnames(panama)[which(colnames(panama)=="Site")]<-"region"
colnames(panama)[which(colnames(panama)=="Plot")]<-"site"
colnames(panama)[which(colnames(panama)=="Species")]<-"taxon"
colnames(panama)[which(colnames(panama)=="Id")]<-"ID"

#Convert to tidy/skinny/long form
panama_traits <- gather(data=panama,key = "trait","value",12:20)
panama_traits$value <- as.numeric(panama_traits$value)

#which traits have the most NAs?  Toss those and then any rows with NA values
panama_traits %>% group_by(trait) %>% summarise(across(value,~length(which(is.na(.x)))))
panama_traits <- panama_traits[which(panama_traits$trait!="SPAD.average"),]
panama_traits <- panama_traits[which(!is.na(panama_traits$value)),]

#check scales, log tf if necessary
ggplot(data = panama_traits,aes(x=value))+geom_histogram()+facet_wrap(~trait,scales = "free")
ggplot(data = panama_traits,aes(x=log(value)))+geom_histogram()+facet_wrap(~trait,scales = "free")

#Log tfing improves everything except LDMC and LLC so I'll toss those rather than dealing with confusing axis options and captions
panama_traits <- panama_traits[which(!panama_traits$trait %in% c("LCC","LDMC")),]
panama_traits$value <- log10(panama_traits$value)

#Convert to trait vs community
panama_traits %>% group_by(region,site,taxon) %>%
  summarise(across(ID,~(length(unique(.x))),.names = "abundance"),.groups="drop") -> panama_community



#Run pct cover sims
  panama_pct_sims <- sim_percent_sampling(traits = panama_traits,
                                          community = panama_community,
                                          nreps = 10,
                                          nsamples = 10,
                                          n_reps_boot = 200)
  
  saveRDS(object = panama_pct_sims,file = "output_data/Panama_percent_community_sims.RDS")  

#Run sample size sims
  output_pa <- sim_sample_size(tidy_traits = panama_traits,n_to_sample = (1:16)^2)
  saveRDS(object = output_pa,file = "output_data/panama_simulation_results.RDS")
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
portal_traits <- portal_traits[which(portal_traits$year==as.numeric(names(which.max(table(portal_traits$year))))),]

#Log tf weight
portal_traits$wgt <- log10(portal_traits$wgt)

#Reformat to match simulation inputs (site, taxon, id,trait,value)
portal_traits <- portal_traits[c("plot","scientificname","wgt")]
colnames(portal_traits) <- c("site","taxon","value")
portal_traits$trait <- "log10_weight"
portal_traits$ID <- 1:nrow(portal_traits)


#Make community data
portal_traits %>% group_by(taxon,site) %>% summarise(across(ID,~(length(unique(.x))),.names = "abundance"),.groups="drop") -> portal_community

#Make sure site is a character to keep code happy
portal_traits$site <- as.character(portal_traits$site)
portal_community$site <- as.character(portal_community$site)

#Run sample size sims
output_rodents <- sim_sample_size(tidy_traits = panama_traits,
                                  n_to_sample = (1:13)^2)
saveRDS(object = output_rodents,file = "output_data/simulation_results_rodents.RDS")



###############################################################################
#Global vs local should be a separate simulation 

  # Using "best guess" means
  # Using all data (gives wider CIs)


#Get global data
traits <- unique(atraits$trait)
taxa <- unique(atraits$taxon)

BIEN_trait_list()

#needed:  
#"leaf_area_mm2"   
#"dry_mass_mg"     
#"height"          
#"biomass_per_ind" 
#"LMA_mg_mm2"     

BIEN_trait_means <- lapply(X = c("leaf area","leaf dry mass","whole plant height","leaf area per leaf dry mass"),FUN = function(x){BIEN_trait_mean(species = taxa,trait = x)})
BIEN_trait_means <- do.call(rbind,BIEN_trait_means) 
table(BIEN_trait_means$level_used)#Very little data at species level

#Need to mak